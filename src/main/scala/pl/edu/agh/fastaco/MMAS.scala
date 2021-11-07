package pl.edu.agh.fastaco

import com.typesafe.scalalogging.StrictLogging
import pl.edu.agh.fastaco.Launcher.logger
import pl.edu.agh.fastaco.utils.{AlgorithmParams, IdxWithValue, ProblemData, TSPUtils, TwoOpt, WeightedRandom}

import scala.collection.mutable
import scala.util.Random

class MMAS(algParams: AlgorithmParams, problem: ProblemData) extends StrictLogging {
  import pl.edu.agh.fastaco.utils.Utils._

  private val cityIdxInNeighboursList: Array[Array[Int]] = Array.ofDim[Int](algParams.problemDimension, algParams.problemDimension)
  private val neighbours: Array[Array[Int]] = Array.ofDim[Int](algParams.problemDimension, algParams.problemDimension)
  private val distanceAttractiveness: Array[Array[Double]] = Array.ofDim[Double](algParams.problemDimension, algParams.neighbourhoodLimit)
  private val pheromones: Array[Array[Double]] = Array.ofDim[Double](algParams.problemDimension, algParams.neighbourhoodLimit)

  private val twoOpt = new TwoOpt(algParams, neighbours, problem.distance)

  def initData(): Unit = {
    0.loop(algParams.problemDimension) { i =>
      val queue = mutable.PriorityQueue.empty[IdxWithValue]
      0.loop(algParams.problemDimension) { j =>
        if (i != j) {
          queue.enqueue(new IdxWithValue(j, problem.distance(i, j)))
        }
      }

      (problem.dimension - 2).loop(0, step = -1, inclusive = true) { j =>
        val v = queue.dequeue()
        neighbours(i)(j) = v.cityIdx
        cityIdxInNeighboursList(i)(v.cityIdx) = j
        if (j < algParams.neighbourhoodLimit) {
          distanceAttractiveness(i)(j) = math.pow(1.0 / v.value, algParams.desirabilityPower)
          pheromones(i)(j) = algParams.initPheromones
        }
      }
    }
  }

  def run(): Double = {
    val visited = Array.ofDim[Boolean](algParams.problemDimension)
    val nextMoveProbability = Array.ofDim[Double](algParams.neighbourhoodLimit)
    val totalAttractiveness = Array.ofDim[Double](algParams.problemDimension, algParams.neighbourhoodLimit)
    val paths = Array.ofDim[Int](algParams.antsCount, algParams.problemDimension)
    val globalBestPath = Array.ofDim[Int](algParams.problemDimension)

    (1 to algParams.iterations).foldLeft(Double.PositiveInfinity) { (globalBestCost, it) =>
      runIteration(it, visited, nextMoveProbability, paths, globalBestCost, globalBestPath, totalAttractiveness)
    }
  }

  private def runIteration(
    it: Int, visited: Array[Boolean], nextMoveProbability: Array[Double], paths: Array[Array[Int]],
    currentGlobalBestCost: Double, globalBestPath: Array[Int], totalAttractiveness: Array[Array[Double]],
  ): Double = {
    0.loop(algParams.problemDimension) { i =>
      visited(i) = false

      0.loop(algParams.neighbourhoodLimit) { j =>
        totalAttractiveness(i)(j) = {
          val pheromoneValue = math.pow(pheromones(i)(j), algParams.pheromonePower)
          pheromoneValue * distanceAttractiveness(i)(j)
        }
      }
    }

    0.loop(algParams.antsCount) { antId =>
      runAnt(paths(antId), visited, nextMoveProbability, totalAttractiveness)

      // clean visited array before next ant
      0.loop(algParams.problemDimension) { idx => visited(idx) = false }
    }

    val newGlobalBestCost = updatePheromones(it, paths, currentGlobalBestCost, globalBestPath)

    logger.info(s"Iteration $it (Global best: $newGlobalBestCost)")

    newGlobalBestCost
  }

  private def runAnt(
    path: Array[Int], visited: Array[Boolean],
    nextMoveProbability: Array[Double], totalAttractiveness: Array[Array[Double]],
  ): Unit = {
    var lastNode = Random.nextInt(algParams.problemDimension)
    var step = 0

    path(step) = lastNode
    visited(lastNode) = true
    step += 1

    while (step < algParams.problemDimension) {
      // copy attractiveness values of lastNode neighbours
      Array.copy(totalAttractiveness(lastNode), 0, nextMoveProbability, 0, algParams.neighbourhoodLimit)

      lastNode = WeightedRandom.selectNextMove(nextMoveProbability, neighbours(lastNode), visited, algParams.neighbourhoodLimit)

      path(step) = lastNode
      visited(lastNode) = true

      step += 1
    }

    twoOpt.twoOpt(path)
  }

  private def updatePheromones(it: Int, paths: Array[Array[Int]], currentGlobalBestCost: Double, globalBestPath: Array[Int]): Double = {
    var globalBestCost = currentGlobalBestCost

    // evaporation
    0.loop(algParams.problemDimension) { i =>
      0.loop(algParams.neighbourhoodLimit) { j =>
        pheromones(i)(j) = pheromones(i)(j) * (1 - algParams.evaporationCoefficient)
      }
    }

    // iteration best solution
    val (iterationBestAnt, iterationBestCost) = (for (ant <- (0 until algParams.antsCount).iterator) yield {
      val distanceSum = TSPUtils.totalCost(problem.distance, paths(ant))
      (ant, distanceSum)
    }).minBy(_._2)

    // global best update
    if (globalBestCost > iterationBestCost) {
      globalBestCost = iterationBestCost
      Array.copy(paths(iterationBestAnt), 0, globalBestPath, 0, algParams.problemDimension)
    }

    { // best path reinforcement
      val globalBestFactor = {
        if (it < 25) 25
        else if (it < 75) 5
        else if (it < 125) 3
        else if (it < 250) 2
        else 1
      }
      val updatePath = if (it % globalBestFactor == 0) globalBestPath else paths(iterationBestAnt)
      val updateCost = if (it % globalBestFactor == 0) globalBestCost else iterationBestCost
      0.loop(algParams.problemDimension) { i =>
        val j = (i + 1) % algParams.problemDimension
        val from = updatePath(i)
        val to = updatePath(j)
        val idx = cityIdxInNeighboursList(from)(to)
        if (idx < algParams.neighbourhoodLimit) {
          pheromones(from)(idx) += algParams.pheromoneUpdateUnit * globalBestCost / updateCost
          if (pheromones(from)(idx) > algParams.maxPheromones) pheromones(from)(idx) = algParams.maxPheromones
          else if (pheromones(from)(idx) < algParams.minPheromones) pheromones(from)(idx) = algParams.minPheromones

          // pheromones in TSP should be symmetric
          val sIdx = cityIdxInNeighboursList(to)(from)
          if (sIdx < algParams.neighbourhoodLimit) {
            pheromones(to)(sIdx) = pheromones(from)(idx)
          }
        }
      }
    }

    globalBestCost
  }
}
