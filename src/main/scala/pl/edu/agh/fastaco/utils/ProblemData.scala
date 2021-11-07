package pl.edu.agh.fastaco.utils

import java.io.File

import org.moeaframework.problem.tsplib.TSPInstance

class ProblemData(val dimension: Int, private val distances: Array[Array[Double]]) extends Serializable {
  def distance(i: Int, j: Int): Double = {
    distances(i)(j)
  }
}

object ProblemData {
  def problemDimension(file: String): Int = {
    val problem: TSPInstance = {
      val resource = ProblemData.getClass.getClassLoader.getResource(file)
      new TSPInstance(new File(resource.toURI))
    }

    problem.getDimension
  }

  def fromTspFile(file: String): ProblemData = {
    import Utils._

    val problem: TSPInstance = {
      val resource = ProblemData.getClass.getClassLoader.getResource(file)
      new TSPInstance(new File(resource.toURI))
    }

    val dimension: Int = problem.getDimension

    val distances: Array[Array[Double]] = {
      val d = Array.ofDim[Double](dimension, dimension)
      0.loop(dimension) { i =>
        0.loop(dimension) { j =>
          d(i)(j) = problem.getDistanceTable.getDistanceBetween(i + 1, j + 1)
        }
      }
      d
    }

    new ProblemData(dimension, distances)
  }
}
