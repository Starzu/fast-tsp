package pl.edu.agh.fastaco.utils

import scala.util.Random

object WeightedRandom {
  import Utils._

  /** @note It modifies `nextMoveProbability`! */
  def selectNextMove(
    nextMoveProbability: Array[Double], neighbours: Array[Int], visited: Array[Boolean], neighbourhoodLimit: Int
  ): Int = {
    val weightsSum = 0.sum(neighbourhoodLimit) { idx =>
      if (visited(neighbours(idx))) {
        // node was already visited, so probability should be 0
        nextMoveProbability(idx) = 0.0
      }
      nextMoveProbability(idx)
    }

    if (weightsSum > 0) {
      val rand = Random.nextDouble() * weightsSum
      var idx = 0
      var partialSum = 0.0
      partialSum += nextMoveProbability(idx)

      while (partialSum < rand && idx < neighbourhoodLimit - 1) {
        idx += 1
        partialSum += nextMoveProbability(idx)
      }

      if (partialSum >= rand) neighbours(idx)
      else selectClosestNotVisited(neighbours, visited)
    } else {
      selectClosestNotVisited(neighbours, visited)
    }
  }

  private def selectClosestNotVisited(neighbours: Array[Int], visited: Array[Boolean]): Int = {
    var idx = 0
    while (visited(neighbours(idx))) idx += 1
    neighbours(idx)
  }
}
