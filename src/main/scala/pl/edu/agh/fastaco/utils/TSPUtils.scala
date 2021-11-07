package pl.edu.agh.fastaco.utils

object TSPUtils {
  import Utils._

  def totalCost(distance: (Int, Int) => Double, path: Array[Int]): Double = {
    0.sum(path.length - 1) { i =>
      distance(path(i), path(i + 1))
    } + distance(path(path.length - 1), path(0))
  }
}
