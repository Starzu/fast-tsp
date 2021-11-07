package pl.edu.agh.fastaco.utils

import scala.util.Random

object Utils {
  def shuffleArray(a: Array[Int]): Unit = {
    for (i <- a.indices) {
      val node = (Random.nextDouble() * (a.length - i)).toInt
      val help = a(i)
      a(i) = a(i + node)
      a(i + node) = help
    }
  }

  implicit class LoopInt(val from: Int) extends AnyVal {
    def loop(to: Int, step: Int = 1, inclusive: Boolean = false)(body: Int => Unit): Unit = {
      var i = from
      if (step > 0) {
        val limit = if (inclusive) to else to - 1
        while (i <= limit) {
          body(i)
          i += step
        }
      } else {
        val limit = if (inclusive) to else to + 1
        while (i >= limit) {
          body(i)
          i += step
        }
      }
    }

    def sum(to: Int, step: Int = 1, inclusive: Boolean = false)(body: Int => Double): Double = {
      var i = from
      var sum = 0.0
      if (step > 0) {
        val limit = if (inclusive) to else to - 1
        while (i <= limit) {
          sum += body(i)
          i += step
        }
      } else {
        val limit = if (inclusive) to else to + 1
        while (i >= limit) {
          sum += body(i)
          i += step
        }
      }
      sum
    }
  }
}
