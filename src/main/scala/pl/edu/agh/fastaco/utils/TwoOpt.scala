package pl.edu.agh.fastaco.utils

import scala.annotation.tailrec

class TwoOpt(algParams: AlgorithmParams, neighbours: Array[Array[Int]], distance: (Int, Int) => Double) {
  import Utils._

  case class SwapPoints(c1: Int, c2: Int, c3: Int, c4: Int, permIdx: Int)
  object SwapPoints {
    final val Empty: SwapPoints = SwapPoints(-1, -1, -1, -1, -1)
  }

  private val position = Array.ofDim[Int](algParams.problemDimension)
  private val dontLook = Array.ofDim[Boolean](algParams.problemDimension)
  private val permutation = Array.ofDim[Int](algParams.problemDimension)

  0.loop(algParams.problemDimension) { i =>
    permutation(i) = i
  }
  Utils.shuffleArray(permutation)

  def twoOpt(tour: Array[Int]): Unit = {
    0.loop(algParams.problemDimension) { i =>
      position(tour(i)) = i
      dontLook(i) = false
    }

    @tailrec
    def loopWhileImproved(): Unit = {
      @tailrec
      def loopPermutation(permIdx: Int, improved: Boolean): Boolean = {
        if (permIdx >= tour.length) improved
        else {
          @tailrec
          def findSwapPoints(permIdx: Int): SwapPoints = {
            if (permIdx >= tour.length) SwapPoints.Empty
            else if (dontLook(permutation(permIdx))) findSwapPoints(permIdx + 1)
            else {
              val c1 = permutation(permIdx)
              val c1Position = position(c1)

              @tailrec
              def findSuccessor(c1Succ: Int, radius: Double, neighbourIdx: Int): SwapPoints = {
                if (neighbourIdx >= algParams.neighbourhoodLimit) SwapPoints.Empty
                else {
                  val c2 = neighbours(c1)(neighbourIdx)
                  if (radius > distance(c1, c2)) {
                    val c2Position = position(c2)
                    val c2Succ = if (c2Position < tour.length - 1) tour(c2Position + 1) else tour(0)
                    if (c1Succ == c2 || c2Succ == c1) findSuccessor(c1Succ, radius, neighbourIdx + 1)
                    else {
                      val diff = -radius + distance(c1, c2) + distance(c1Succ, c2Succ) - distance(c2, c2Succ)
                      if (diff < 0) {
                        SwapPoints(c1, c1Succ, c2, c2Succ, permIdx)
                      } else findSuccessor(c1Succ, radius, neighbourIdx + 1)
                    }
                  } else SwapPoints.Empty
                }
              }

              @tailrec
              def findPredecessor(c1Pred: Int, radius: Double, neighbourIdx: Int): SwapPoints = {
                if (neighbourIdx >= algParams.neighbourhoodLimit) SwapPoints.Empty
                else {
                  val c2 = neighbours(c1)(neighbourIdx)
                  if (radius > distance(c1, c2)) {
                    val c2Position = position(c2)
                    val c2Pred = if (c2Position > 0) tour(c2Position - 1) else tour(tour.length - 1)
                    if (c1Pred == c2 || c2Pred == c1) findPredecessor(c1Pred, radius, neighbourIdx + 1)
                    else {
                      val diff = -radius + distance(c1, c2) + distance(c1Pred, c2Pred) - distance(c2Pred, c2)
                      if (diff < 0) {
                        SwapPoints(c1Pred, c1, c2Pred, c2, permIdx)
                      } else findPredecessor(c1Pred, radius, neighbourIdx + 1)
                    }
                  } else SwapPoints.Empty
                }
              }

              val swapPoints = {
                val c1Succ = if (c1Position < tour.length - 1) tour(c1Position + 1) else tour(0)
                val radius = distance(c1, c1Succ)
                val succResult = findSuccessor(c1Succ, radius, 0)
                if (succResult != SwapPoints.Empty) succResult
                else {
                  val c1Pred = if (c1Position > 0) tour(c1Position - 1) else tour(tour.length - 1)
                  val radius = distance(c1Pred, c1)
                  findPredecessor(c1Pred, radius, 0)
                }
              }

              if (swapPoints == SwapPoints.Empty) {
                dontLook(c1) = true
                findSwapPoints(permIdx + 1)
              } else swapPoints
            }
          }

          val swapPoints = findSwapPoints(permIdx)

          if (swapPoints != SwapPoints.Empty) {
            dontLook(swapPoints.c1) = false
            dontLook(swapPoints.c2) = false
            dontLook(swapPoints.c3) = false
            dontLook(swapPoints.c4) = false

            var h1 = swapPoints.c1
            var h2 = swapPoints.c2
            var h3 = swapPoints.c3
            var h4 = swapPoints.c4
            if (position(h3) < position(h1)) {
              var help = 0
              help = h1
              h1 = h3
              h3 = help
              help = h2
              h2 = h4
              h4 = help
            }
            if (position(h3) - position(h2) < tour.length / 2 + 1) {
              var i = position(h2)
              var j = position(h3)
              while (i < j) {
                val c1 = tour(i)
                val c2 = tour(j)
                tour(i) = c2
                tour(j) = c1
                position(c1) = j
                position(c2) = i
                i += 1
                j -= 1
              }
            } else {
              var i = position(h1)
              var j = position(h4)
              var help = if (j > i) tour.length - (j - i) + 1 else (i - j) + 1
              help = help / 2
              var h = 0
              while (h < help) {
                val c1 = tour(i)
                val c2 = tour(j)
                tour(i) = c2
                tour(j) = c1
                position(c1) = j
                position(c2) = i
                i -= 1
                j += 1
                if (i < 0) i = tour.length - 1
                if (j >= tour.length) j = 0

                h += 1
              }
            }

            loopPermutation(swapPoints.permIdx + 1, improved = true)
          } else improved
        }
      }

      if (loopPermutation(0, improved = false)) loopWhileImproved()
    }

    loopWhileImproved()
  }
}
