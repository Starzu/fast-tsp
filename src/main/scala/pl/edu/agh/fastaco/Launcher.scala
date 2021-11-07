package pl.edu.agh.fastaco

import com.typesafe.scalalogging.StrictLogging
import pl.edu.agh.fastaco.utils._

import java.util.concurrent.TimeUnit

object Launcher extends StrictLogging {
  def main(args: Array[String]): Unit = {
    // ./acotsp -r 1 -t 120 -i usa13509.tsp -m 25 -g 20 -l 1 -k 20 -a 2 -b 3 -e 0.05 --mmas
    val problem = ProblemData.fromTspFile("data/tsp/pr107.tsp")

    // Theoretical best result limit calculated as a sum of 2 shortest edges connected with each node
    val theoreticalLimit = (0 until problem.dimension)
      .map(from => (0 until problem.dimension).filter(_ != from).map(problem.distance(from, _)).sorted.take(2).sum)
      .sum / 2
    logger.info(s"Theoretical best result limit: $theoreticalLimit")

    val algParams: AlgorithmParams = AlgorithmParams(problem.dimension, iterations = 500)

    val initStart = System.nanoTime()
    val mmas: MMAS = new MMAS(algParams, problem)
    mmas.initData()
    logger.info(s"Init time ${TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - initStart)}ms")

    val globalStart = System.nanoTime()
    val bestResult = mmas.run()
    logger.info(s"$bestResult - Total time (${TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - globalStart)}ms)")
  }
}
