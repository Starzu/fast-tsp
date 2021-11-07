package pl.edu.agh.fastaco.utils

case class AlgorithmParams(
  problemDimension: Int,
  antsCount: Int = 25, // per node in case of desynchronized ACO
  iterations: Int = 1000,
  preferredNeighbourhoodLimit: Int = 20,
  maxPheromones: Double = 0.999,
  minPheromones: Double = 0.001,
  pheromonePower: Double = 2.0,
  desirabilityPower: Double = 3.0,
  pheromoneUpdateUnit: Double = 0.05,
  evaporationCoefficient: Double = 0.05,
  selectBestAnts: Int = 0, // unused in seq MMAS
  additionalGlobalBestUpdates: Int = 1, // unused in seq MMAS
) {
  val initPheromones: Double = maxPheromones
  val neighbourhoodLimit: Int = math.min(preferredNeighbourhoodLimit, problemDimension - 1)
}
