#' Runs HyperNEAT simulation
#'
#' This function runs the HyperNEAT simulation according to the problem configuration
#' @param simulationParameters List of parameters that control the simulation
#' @param substrateConfiguration List of substrate configurations
#' @value Returns a completed simulation according to the end criteria.
#' @export
HyperNEATSimulation <- function(simulationParameters = NULL,
                                substrateConfiguration = NULL){
  # Initial simulation ------------------------------------------------------
  simulation <- list()
  simulation$generation <- generateInitialGeneration(simulationParameters$numGenomes,
                                          simulationParameters$numInputs,
                                          simulationParameters$numOutputs)
  simulation$fitness$globalMaxFitness <- 0
  simulation$fitness$globalMaxFitnessGeneration <- 0
  simulation$fitness$currentMaxFitness <- 0
  simulation$parameters <- simulationParameters
  simulation$currentGeneration <- 1
  simulation$substrateConfiguration <- substrateConfiguration


  # Simulation itself -------------------------------------------------------
  while(simulation$fitness$globalMaxFitness < simulation$parameters$targetFitness &
        simulation$currentGeneration < simulation$parameters$maxGeneration){
    simulation <- runSingleGeneration(simulation)
  }

  return(simulation)
}
