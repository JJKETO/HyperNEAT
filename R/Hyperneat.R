#' Generate initial generation
#'
#' This function creates initial pool of genomes.
#' @param numGenomes Number of genomes
#' @param numInputs  Number of inputs
#' @param numOutputs Number of outputs
#' @export
generateInitialGeneration <- function(numGenomes=NULL, numInputs=NULL, numOutputs=NULL){
  if(numInputs  < 1) stop( "numInputs needs to be positive integer")
  if(numOutputs < 1) stop("numOutputs needs to be positive integer")
  if(numGenomes < 1) stop("numGenomes needs to be positive integer")
  generation <- replicate(numGenomes, generateInitialGenome(numInputs, numOutputs),simplify = FALSE )
  class(generation) <- "generation"
  return(generation)
}

generateInitialGenome <- function(numInputs=NULL, numOutputs=NULL){
  inputs <- c(1:numInputs)
  outputs <- c(1:numOutputs)

  genome <- list()
  for(input in inputs){
    for(output in outputs){
      index <- numOutputs*(input-1) + output
      genome$gene[[index]] <- generateInitialGene(input, output, index)
    }
  }
  genome$fitness <- 0
  genome$globalRank <- 0
  genome$adjustedFitness
  class(genome) <- "genome"
  return(genome)
}

#Generates initial genes with relu's and initial weight from runif(1,0,1)
generateInitialGene <- function(startNode, endNode, index){
  gene <- list(
    startNode = startNode,
    endNode = endNode,
    activationFunction = "relu",
    weight = runif(1,0,1),
    index = index,
    activeStatus = TRUE
  )
  class(gene) <- "gene"
  return(gene)
}
