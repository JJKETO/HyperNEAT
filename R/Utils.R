#Print function for one gene
print.gene <- function(genome){
  cat("Start Node: ", genome$startNode,
      "\nEnd Node: ", genome$endNode,
      "\nWeight: ", genome$weight,
      "\nIndex: ", genome$index,
      "\nActive Status: ", genome$activeStatus,
      "\nActivation Function: ", genome$activationFunction,
      "\n")
}
