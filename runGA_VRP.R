library(GA)

#This function is used by the GA to compute or report the statistics of your interest after every generation.
#This function overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  # gaMonitor(obj)                      #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter                      #get the current iternation/generation number 
  if (iter <= maxGenerations){          #some error checking
    fitness <- obj@fitness              #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.    
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else{                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}

runGA <- function(noRuns = 30, problem = "feature"){
  #Specify GA parameter values; using the default values below. 
  if (problem == "feature"){
    maxGenerations <<- 20    #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    popSize = 50
    pcrossover = 0.9
    pmutation = 0.1
    type = "binary"
    crossover = 
    data <- getData()
    xx <- data[,-ncol(data)]
    yy <- data[,ncol(data)]
    fitness = featureFitness              #fitness function defined in feature-selection.R
  }
  else if (problem == "tsp"){
    maxGenerations <<- 20
  
    popSize = 50
    pcrossover = 0.98
    pmutation = 0.10
    type = "permutation"
    data = getData()
    lower = 1                             #minimum is city indexed 1
    upper = nrow(getData())               #maximum is the number of cities in the data set
    fitness = vrp_WS_STWC_fitness_function 
    #fitness = vrp_EWP_fitness_function
    #fitness = vrp_EALWP_constraint_fitness_function
    #result_EALWP <- runGA(problem = "tsp")
    #BestFitness_STWC <- getBestFitness()
    #solution_STWC <- getBestSolution()
  }
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  
  
  #Set up what stats you wish to note.    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    if (problem == "feature")
      GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx), 
               names = colnames(xx), seed=i, popSize = popSize, 
               pcrossover = pcrossover, pmutation = pmutation, 
               maxiter = maxGenerations, monitor= monitor)
    else if (problem == "tsp"){
      sink("/Users/meeramohanm/BCU/MO_Assignment/Log Files/8May2023/ox_p3_8May2023.txt", append = TRUE)
      cat("Starting Run ", i, "\n")
      GA <- ga(type = type, fitness = fitness, solomon_mat = getData(), 
                     lower = lower, upper = upper, popSize = popSize, maxiter = maxGenerations,
                     pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i, crossover = gaperm_oxCrossover)
     sink()
    }
    else if (problem == "vehicle")
      GA <- ga(type = type, fitness = fitness, 
               min = min, max = max, popSize = popSize, maxiter = maxGenerations,
               pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
      
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (resultsMatrix)
}

getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}