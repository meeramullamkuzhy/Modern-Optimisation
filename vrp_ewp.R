#Create Solomon data matrix

#print("Starting getData")
solomon <- read.csv("/Users/meeramohanm/BCU/MO_Assignment/solomon_data.csv")
mat1 <- data.frame(x=solomon$XCOORD., y=solomon$YCOORD.)
solomon_mat <- dist(mat1, method = "euclidean", diag=T, upper=T)
solomon_mat <- as.matrix(solomon_mat)


#############################################################################
#Calculate the arrival time for each node
n_customers <<- 100
arrival_time <- numeric(100)
depot <- solomon[1,]
service_times <- solomon$SERVICE.TIME  # Customer service times
time_windows <- data.frame(x=solomon$READY.TIME , y = solomon$DUE.DATE)  # Customer time windows
customers <- solomon[-1,]
n_customers <- nrow(customers)
euclidean_distance <- function(point1, point2) {
  return(sqrt((point1[2] - point2[2])^2 + (point1[3] - point2[3])^2))
}

arrival_time[1] = 0
for (i in 1:101) {
  # Calculate travel time from the depot to the current customer
  travel_time <- euclidean_distance(depot, customers[i,])
  #travel_time <- solomon_mat[depot$CUST.NO., customers[i,]$CUST.NO.]
  
  # Calculate arrival time for the current customer
  arrival_time[i+1] <- max(travel_time, time_windows[i,1]) + service_times[i]
  
  # Update the arrival time for the next customer
  if (i+1 < n_customers) {
    arrival_time[i + 1+1] <- max(arrival_time[i] + travel_time, time_windows[i + 1,1]) + service_times[i + 1]
  }
}

cat("Arrival Time for Customers:\n")
for (i in 1:n_customers+1) {
  cat("Customer", i, ": ", arrival_time[i], "\n")
}
######################################################
#Calculate tourlength
tourLength <- function(tour, solomon_mat) {
  tourlength <- 0
  penalty <- 0
  tour <- c(tour, tour[1])             #e.g. convert A,B,C to A,B,C,A. Thus the tour finishes where it started.
  route <- embed(tour, 2)[,2:1]        #converts the tour into a matrix of trips. i.e. 
  route_length <<- nrow(route)
  
  #--------------Start : Capacity & vehicle count Constraint--------------------------
  vehicle_load <- 0
  vehicle_count <- 0 
  for(i in 1:route_length)
  {
    if(vehicle_count <= 25)
    {
      #cat("\n Vehicle Count : ", vehicle_count, "\n")
      vehicle_load <- vehicle_load + solomon$DEMAND[route[i,1]]
      if(vehicle_load >= 200)
      {
        vehicle_count <- vehicle_count + 1
        vehicle_load <- 0
      }
    }
    tourlength = tourlength + solomon_mat[route[i,1],route[i,2]]
    cat("Tourlength before penalty : ", tourlength, "\n")
    flag <- 0
    depart_time <- 0
    depart_time <- arrival_time[route[i,2]] + solomon$SERVICE.TIME[route[i,2]]
    if(arrival_time[route[i,2]] < solomon$READY.TIME[route[i,2]]){
        tourlength = tourlength + solomon_mat[1,route[i,2]]
    }
    else if(arrival_time[route[i,2]] > solomon$DUE.DATE[route[i,2]])
    {
      flag <- 1
    }
  }
  
  #--------------Start : Capacity & vehicle count Constraint--------------------------

  if(flag == 1)
  {
    fitness_product = 0
  }
  else
  {
    fitness_product = 1/vehicle_count * 1/tourlength  
  }

cat("\n", " No.of Vehicles : ", vehicle_count, "\n", "Fitness : ", fitness_product[1], "\n") 
return(fitness_product) 
}

vrp_EWP_fitness_function <- function(tour, ...){
  return (tourLength(tour, ...))
}
