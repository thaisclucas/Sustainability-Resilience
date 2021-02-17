realfront <- read.table("Data/RealParetoFront.csv", header = T, sep = ";", dec = ",")
normalized_realfront <- matrix(nrow = nrow(realfront), ncol=ncol(realfront))

simulatedfront <- read.table("Results/Fitness_18.csv", header = T, sep = ";", dec = ",")
normalized_simulatedfront <- matrix(nrow = nrow(simulatedfront), ncol=ncol(simulatedfront))

#normalizationfactor <- matrix(nrow = ObjFunNumber, ncol=2)
#for(k in 1:ObjFunNumber){
#  normalizationfactor[k,1] <- -(max(realfront[,k]))
#  normalizationfactor[k,2] <- -(min(realfront[,k],simulatedfront[,k]))
#}

normalizationfactor <- matrix(c(16.95744062,2.23106817,0.071428091,0,27.16,3.73,0.1,144000),nrow = ObjFunNumber, ncol=2)

for(j in 1:length(normalized_realfront[,1])){
  for(k in 1:ObjFunNumber) normalized_realfront[j,k]<-(realfront[j,k]+normalizationfactor[k,2])/(normalizationfactor[k,2]-normalizationfactor[k,1])
}
for(j in 1:length(normalized_simulatedfront[,1])){
  for(k in 1:ObjFunNumber) normalized_simulatedfront[j,k]<-(simulatedfront[j,k]+normalizationfactor[k,2])/(normalizationfactor[k,2]-normalizationfactor[k,1])
}

minimum_distances <- matrix(nrow=nrow(normalized_simulatedfront), ncol=2)
distances <- matrix(nrow = nrow(normalized_simulatedfront), ncol = nrow(normalized_realfront))
fitness_minimum_distances <- matrix(nrow = nrow(minimum_distances), ncol = ObjFunNumber)
sum <- 0

for(j1 in 1:length(normalized_simulatedfront[,1])){
  for(j2 in 1:length(normalized_realfront[,1])){
    sum <- 0
    for(k in 1:ObjFunNumber){
      sum <- sum + (normalized_simulatedfront[j1,k]-normalized_realfront[j2,k])^2
    }
    distances[j1,j2] <- sqrt(sum)
  }
}

for(j in 1:length(minimum_distances[,1])){
  posit <- order(distances[j,])
  minimum_distances[j,] <- c(distances[j,posit[1]],posit[1])
}

for(j in 1:length(fitness_minimum_distances[,1])){
  for(k in 1:ObjFunNumber){
    fitness_minimum_distances[j,k] <- abs(simulatedfront[j,k]-realfront[minimum_distances[j,2],k])
  }
}

write.table(normalized_simulatedfront, "Results/nomalized_fitness_18.csv", row.names = F, col.names = T, sep = ";", dec = ",")
write.table(fitness_minimum_distances, "Results/fitness_distances_18.csv", row.names = F, col.names = T, sep = ";", dec = ",")
write.table(minimum_distances, "Results/euclidean_distances_18.csv", row.names = F, col.names = T, sep = ";", dec = ",")
