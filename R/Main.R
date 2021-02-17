library(stringr)

#-----------------PROBLEM PARAMETERS------------------------##
source("R/ProblemParameters.R")

##----------------GA PARAMETERS--------------------------##
Ngen <- 200; P <- 200; E <- 10; pc <- 0.9; pm <- 0.1 #AG Parameters
#DecVarNumber <- 2 #usa no GA genérico monoobjetivo
ObjFunNumber <- 5; #Model Parameters
dec <- c(-5,1, -2, -1, -2) # Ajustar a depender das funções analisadas dec <- c(-5,1,-2,-1,-2) 
if ((P-E) < 3) E <- P-3

##---------TYPING and BOUNDS of DECISION VARIABLES----------##
#bound <- array(dim = c(DecVarNumber,2))
#integer <- array(0,dim = c(DecVarNumber))

#------Weibull_Exponential_MLE------START 
#bound[1,] <- c(0,2000); bound[2,] <- c(0,2000); bound[3,] <- c(0,10)
#integer[1] <- 0; integer[2] <- 0; integer[3] <- 0
#-------Weibull_Exponential_MLE------END 

#--Replacement policy for systems subjected to imperfect repairs--START 
'bound[1,] <- c(1,H)
bound[2,] <- c(0,floor(budget/s_cost))
integer[1] <- 1
integer[2] <- 1'
#--Replacement policy for systems subjected to imperfect repairs--END 

##-----------------PERFORMING GA-----------------------------##
number_of_replications <- 10 #it defines how many times MOGA is going to be run
for(nr in 1:number_of_replications){
  source("R/GArun.R")
  source("R/ParetoSolutions.R")
}
