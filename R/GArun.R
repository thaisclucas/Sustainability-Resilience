#aux_individuals <- list() #auxiliary population to store non-dominated individuals NÃO USA ~~ tou usando
aux_of <- list();aux_ak <- list();aux_level <- list();aux_sf_SM <- list();aux_sf_MW <- list();aux_sf_WC <- list();aux_ar <- list();aux_rr <- list();aux_f_SM <- list();aux_f_MW <- list();aux_f_WC <- list() #resilient and sustainable operations network design
aux_fitness <- list() #fitness of individuals stored in aux_individuals
#dis_individuals <- list() #auxiliary population used to maintain the main population with P individuals #multi
dis_of <- list();dis_ak <- list();dis_level <- list();dis_sf_SM <- list();dis_sf_MW <- list();dis_sf_WC <- list();dis_ar <- list();dis_rr <- list();dis_f_SM <- list();dis_f_MW <- list();dis_f_WC <- list() #resilient and sustainable operations network design
dis_fitness <- list() #fitness of individuals stored in dis_individuals #multi

cross <- c(rep(0,P)) #vector to support the crossover step
mutated <- c(rep(0,P)) #vector to support the mutation step
fit <- c(rep(0,P)) #vector to support the selection step

#best_individual <- c(rep(0,DecVarNumber)) #não usa pra resiliência e sustentabilidade
#best_of <- array(dim=c(M+W)); best_ak <- array(dim=c(1)); best_sf_SM <- array(dim=c(S,M)); best_sf_MW <- array(dim=c(M,W)); best_sf_WC <- array(dim=c(W,C)); best_ar <- array(dim=c(1)); best_rr <- array(dim=c(M+W,N+1,SCENARIO)); best_f_SM <- array(dim=c(S,M,N+1,SCENARIO)); best_f_MW <- array(dim=c(M,W,N+1,SCENARIO)); best_f_WC <- array(dim=c(W,C,N+1,SCENARIO))
#best_fitness <- array(dim=c(ObjFunNumber)) #monoobjetivo

source("R/GenerateChildren.R")

source("R/InitialPopulation.R")

ng <- 0
if (ObjFunNumber > 1) source("R/MultiobjectiveSelection.R") else source("R/MonobjectiveSelection.R") #selection for first generation
for (ng in 1:Ngen){ #Generations
  
  #---CROSSOVER---#
  source("R/Crossover.R")
  
  #---MUTATION---#
  source("R/Mutation.R")
  
  #---SELECTION---#
  if (ObjFunNumber > 1) source("R/MultiobjectiveSelection.R") else source("R/MonobjectiveSelection.R")
  
}
