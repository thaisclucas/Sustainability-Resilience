for (i in 1:P){
  u <- runif(1)
  if (u<=pc) cross[i] <- i else cross[i] <- 0 #it defines which individuals (i) are going to crossover
}
cr <- cross[!cross==0]
if (length(cr)%%2 != 0){ #number of individuals in crossover must be pair
  out = ceiling(runif(1,0,length(cr)))
  cr[out] <- 0
  cr <- cr[!cr==0]
}
while (length(cr)>1){ #Crossover - start
  c1 = ceiling(runif(1,0,length(cr)))
  k1 <- cr[c1]
  cr[c1] <- 0
  cr <- cr[!cr==0] #it removes individuals who have been through the crossover
  c2 = ceiling(runif(1,0,length(cr)))
  k2 <- cr[c2]
  cr[c2] <- 0
  cr <- cr[!cr==0] #it removes individuals who have been through the crossover
  
  #----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#START
  rule = ceiling(runif(1,0,3))
  
  if(rule == 1){
    level<-GenerateChildren(k1,k2,level)
  }else if(rule == 2){
    of <- GenerateChildren(k1,k2,of)
    ak <- GenerateChildren(k1,k2,ak)
  }else{
    ar <- GenerateChildren(k1,k2,ar)
  }
  if(rule <= 2){
    for(ind in c(k1,k2)){
      network_capacity <- min(sum(SUPPLIERS_CAPACITIES),sum((INITIAL_CAPACITIES[1:M]+ak[[ind]][1:M])*of[[ind]][1:M]),sum((INITIAL_CAPACITIES[(M+1):(M+W)]+ak[[ind]][(M+1):(M+W)])*of[[ind]][(M+1):(M+W)]))
      if(sum(level[[ind]])>network_capacity) level[[ind]] <- level_gen(of[[ind]],ak[[ind]])
      
      sf_WC[[ind]] <- sf_WC_gen(of[[ind]][(M+1):(M+W)],ak[[ind]][(M+1):(M+W)],level[[ind]])
      sf_MW[[ind]] <- sf_MW_gen(of[[ind]][1:M],ak[[ind]][1:M],sf_WC[[ind]])
      sf_SM[[ind]] <- sf_SM_gen(sf_MW[[ind]])
    }
  }
  if(rule >=2){
    for(ind in c(k1,k2)){
      required_resource <- array(0,dim = c(SCENARIO))
      for(l in 1:SCENARIO){
        if (sum(IMPACT[,l])==sum(IMPACT[,l]*of[[ind]])) required_resource[l] <- ceiling(sum((INITIAL_CAPACITIES+ak[[ind]])*IMPACT[,l]*REQUIRED_RESOURCES_TO_RECOVERING)/N)
      }
      min_ar <- max(0,max(required_resource)-INITIAL_RESOURCE)
      if(isTRUE(min_ar>ar[[ind]])) ar[[ind]] <- ar_gen(of[[ind]],ak[[ind]])
    }
  }
  for(ind in c(k1,k2)){
    rr[[ind]] <- rr_gen(of[[ind]],ak[[ind]],ar[[ind]],sf_MW[[ind]],sf_WC[[ind]])
    f_WC[[ind]] <- f_WC_gen(of[[ind]],ak[[ind]],level[[ind]],sf_MW[[ind]],sf_WC[[ind]],rr[[ind]])
    f_MW[[ind]] <- f_MW_gen(of[[ind]],ak[[ind]][1:M],f_WC[[ind]],sf_MW[[ind]],rr[[ind]])
    f_SM[[ind]] <- f_SM_gen(of[[ind]],f_MW[[ind]],sf_SM[[ind]])
    fitness[[ind]] <- CalculateFitness(of[[ind]],ak[[ind]],level[[ind]],ar[[ind]],sf_SM[[ind]],sf_MW[[ind]],sf_WC[[ind]],rr[[ind]],f_SM[[ind]],f_MW[[ind]],f_WC[[ind]])
  }
  
    range <- c(k1,k2)
    for(ind in range){
      ind2 <- range[!range==ind]
      stop <- 0
      for(l in 1:SCENARIO){
        if(stop==1) break
        for(n in 2:N+1){
          required_resource <- sum(REQUIRED_RESOURCES_TO_RECOVERING*rr[[ind]][,n,l]); available_resource <- INITIAL_RESOURCE+ar[[ind]] 
          if(isTRUE(required_resource > available_resource)){
            rr[[ind]] <- rr_gen(ak[[ind]],ar[[ind]],sf_MW[[ind]],sf_WC[[ind]])
            f_WC[[ind]] <- f_WC_gen(of[[ind]][(M+1):(M+W)],ak[[ind]],level[[ind]],sf_MW[[ind]],sf_WC[[ind]],rr[[ind]])
            f_MW[[ind]] <- f_MW_gen(of[[ind]][1:M],ak[[ind]][1:M],f_WC[[ind]],sf_MW[[ind]],rr[[ind]])
            f_SM[[ind]] <- f_SM_gen(f_MW[[ind]],sf_SM[[ind]])
            stop <- 1
            break
          }
        }
      }
      if(stop==0) fitness[[ind]][1] <- fitness[[ind]][1] + AR_INVESTMENT*(ar[[ind]]-ar[[ind2]]) else fitness[[ind]] <- CalculateFitness(of[[ind]],ak[[ind]],level[[ind]],ar[[ind]],sf_SM[[ind]],sf_MW[[ind]],sf_WC[[ind]],rr[[ind]],f_SM[[ind]],f_MW[[ind]],f_WC[[ind]])
    }
  #----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#END
  
'
  pos = ceiling(runif(1,0,DecVarNumber))
  children <- GenerateChildren(k1,k2,pos) #it generates children and applies replacement method
  individuals[[k1]] <- children[1,1:DecVarNumber]
  individuals[[k2]] <- children[2,1:DecVarNumber]
  fitness[[k1]] <- children[1,(DecVarNumber+1):(DecVarNumber+ObjFunNumber)]
  fitness[[k2]] <- children[2,(DecVarNumber+1):(DecVarNumber+ObjFunNumber)]
'
  
}
