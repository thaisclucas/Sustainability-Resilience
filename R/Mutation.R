for (i in 1:P){
  u <- runif(1)
  if (u<=pm) mutated[i] <- i else mutated[i] <- 0 #It defines which individuals (i) are being mutated
}
mut <- mutated[!mutated==0]
while (length(mut)>0){ #Mutation - Begin
  y1 <- ceiling(runif(1,0,length(mut)))
  n1 <- mut[y1]

  #----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#START
  rule = ceiling(runif(1,0,11))
  
  if(rule == 1){
    of[[n1]] <- of_gen()
    ak[[n1]] <- ak_gen(of[[n1]])
    network_capacity <- min(sum(SUPPLIERS_CAPACITIES),sum((INITIAL_CAPACITIES[1:M]+ak[[n1]][1:M])*of[[n1]][1:M]),sum((INITIAL_CAPACITIES[(M+1):(M+W)]+ak[[n1]][(M+1):(M+W)])*of[[n1]][(M+1):(M+W)]))
    if(sum(level[[n1]])>network_capacity) level[[n1]] <- level_gen(of[[n1]],ak[[n1]])
    sf_WC[[n1]] <- sf_WC_gen(of[[n1]][(M+1):(M+W)],ak[[n1]][(M+1):(M+W)],level[[n1]])
    sf_MW[[n1]] <- sf_MW_gen(of[[n1]][1:M],ak[[n1]][1:M],sf_WC[[n1]])
    sf_SM[[n1]] <- sf_SM_gen(sf_MW[[n1]])
    ar[[n1]] <- ar_gen(of[[n1]],ak[[n1]])
    rr[[n1]] <- rr_gen(of[[n1]],ak[[n1]],ar[[n1]],sf_MW[[n1]],sf_WC[[n1]])
    f_WC[[n1]] <- f_WC_gen(of[[n1]],ak[[n1]],level[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]])
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 2){
    ak[[n1]] <- ak_gen(of[[n1]])
    network_capacity <- min(sum(SUPPLIERS_CAPACITIES),sum((INITIAL_CAPACITIES[1:M]+ak[[n1]][1:M])*of[[n1]][1:M]),sum((INITIAL_CAPACITIES[(M+1):(M+W)]+ak[[n1]][(M+1):(M+W)])*of[[n1]][(M+1):(M+W)]))
    if(sum(level[[n1]])>network_capacity) level[[n1]] <- level_gen(of[[n1]],ak[[n1]])
    sf_WC[[n1]] <- sf_WC_gen(of[[n1]][(M+1):(M+W)],ak[[n1]][(M+1):(M+W)],level[[n1]])
    sf_MW[[n1]] <- sf_MW_gen(of[[n1]][1:M],ak[[n1]][1:M],sf_WC[[n1]])
    sf_SM[[n1]] <- sf_SM_gen(sf_MW[[n1]])
    ar[[n1]] <- ar_gen(of[[n1]],ak[[n1]])
    rr[[n1]] <- rr_gen(of[[n1]],ak[[n1]],ar[[n1]],sf_MW[[n1]],sf_WC[[n1]])
    f_WC[[n1]] <- f_WC_gen(of[[n1]],ak[[n1]],level[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]])
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 3){
    level[[n1]] <- level_gen(of[[n1]],ak[[n1]])
    sf_WC[[n1]] <- sf_WC_gen(of[[n1]][(M+1):(M+W)],ak[[n1]][(M+1):(M+W)],level[[n1]])
    sf_MW[[n1]] <- sf_MW_gen(of[[n1]][1:M],ak[[n1]][1:M],sf_WC[[n1]])
    sf_SM[[n1]] <- sf_SM_gen(sf_MW[[n1]])
    rr[[n1]] <- rr_gen(of[[n1]],ak[[n1]],ar[[n1]],sf_MW[[n1]],sf_WC[[n1]])
    f_WC[[n1]] <- f_WC_gen(of[[n1]],ak[[n1]],level[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]])
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 4){
    sf_WC[[n1]] <- sf_WC_gen(of[[n1]][(M+1):(M+W)],ak[[n1]][(M+1):(M+W)],level[[n1]])
    sf_MW[[n1]] <- sf_MW_gen(of[[n1]][1:M],ak[[n1]][1:M],sf_WC[[n1]])
    sf_SM[[n1]] <- sf_SM_gen(sf_MW[[n1]])
    rr[[n1]] <- rr_gen(of[[n1]],ak[[n1]],ar[[n1]],sf_MW[[n1]],sf_WC[[n1]])
    f_WC[[n1]] <- f_WC_gen(of[[n1]],ak[[n1]],level[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]])
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 5){
    sf_MW[[n1]] <- sf_MW_gen(of[[n1]][1:M],ak[[n1]][1:M],sf_WC[[n1]])
    sf_SM[[n1]] <- sf_SM_gen(sf_MW[[n1]])
    rr[[n1]] <- rr_gen(of[[n1]],ak[[n1]],ar[[n1]],sf_MW[[n1]],sf_WC[[n1]])
    f_WC[[n1]] <- f_WC_gen(of[[n1]],ak[[n1]],level[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]])
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 6){
    sf_SM[[n1]] <- sf_SM_gen(sf_MW[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 7){
    ar[[n1]] <- ar_gen(of[[n1]],ak[[n1]])
    rr[[n1]] <- rr_gen(of[[n1]],ak[[n1]],ar[[n1]],sf_MW[[n1]],sf_WC[[n1]])
    f_WC[[n1]] <- f_WC_gen(of[[n1]],ak[[n1]],level[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]])
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 8){
    rr[[n1]] <- rr_gen(of[[n1]],ak[[n1]],ar[[n1]],sf_MW[[n1]],sf_WC[[n1]])
    f_WC[[n1]] <- f_WC_gen(of[[n1]],ak[[n1]],level[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]])
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 9){
    f_WC[[n1]] <- f_WC_gen(of[[n1]],ak[[n1]],level[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]])
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else if(rule == 10){
    f_MW[[n1]] <- f_MW_gen(of[[n1]],ak[[n1]][1:M],f_WC[[n1]],sf_MW[[n1]],rr[[n1]])
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }else{
    f_SM[[n1]] <- f_SM_gen(of[[n1]],f_MW[[n1]],sf_SM[[n1]])
  }
  fitness[[n1]] <- CalculateFitness(of[[n1]],ak[[n1]],level[[n1]],ar[[n1]],sf_SM[[n1]],sf_MW[[n1]],sf_WC[[n1]],rr[[n1]],f_SM[[n1]],f_MW[[n1]],f_WC[[n1]])
  #----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#END
' 
  pos <- ceiling(runif(1,0,DecVarNumber))
  individuals[[n1]][pos] <- GenerateVariable(integer[pos],bound[pos,]) #mutation
  fitness[[n1]] <- CalculateFitness(individuals[[n1]]) #mutated individual fitness evaluating
'

  mut[y1] <- 0
  mut <- mut[!mut==0] #It removes individuals who have been through the mutation
}# Mutation - End
