#Eliminate dominated individuals of P#
j1 <- 1
while(j1<length(fitness)){
  j2 <- j1+1; j1_elim <- 0
  while(j2 <= length(fitness)){
    dom_1 <- c(rep(0,ObjFunNumber))
    dom_2 <- c(rep(0,ObjFunNumber))
    for (o in 1:ObjFunNumber){
      fit1 <- round(fitness[[j1]][o],dec[o])
      fit2 <- round(fitness[[j2]][o],dec[o])
      if(isTRUE(fit1>fit2)) dom_1[o]<-1 else if(isTRUE(fit1<fit2)) dom_2[o]<-1
    }
    if (sum(dom_1)>0 & sum(dom_2)==0){
      #individuals[[j2]]<-NULL
      of[[j2]]<-NULL; ak[[j2]]<-NULL;level[[j2]]<-NULL; sf_SM[[j2]]<-NULL; sf_MW[[j2]]<-NULL; sf_WC[[j2]]<-NULL; ar[[j2]]<-NULL; rr[[j2]]<-NULL; f_SM[[j2]]<-NULL; f_MW[[j2]]<-NULL; f_WC[[j2]]<-NULL #resilient and sustainable operations network design
      fitness[[j2]]<-NULL
    }else if (sum(dom_1)==0 & sum(dom_2)>0){
      #individuals[[j1]]<-NULL
      of[[j1]]<-NULL; ak[[j1]]<-NULL;level[[j1]]<-NULL; sf_SM[[j1]]<-NULL; sf_MW[[j1]]<-NULL; sf_WC[[j1]]<-NULL; ar[[j1]]<-NULL; rr[[j1]]<-NULL; f_SM[[j1]]<-NULL; f_MW[[j1]]<-NULL; f_WC[[j1]]<-NULL #resilient and sustainable operations network design
      fitness[[j1]]<-NULL
      j1_elim <- 1
      break #j2 <- length(fitness)+1
    }else{
      j2<-j2+1 
    }
  }
  if(j1_elim != 1) j1 <- j1+1
}

#Paux upadate#
dis <- 0
if(ng==0){
  ja = 0
  for(j in 1:length(fitness)){
    ja <- ja+1
    #aux_individuals[[ja]] <- individuals[[j]]
    aux_of[[ja]]<-of[[j]]; aux_ak[[ja]]<-ak[[j]]; aux_level[[ja]]<-level[[j]]; aux_sf_SM[[ja]]<-sf_SM[[j]]; aux_sf_MW[[ja]]<-sf_MW[[j]]; aux_sf_WC[[ja]]<-sf_WC[[j]]; aux_ar[[ja]]<-ar[[j]]; aux_rr[[ja]]<-rr[[j]]; aux_f_SM[[ja]]<-f_SM[[j]]; aux_f_MW[[ja]]<-f_MW[[j]]; aux_f_WC[[ja]]<-f_WC[[j]] #resilient and sustainable operations network design
    aux_fitness[[ja]] <- fitness[[j]]
  }
}else{
  j <- 1
  while(j <= length(fitness)){
    ja <- 1; j_elim <- 0
    while(ja <= length(aux_fitness)){
      dom_1 <- c(rep(0,ObjFunNumber))
      dom_2 <- c(rep(0,ObjFunNumber))
      for (o in 1:ObjFunNumber){
        fit1 <- round(fitness[[j]][o],dec[o])
        fit2 <- round(aux_fitness[[ja]][o],dec[o])
        if(isTRUE(fit1>fit2)) dom_1[o]<-1 else if(isTRUE(fit1<fit2)) dom_2[o]<-1
      }
      if (sum(dom_1)>0 & sum(dom_2)==0){
        #aux_individuals[[ja]] <- NULL
        aux_of[[ja]]<-NULL; aux_ak[[ja]]<-NULL; aux_level[[ja]]<-NULL; aux_sf_SM[[ja]]<-NULL; aux_sf_MW[[ja]]<-NULL; aux_sf_WC[[ja]]<-NULL; aux_ar[[ja]]<-NULL; aux_rr[[ja]]<-NULL; aux_f_SM[[ja]]<-NULL; aux_f_MW[[ja]]<-NULL; aux_f_WC[[ja]]<-NULL #resilient and sustainable operations network design
        aux_fitness[[ja]] <- NULL
      }else if (sum(dom_1)==0 & sum(dom_2)>0){
        dis <- dis+1
        #dis_individuals[[dis]] <- individuals[[j]]
        dis_of[[dis]]<-of[[j]]; dis_ak[[dis]]<-ak[[j]]; dis_level[[dis]]<-level[[j]]; dis_sf_SM[[dis]]<-sf_SM[[j]]; dis_sf_MW[[dis]]<-sf_MW[[j]]; dis_sf_WC[[dis]]<-sf_WC[[j]]; dis_ar[[dis]]<-ar[[j]]; dis_rr[[dis]]<-rr[[j]]; dis_f_SM[[dis]]<-f_SM[[j]]; dis_f_MW[[dis]]<-f_MW[[j]]; dis_f_WC[[dis]]<-f_WC[[j]] #resilient and sustainable operations network design
        dis_fitness[[dis]] <- fitness[[j]]
        #individuals[[j]] <- NULL
        of[[j]]<-NULL; ak[[j]]<-NULL;level[[j]]<-NULL; sf_SM[[j]]<-NULL; sf_MW[[j]]<-NULL; sf_WC[[j]]<-NULL; ar[[j]]<-NULL; rr[[j]]<-NULL; f_SM[[j]]<-NULL; f_MW[[j]]<-NULL; f_WC[[j]]<-NULL #resilient and sustainable operations network design
        fitness[[j]] <- NULL
        j_elim <- 1
        break #ja <- length(aux_fitness)+1
      }else{
        ja<-ja+1
      }
    }
    if(j_elim != 1) j <- j+1
  }
  ja <- length(aux_fitness)
  for (j in 1:length(fitness)){
    ja <- ja + 1
    #aux_individuals[[ja]] <- individuals[[j]]
    aux_of[[ja]]<-of[[j]]; aux_ak[[ja]]<-ak[[j]]; aux_level[[ja]]<-level[[j]]; aux_sf_SM[[ja]]<-sf_SM[[j]]; aux_sf_MW[[ja]]<-sf_MW[[j]]; aux_sf_WC[[ja]]<-sf_WC[[j]]; aux_ar[[ja]]<-ar[[j]]; aux_rr[[ja]]<-rr[[j]]; aux_f_SM[[ja]]<-f_SM[[j]]; aux_f_MW[[ja]]<-f_MW[[j]]; aux_f_WC[[ja]]<-f_WC[[j]] #resilient and sustainable operations network design
    aux_fitness[[ja]] <- fitness[[j]]
  }
}

#maintaing the auxiliar population with 150 individuals, in maximum#
if(length(aux_fitness)>150){
  while(length(aux_fitness)>150){
    ja <- ceiling(runif(1,0,length(aux_fitness)))
    #aux_individuals[[ja]] <- NULL
    aux_of[[ja]]<-NULL; aux_ak[[ja]]<-NULL; aux_level[[ja]]<-NULL; aux_sf_SM[[ja]]<-NULL; aux_sf_MW[[ja]]<-NULL; aux_sf_WC[[ja]]<-NULL; aux_ar[[ja]]<-NULL; aux_rr[[ja]]<-NULL; aux_f_SM[[ja]]<-NULL; aux_f_MW[[ja]]<-NULL; aux_f_WC[[ja]]<-NULL #resilient and sustainable operations network design
    aux_fitness[[ja]] <- NULL
  }
}


#maintaing the population with P individuals#
if(dis>0){
  for (j in 1:dis){
    #dis_individuals[[j]] -> individuals[[length(fitness)+1]]
    dis_of[[j]]->of[[length(fitness)+1]]; dis_ak[[j]]->ak[[length(fitness)+1]]; dis_level[[j]]->level[[length(fitness)+1]]; dis_sf_SM[[j]]->sf_SM[[length(fitness)+1]]; dis_sf_MW[[j]]->sf_MW[[length(fitness)+1]]; dis_sf_WC[[j]]->sf_WC[[length(fitness)+1]]; dis_ar[[j]]->ar[[length(fitness)+1]]; dis_rr[[j]]->rr[[length(fitness)+1]]; dis_f_SM[[j]]->f_SM[[length(fitness)+1]]; dis_f_MW[[j]]->f_MW[[length(fitness)+1]]; dis_f_WC[[j]]->f_WC[[length(fitness)+1]] #resilient and sustainable operations network design
    dis_fitness[[j]] -> fitness[[length(fitness)+1]]
  }
}
while(length(fitness) < P){
  ja <- ceiling(runif(1,0,length(aux_fitness)))
  #aux_individuals[[ja]] -> individuals[[length(fitness)+1]]
  aux_of[[ja]]->of[[length(fitness)+1]]; aux_ak[[ja]]->ak[[length(fitness)+1]]; aux_level[[ja]]->level[[length(fitness)+1]]; aux_sf_SM[[ja]]->sf_SM[[length(fitness)+1]]; aux_sf_MW[[ja]]->sf_MW[[length(fitness)+1]]; aux_sf_WC[[ja]]->sf_WC[[length(fitness)+1]]; aux_ar[[ja]]->ar[[length(fitness)+1]]; aux_rr[[ja]]->rr[[length(fitness)+1]]; aux_f_SM[[ja]]->f_SM[[length(fitness)+1]]; aux_f_MW[[ja]]->f_MW[[length(fitness)+1]]; aux_f_WC[[ja]]->f_WC[[length(fitness)+1]] #resilient and sustainable operations network design
  aux_fitness[[ja]] -> fitness[[length(fitness)+1]]
}