#Tournament Method

for (j in 1:P) fit[j] <- fitness[[j]]
order_ind <- order(fit, decreasing = TRUE) #it orders the individuals from the fittest to least fit

if (ng==0) {
  best_of <- of[[order_ind[1]]]; best_ak<-ak[[order_ind[1]]]; best_level <-level[[order_ind[1]]]; best_sf_SM <-sf_SM[[order_ind[1]]]; best_sf_MW <-sf_MW[[order_ind[1]]]; best_sf_WC <-sf_WC[[order_ind[1]]]; best_ar <-ar[[order_ind[1]]]; best_rr <-rr[[order_ind[1]]]; best_f_SM <-f_SM[[order_ind[1]]]; best_f_MW <-f_MW[[order_ind[1]]]; best_f_WC <-f_WC[[order_ind[1]]] #resilient and sustainable operations network design
  #best_individual <- individual[[order_ind[1]]]
  best_fitness <- fitness[[order_ind[1]]]
}else{

  if (fitness[[order_ind[1]]] < best_fitness){
    best_of <- of[[order_ind[1]]]; best_ak<-ak[[order_ind[1]]]; best_level <-level[[order_ind[1]]]; best_sf_SM <-sf_SM[[order_ind[1]]]; best_sf_MW <-sf_MW[[order_ind[1]]]; best_sf_WC <-sf_WC[[order_ind[1]]]; best_ar <-ar[[order_ind[1]]]; best_rr <-rr[[order_ind[1]]]; best_f_SM <-f_SM[[order_ind[1]]]; best_f_MW <-f_MW[[order_ind[1]]]; best_f_WC <-f_WC[[order_ind[1]]] #resilient and sustainable operations network design
    #best_individual <- individual[[order_ind[1]]]
    best_fitness <- fitness[[order_ind[1]]] 
  }
  
  for (j in 1:(P-E)){ #elitist selection
    
    #chosing three different individuals to selection step
    s <- c(1,2,3); sel <- c((E+1):P)
    for (k in 1:3){
      s[k] = ceiling(runif(1,0,length(sel)))
      sel[s[k]] <- 0
      sel <- sel[!sel==0]
      s[k] <- s[k] + E
    }
    
    #selecting the best one and inserting it in auxiliar population
    best <- min(s)
    aux_of[[j]]<-of[[order_ind[best]]]; aux_ak[[j]]<-ak[[order_ind[best]]]; aux_level[[j]]<-level[[order_ind[best]]]; aux_sf_SM[[j]]<-sf_SM[[order_ind[best]]]; aux_sf_MW[[j]]<-sf_MW[[order_ind[best]]]; aux_sf_WC[[j]]<-sf_WC[[order_ind[best]]]; aux_ar[[j]]<-ar[[order_ind[best]]]; aux_rr[[j]]<-rr[[order_ind[best]]]; aux_f_SM[[j]]<-f_SM[[order_ind[best]]]; aux_f_MW[[j]]<-f_MW[[order_ind[best]]]; aux_f_WC[[j]]<-f_WC[[order_ind[best]]] #resilient and sustainable operations network design
    #aux_individuals[[j]] <- individuals[[order_ind[best]]]
    aux_fitness[[j]] <- fitness[[order_ind[best]]]
    
  }
  
  #Selection
  for (j in (E+1):P){
    aux_of[[j-E]]->of[[order_ind[j]]]; aux_ak[[j-E]]->ak[[order_ind[j]]]; aux_level[[j-E]]->level[[order_ind[j]]]; aux_sf_SM[[j-E]]->sf_SM[[order_ind[j]]]; aux_sf_MW[[j-E]]->sf_MW[[order_ind[j]]]; aux_sf_WC[[j-E]]->sf_WC[[order_ind[j]]]; aux_ar[[j-E]]->ar[[order_ind[j]]]; aux_rr[[j-E]]->rr[[order_ind[j]]]; aux_f_SM[[j-E]]->f_SM[[order_ind[j]]]; aux_f_MW[[j-E]]->f_MW[[order_ind[j]]]; aux_f_WC[[j-E]]->f_WC[[order_ind[j]]] #resilient and sustainable operations network design
    #individuals[[order_ind[j]]] <- aux_individuals[[j-E]]
    aux_fitness[[j-E]] -> fitness[[order_ind[j]]]
  }
} 