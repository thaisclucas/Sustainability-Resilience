source("R/CalculateFitness.R")
'
individuals <- list()
fitness <- list()

GenerateVariable <- function(int,bounds){
  gen <- array(dim=c(DecVarNumber))
  if(int==1) gen <- floor(runif(1,bounds[1],bounds[2]+1))
  else gen <- runif(1,bounds[1],bounds[2])
  return(gen)
}

for (i in 1:P){
  individuals[[i]]<-array(dim = DecVarNumber)
  for(k in 1:DecVarNumber) individuals[[i]][k] <- GenerateVariable(integer[k],bound[k,])
  fitness[[i]] <- CalculateFitness(individuals[[i]])
}
'
#----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#START
of <- list();ak <- list();level <- list();sf_SM <- list();sf_MW <- list();sf_WC <- list();ar <- list();rr <- list();f_SM <- list();f_MW <- list();f_WC <- list() #resilient and sustainable operations network design
fitness <- list()

of_gen <- function(){
  
  not_open <- round(runif(1))
  open_all <- round(runif(1))
  if (not_open==1) {
    open_facility <- OF_INITIAL
 #   of_m1 <- c(rep(0,M1)); of_w1 <- c(rep(0,W1))
  }else if(open_all==1){
    open_facility <- c(rep(1, M+W))
#    of_m1 <- c(rep(1,M1)); of_w1 <- c(rep(1,W1))
  }else{
    open_facility <- OF_INITIAL
    for (i in 1:M+W){
      if(open_facility[i]==0) open_facility[i] = round(runif(1))
    }
  #  of_m1 <- round(runif(M1)); of_w1 <- round(runif(W1))
  }
  #open_facility <- c(c(rep(1,M-M1)),of_m1,c(rep(1,W-W1)),of_w1)
  if (sum(open_facility[1:M])==0) open_facility[ceiling(runif(1,0,M))] <- 1
  if (sum(open_facility[(M+1):(M+W)])==0) open_facility[ceiling(runif(1,M,M+W))] <- 1
  return(open_facility)
}

ak_gen <- function(open_facility){
  additional_capacity <- array(0,dim = c(M+W))
  not_add <- round(runif(1))
  if (not_add==0){
    for(j in 1:(M+W)){
      if (open_facility[j]==1){
        add <- round(runif(1))
        if(add==1) additional_capacity[j] <- round(runif(1,MIN_ADDITIONAL_CAPACITIES[j],MAX_ADDITIONAL_CAPACITIES[j]),-1)
      }
    }
  }
  return(additional_capacity)
}

level_gen <- function(open_facility,additional_capacity){
  lev <- array(0,dim = c(C))
  network_capacity <- min(sum(SUPPLIERS_CAPACITIES),sum((INITIAL_CAPACITIES[1:M]+additional_capacity[1:M])*open_facility[1:M]),sum((INITIAL_CAPACITIES[(M+1):(M+W)]+additional_capacity[(M+1):(M+W)])*open_facility[(M+1):(M+W)]))
  total_demand <- sum(DEMANDS)
  maximum <- round(runif(1))
  if (maximum==1){
    if(network_capacity >= total_demand){
      lev <- DEMANDS
    }else{
      for(j in 1:(C-1)) lev[j] <- floor(runif(1,max(network_capacity-sum(lev)-sum(DEMANDS[(j+1):C]),0),min(network_capacity-sum(lev),DEMANDS[j])+1))
      lev[C] <- network_capacity-sum(lev)
    }
  }else{
    if(network_capacity >= total_demand){
      for(j in 1:C){
        maximum <- round(runif(1))
        if(maximum==1) lev[j] <- DEMANDS[j] else lev[j] <- round(runif(1,0,DEMANDS[j]))
      }
    }else{
      for(j in 1:C) lev[j] <- floor(runif(1,0,min(network_capacity-sum(lev),DEMANDS[j])+1))
    }
  }
  return(lev)
}  

sf_WC_gen <- function(wareh_of,wareh_capacity,lev){
  sf <- array(0,dim = c(W,C)); supply <- c(1:W)
  for(k in 1:C){
    demand <- lev[k]
    while (demand > 0){
      pos <- ceiling(runif(1,0,length(supply))); j <- supply[pos]
      if(wareh_of[j]==1){
        sup <- INITIAL_CAPACITIES[M+j]+wareh_capacity[j]-sum(sf[j,])
        sf[j,k] <- min(sup,demand)
        if (sup-sf[j,k]==0) supply <- supply[!supply==j]
      }else{
        supply <- supply[!supply==j]
      }
      demand <- demand-sf[j,k]
    }
  }

  'for(k in 1:C){
    for(j in 1:(W-1)){
      if(wareh_of[j]==1){
        if (sum(wareh_of[(j+1):W])==0) sf[j,k] <- lev[k] - sum(sf[,k])
        else sf[j,k] <- round(runif(1,max(lev[k]-sum(sf[,k])-sum(INITIAL_CAPACITIES[(M+j+1):(M+W)]+wareh_capacity[(j+1):W])+sum(sf[(j+1):W,]),0),min(lev[k]-sum(sf[,k]),INITIAL_CAPACITIES[M+j]+wareh_capacity[j]-sum(sf[j,]))),-1)
      }
    }
    if (wareh_of[W]==1) sf[W,k] <- lev[k] - sum(sf[,k])
  }'

  return(sf)
}

sf_MW_gen <- function(manuf_of,manuf_capacity,WC_sf){
  sf <- array(0,dim = c(M,W)); supply <- c(1:M)
  for(k in 1:W){
    demand <- sum(WC_sf[k,])
    while (demand > 0){
      pos <- ceiling(runif(1,0,length(supply))); j <- supply[pos]
      if(manuf_of[j]==1){
        sup <- INITIAL_CAPACITIES[j]+manuf_capacity[j]-sum(sf[j,])
        sf[j,k] <- min(sup,demand)
        if (sup-sf[j,k]==0) supply <- supply[!supply==j]
      }else{
        supply <- supply[!supply==j]
      }
      demand <- demand-sf[j,k]
    }
  }

  'for(k in 1:W){
    for(j in 1:(M-1)){
      if(manuf_of[j]==1){
        if (sum(manuf_of[(j+1):M])==0) sf[j,k] <- sum(WC_sf[k,]) - sum(sf[,k])
        else sf[j,k] <- round(runif(1,max(sum(WC_sf[k,])-sum(sf[,k])-sum(INITIAL_CAPACITIES[(j+1):M]+manuf_capacity[(j+1):M])+sum(sf[(j+1):M,]),0),min(sum(WC_sf[k,])-sum(sf[,k]),INITIAL_CAPACITIES[j]+manuf_capacity[j]-sum(sf[j,]))),-1)
      }
    }
    if (manuf_of[M]==1) sf[M,k] <- sum(WC_sf[k,]) - sum(sf[,k])
  }'

  return(sf)
}  

sf_SM_gen <- function(MW_sf){
  sf <- array(0,dim = c(S,M)); supply <- c(1:S)
  for(k in 1:M){
    demand <- sum(MW_sf[k,])
    while (demand > 0){
      pos <- ceiling(runif(1,0,length(supply))); j <- supply[pos]
      sup <- SUPPLIERS_CAPACITIES[j]-sum(sf[j,])
      sf[j,k] <- min(sup,demand)
      if (sup-sf[j,k]==0) supply <- supply[!supply==j]
      demand <- demand-sf[j,k]
    }
  }

  'for(k in 1:M){
    for(j in 1:(S-1)) sf[j,k] <- round(runif(1,max(sum(MW_sf[k,])-sum(sf[,k])-sum(SUPPLIERS_CAPACITIES[(j+1):S])+sum(sf[(j+1):S,]),0),min(sum(MW_sf[k,])-sum(sf[,k]),SUPPLIERS_CAPACITIES[j]-sum(sf[j,]))),-1)
    sf[S,k] <- sum(MW_sf[k,]) - sum(sf[,k])
  }'

  return(sf)
}

ar_gen <- function(open_facility,additional_capacity){
  required_resource <- array(0,dim = c(SCENARIO))
  for(l in 1:SCENARIO){
    if (sum(IMPACT[,l])==sum(IMPACT[,l]*open_facility)) required_resource[l] <- ceiling(sum((INITIAL_CAPACITIES+additional_capacity)*IMPACT[,l]*REQUIRED_RESOURCES_TO_RECOVERING)/N)
  }
  min_ar <- max(0,max(required_resource)-INITIAL_RESOURCE)
  minimum <- round(runif(1))
  if(minimum == 0) additional_resource <- ceiling(runif(1,min_ar,MAX_ADDITIONAL_RESOURCE)) else additional_resource <- min_ar
  return(additional_resource)
}
#MAX_ADDITIONAL_RESOURCE = 2500000
'for (i in 1:P){
  if(is.na(ar[i]) == TRUE) break
  
  }
open_facility <-of[[3]]
additional_capacity <- ak[[3]]'
rr_gen <- function(open_facility,additional_capacity,additional_resource,MW_sf,WC_sf){
  recovery_rate <- array(0,dim = c((M+W),(N+1),SCENARIO))
  for(l in 1:SCENARIO){
    if (sum(IMPACT[,l])==sum(IMPACT[,l]*open_facility)){
      total_kapacity_to_recover <- ceiling((INITIAL_CAPACITIES+additional_capacity)*IMPACT[,l])
      useful_kapacity_to_recover <- array(dim = c(M+W))
      for(j in 1:M) useful_kapacity_to_recover[j] <- max(0,sum(MW_sf[j,])-floor((INITIAL_CAPACITIES[j]+additional_capacity[j])*(1-IMPACT[j,l])))
      for(j in 1:W) useful_kapacity_to_recover[M+j] <- max(0,sum(WC_sf[j,])-floor((INITIAL_CAPACITIES[M+j]+additional_capacity[M+j])*(1-IMPACT[M+j,l])))
      total_required_resource <- ceiling(total_kapacity_to_recover*REQUIRED_RESOURCES_TO_RECOVERING)
      useful_required_resource <- ceiling(useful_kapacity_to_recover*REQUIRED_RESOURCES_TO_RECOVERING)
      total_used_resource <- c(rep(0,M+W))
      total_kapacity_recovered <- c(rep(0,M+W))
      for(n in 2:(N+1)){
        period_required_resource <- total_required_resource-total_used_resource
        useful_period_required_resource <- pmax(c(rep(0,M+W)),useful_required_resource-total_used_resource)
        period_used_resource <- c(rep(0,M+W))
        if(sum(period_required_resource)>0){
          if(isTRUE(sum(period_required_resource) <= INITIAL_RESOURCE+additional_resource)){
            recovery_rate[,n,l] <- total_kapacity_to_recover-total_kapacity_recovered
            total_kapacity_recovered <- total_kapacity_recovered + recovery_rate[,n,l]
            period_used_resource <- ceiling(recovery_rate[,n,l]*REQUIRED_RESOURCES_TO_RECOVERING[])
            break
          }else if(isTRUE(sum(useful_period_required_resource) <= INITIAL_RESOURCE + additional_resource)){
            j <- 0
            while(isTRUE(sum(period_used_resource) < INITIAL_RESOURCE + additional_resource)){
              available_resource <- INITIAL_RESOURCE+additional_resource-sum(period_used_resource)
              j <- j + 1
              if(IMPACT[j,l]>0){
                kapacity <- INITIAL_CAPACITIES[j] + additional_capacity[j] - total_kapacity_to_recover[j] + total_kapacity_recovered[j]
                required_resource <- ceiling((INITIAL_CAPACITIES[j]+additional_capacity[j]-kapacity)*REQUIRED_RESOURCES_TO_RECOVERING[j])
                useful_resource <- useful_period_required_resource[j]
                if(j==M+W){
                  recovery_rate[j,n,l] <- floor(available_resource/REQUIRED_RESOURCES_TO_RECOVERING[j])
                  period_used_resource[j] <- ceiling(recovery_rate[j,n,l]*REQUIRED_RESOURCES_TO_RECOVERING[j])
                }else{
                  if(sum(IMPACT[(j+1):(M+W),l])==0){
                    recovery_rate[j,n,l] <- floor(available_resource/REQUIRED_RESOURCES_TO_RECOVERING[j])
                    period_used_resource[j] <- ceiling(recovery_rate[j,n,l]*REQUIRED_RESOURCES_TO_RECOVERING[j])
                  }else{
                    additional_required_resource <- sum(period_required_resource[(j+1):(M+W)])
                    additional_useful_resource <- sum(useful_period_required_resource[(j+1):(M+W)])
                    random <- round(runif(1))
                    if(random==1){
                      period_used_resource[j] <- ceiling(runif(1,max(available_resource-additional_required_resource,useful_resource),min(required_resource,available_resource-additional_useful_resource)))
                    }else{
                      minimum <- round(runif(1))
                      if(minimum==1) period_used_resource[j] <- max(available_resource-additional_required_resource,useful_resource) else period_used_resource[j] <- min(required_resource,available_resource-additional_useful_resource)
                    }
                    recovery_rate[j,n,l] <- floor(period_used_resource[j]/REQUIRED_RESOURCES_TO_RECOVERING[j])
                  }
                }
              }
              total_kapacity_recovered[j] <- total_kapacity_recovered[j]+recovery_rate[j,n,l]
            }
          }else{
            j <- 0
            while(isTRUE(sum(period_used_resource) < INITIAL_RESOURCE+additional_resource)){
              available_resource <- INITIAL_RESOURCE+additional_resource-sum(period_used_resource)
              j <- j + 1
              if(IMPACT[j,l]>0){
                kapacity <- INITIAL_CAPACITIES[j] + additional_capacity[j] - total_kapacity_to_recover[j] + total_kapacity_recovered[j]
                if(j <= M) required_resource <- max(0,ceiling((sum(MW_sf[j,])-kapacity)*REQUIRED_RESOURCES_TO_RECOVERING[j])) else required_resource <- max(0,ceiling((sum(WC_sf[j-M,])-kapacity)*REQUIRED_RESOURCES_TO_RECOVERING[j]))
                if(j==M+W){
                  recovery_rate[j,n,l] <- floor(available_resource/REQUIRED_RESOURCES_TO_RECOVERING[j])
                  period_used_resource[j] <- ceiling(recovery_rate[j,n,l]*REQUIRED_RESOURCES_TO_RECOVERING[j])
                }else{
                  if(sum(IMPACT[(j+1):(M+W),l])==0){
                    recovery_rate[j,n,l] <- floor(available_resource/REQUIRED_RESOURCES_TO_RECOVERING[j])
                    period_used_resource[j] <- ceiling(recovery_rate[j,n,l]*REQUIRED_RESOURCES_TO_RECOVERING[j])
                  }else{
                    additional_required_resource <- sum(useful_period_required_resource[(j+1):(M+W)])
                    random <- round(runif(1))
                    if(random==1){
                      period_used_resource[j] <- ceiling(runif(1,max(available_resource-additional_required_resource,0),min(required_resource,available_resource)))
                    }else{
                      minimum <- round(runif(1))
                      if(minimum==1) period_used_resource[j] <- max(available_resource-additional_required_resource,0) else period_used_resource[j] <- min(required_resource,available_resource)
                    }
                    recovery_rate[j,n,l] <- floor(period_used_resource[j]/REQUIRED_RESOURCES_TO_RECOVERING[j])
                  }
                }
              }
              total_kapacity_recovered[j] <- total_kapacity_recovered[j]+recovery_rate[j,n,l]
            }          
          }
          total_used_resource <- total_used_resource + period_used_resource
        }
      }
    }
  }
  return(recovery_rate)
}

f_WC_gen <- function(open_facility,additional_capacity,lev,MW_sf,WC_sf,recovery_rate){
  flow <- array(0, dim=c(W,C,N+1,SCENARIO))
  consum_prior <- array(dim = c(C))
  consumer <- c(1:C)
  sup_prior <- array(dim = c(C,W))
  for(k in 1:C){
    pos <- ceiling(runif(1,0,length(consumer)))
    consum_prior[k] <- consumer[pos]
    consumer <- consumer[-pos]
    sup_prior[k,] <- order(WC_sf[,k], decreasing = TRUE)
    supply <- which(c(rep(0,W))==WC_sf[,k])
    cont <- W-length(supply)
    while(length(supply)>0){
      cont <- cont + 1
      pos <- ceiling(runif(1,0,length(supply)))
      j <- supply[pos]
      sup_prior[k,cont] <- j
      supply <- supply[-pos]
    }
  }
  opt_lev <- lev

  for(l in 1:SCENARIO){

    if (sum(IMPACT[,l])==sum(IMPACT[,l]*open_facility)){
      j2 <- which(c(rep(0,W))==IMPACT[(M+1):(M+W),l])
      j1 <- setdiff(c(1:W),j2)
      
      for(n in 1:(N+1)){
        flow[,,n,l] <- WC_sf[,]
        lev <- opt_lev
        capacity <- array(dim=c(M+W)); available_capacity <- array(dim=c(M+W))
        for(j in 1:(M+W)){
          capacity[j] <- (round((INITIAL_CAPACITIES[j]+additional_capacity[j])*(1-IMPACT[j,l])) + sum(recovery_rate[j,(1:n),l]))*open_facility[j]
          if(j <= M) available_capacity[j] <- capacity[j]-sum(MW_sf[j,]) else available_capacity[j] <- capacity[j]-sum(WC_sf[j-M,])
        }
        network_capacity <- min(sum(SUPPLIERS_CAPACITIES),sum(capacity[1:M]),sum(capacity[(M+1):(M+W)]))
        cont <- 0
        for(j in j1){
          if(open_facility[M+j]==1){
            cont <- cont + 1
            if(available_capacity[M+j] < 0){
              k2 <- which(c(rep(0,C))==WC_sf[j,])
              k1 <- setdiff(consum_prior, k2)
              flow[j,,n,l] <- c(rep(0,C))
              available_capacity[M+j] <- capacity[M+j]
              lev[k1] <- lev[k1] - WC_sf[j,k1]
              k<-0
              while(available_capacity[M+j] > 0){
                k<-k+1
                flow[j,k1[k],n,l] <- min(available_capacity[M+j],WC_sf[j,k1[k]])
                available_capacity[M+j] <- available_capacity[M+j] - flow[j,k1[k],n,l]
                lev[k1[k]] <- lev[k1[k]] + flow[j,k1[k],n,l]
              }
            }
          }
        }
        if(network_capacity >= sum(lev)){
          unpenalized_consumer <- which(c(rep(0,C))==(opt_lev-lev))
          penalized_consumer <- setdiff(consum_prior,unpenalized_consumer)
          while(sum(opt_lev)>sum(lev) && network_capacity>sum(lev)){
            k <- penalized_consumer[1]
            demand <- opt_lev[k] - sum(flow[,k,n,l])
            cont <- 0
            while(isTRUE(demand>0 && network_capacity>sum(lev))){
              cont <- cont + 1
              j <- sup_prior[k,cont]
              if(isTRUE(open_facility[M+j]==1)){
                add_flow <- min(available_capacity[M+j],demand,network_capacity-sum(lev))
                flow[j,k,n,l] <- flow[j,k,n,l] + add_flow
                available_capacity[M+j] <- available_capacity[M+j] - add_flow
                lev[k] <- lev[k] + add_flow
                demand <- demand - add_flow
              }
            }
            if(isTRUE(demand==0)) penalized_consumer <- penalized_consumer[-1] 
          }
        }else{
          cont_k <- 0
          while(network_capacity < sum(lev)){
            cont_k <- cont_k + 1
            k <- consum_prior[C+1-cont_k]
            cont_j <- 0
            while(lev[k] > 0 & network_capacity < sum(lev)){
              cont_j <- cont_j + 1
              j <- sup_prior[k,W+1-cont_j]
              if(flow[j,k,n,l] > 0){
                sub_flow <- min(flow[j,k,n,l],sum(lev)-network_capacity)
                flow[j,k,n,l] <- flow[j,k,n,l] - sub_flow
                available_capacity[M+j] <- available_capacity[M+j] + sub_flow
                lev[k] <- lev[k] - sub_flow
              }
            }
          }
        }
      }    
    }
  }
  return(flow)
}

f_MW_gen <- function(open_facility,additional_capacity,WC_f,sf,recovery_rate){
  flow <- array(0, dim = c(M,W,N+1,SCENARIO))
  consum_prior <- array(dim = c(W))
  consumer <- c(1:W)
  sup_prior <- array(dim = c(W,M))
  for(k in 1:W){
    pos <- ceiling(runif(1,0,length(consumer)))
    consum_prior[k] <- consumer[pos]
    consumer <- consumer[-pos]
    sup_prior[k,] <- order(sf[,k], decreasing = TRUE)
    supply <- which(c(rep(0,M))==sf[,k])
    cont <- M-length(supply)
    while(length(supply)>0){
      cont <- cont + 1
      pos <- ceiling(runif(1,0,length(supply)))
      j <- supply[pos]
      sup_prior[k,cont] <- j
      supply <- supply[-pos]
    }
  }

  for(l in 1:SCENARIO){

    if (sum(IMPACT[,l])==sum(IMPACT[,l]*open_facility)){
      j2 <- which(c(rep(0,M))==IMPACT[1:M,l])
      j1 <- setdiff(c(1:M),j2)
      
      for(n in 1:(N+1)){
        flow[,,n,l] <- sf[,]
        capacity <- array(dim=c(M+W)); available_capacity <- array(dim=c(M+W))
        for(j in 1:M){
          capacity[j] <- (round(sum(INITIAL_CAPACITIES[j]+additional_capacity[j])*(1-IMPACT[j,l])) + sum(recovery_rate[j,(1:n),l]))*open_facility[j]
          available_capacity[j] <- capacity[j]-sum(flow[j,,n,l])
        }
        cont <- 0
        for(j in j1){
          if(open_facility[j]==1){
            cont <- cont + 1
            if(available_capacity[j] < 0){
              k2 <- which(c(rep(0,W))==sf[j,])
              k1 <- setdiff(consum_prior,k2)
              flow[j,,n,l] <- c(rep(0,W)); available_capacity[j] <- capacity[j]
              k<-0
              while(available_capacity[j] > 0){
                k<-k+1
                flow[j,k1[k],n,l] <- min(available_capacity[j],sf[j,k1[k]])
                available_capacity[j] <- available_capacity[j] - flow[j,k1[k],n,l]
              }
            }
          }
        }
        for(k in consum_prior){
          if(sum(flow[,k,n,l]) > sum(WC_f[k,,n,l])){
            cont <- 0
            while (sum(flow[,k,n,l]) > sum(WC_f[k,,n,l])){
              cont <- cont + 1
              j <- sup_prior[k,M+1-cont]
              if(flow[j,k,n,l] > 0){
                sub_flow <- min(flow[j,k,n,l], sum(flow[,k,n,l]) - sum(WC_f[k,,n,l]))
                flow[j,k,n,l] <- flow[j,k,n,l] - sub_flow
                available_capacity[j] <- available_capacity[j] + sub_flow
              }
            }
          }
        }
        for(k in consum_prior){
          if(sum(flow[,k,n,l]) < sum(WC_f[k,,n,l])){
            cont <- 0
            demand <- sum(WC_f[k,,n,l]) - sum(flow[,k,n,l])
            while ((demand > 0)==TRUE){
              cont <- cont + 1
              j <- sup_prior[k,cont]
              if(open_facility[j]==1){
                add_flow <- min(available_capacity[j], demand)
                flow[j,k,n,l] <- flow[j,k,n,l] + add_flow
                available_capacity[j] <- available_capacity[j] - add_flow
                demand <- demand - add_flow
              }
            }
          }
        }
      }    
    }
  }
  return(flow)
}  

f_SM_gen <- function(open_facility,MW_f,sf){
  flow <- array(0, dim = c(S,M,N+1,SCENARIO))
  consum_prior <- array(dim = c(M))
  consumer <- c(1:M)
  sup_prior <- array(dim = c(M,S))
  for(k in 1:M){
    pos <- ceiling(runif(1,0,length(consumer)))
    consum_prior[k] <- consumer[pos]
    consumer <- consumer[-pos]
    sup_prior[k,] <- order(sf[,k], decreasing = TRUE)
    supply <- which(c(rep(0,S))==sf[,k])
    cont <- S-length(supply)
    while(length(supply)>0){
      cont <- cont + 1
      pos <- ceiling(runif(1,0,length(supply))); j <- supply[pos]
      sup_prior[k,cont] <- j
      supply <- supply[-pos]
    }
  }

  for(l in 1:SCENARIO){

    if (sum(IMPACT[,l])==sum(IMPACT[,l]*open_facility)){
      for(n in 1:(N+1)){
        flow[,,n,l] <- sf[,]
        for(k in consum_prior){
          if(sum(flow[,k,n,l]) > sum(MW_f[k,,n,l])){
            cont <- 0
            while (sum(flow[,k,n,l]) > sum(MW_f[k,,n,l])){
              cont <- cont + 1
              j <- sup_prior[k,S+1-cont]
              if(flow[j,k,n,l] > 0){
                sub_flow <- min(flow[j,k,n,l], sum(flow[,k,n,l]) - sum(MW_f[k,,n,l]))
                flow[j,k,n,l] <- flow[j,k,n,l] - sub_flow
              }
            }
          }
        }
        for(k in consum_prior){
          if(sum(flow[,k,n,l]) < sum(MW_f[k,,n,l])){
            cont <- 0
            demand <- sum(MW_f[k,,n,l]) - sum(flow[,k,n,l])
            while (demand > 0){
              cont <- cont + 1
              j <- sup_prior[k,cont]
              sup <- SUPPLIERS_CAPACITIES[j]-sum(flow[j,,n,l])
              add_flow <- min(sup, demand)
              flow[j,k,n,l] <- flow[j,k,n,l] + add_flow
              demand <- demand - add_flow         
            }
          }
        }
      }    
    }

  }
  return(flow)
}

for (i in 1:P){
  of[[i]] <- of_gen()
  ak[[i]] <- ak_gen(of[[i]])
  level[[i]] <- level_gen(of[[i]],ak[[i]])
  sf_WC[[i]] <- sf_WC_gen(of[[i]][(M+1):(M+W)],ak[[i]][(M+1):(M+W)],level[[i]])
  sf_MW[[i]] <- sf_MW_gen(of[[i]][1:M],ak[[i]][1:M],sf_WC[[i]])
  sf_SM[[i]] <- sf_SM_gen(sf_MW[[i]])
  ar[[i]] <- ar_gen(of[[i]],ak[[i]])
  rr[[i]] <- rr_gen(of[[i]],ak[[i]],ar[[i]],sf_MW[[i]],sf_WC[[i]])
  f_WC[[i]] <- f_WC_gen(of[[i]],ak[[i]],level[[i]],sf_MW[[i]],sf_WC[[i]],rr[[i]])
  f_MW[[i]] <- f_MW_gen(of[[i]],ak[[i]][1:M],f_WC[[i]],sf_MW[[i]],rr[[i]])
  f_SM[[i]] <- f_SM_gen(of[[i]],f_MW[[i]],sf_SM[[i]])
  fitness[[i]] <- CalculateFitness(of[[i]],ak[[i]],level[[i]],ar[[i]],sf_SM[[i]],sf_MW[[i]],sf_WC[[i]],rr[[i]],f_SM[[i]],f_MW[[i]],f_WC[[i]])
}
#----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#END