#----------Weibul_Exponential_MLE------------#START
'CalculateFitness <- function(ind){
  ind_likelihood <- array(dim=length(test_data))
  likelihood <- 1
  a <- ind[1]; b <- ind[1]; betha <- ind[3]
  for(j in 1:test_data){
    ind_likelihood[j] <- (betha/(b*exp(a/test_data[j,2])))*((test_data[j,1]/(b*exp(a/test_data[j,2])))^(betha-1))*exp((test_data[j,1]/(b*exp(a/test_data[j,2])))^betha)
    likelihood <- likelihood*ind_likelihood[j]
  }
  return(likelihood)
}'
#----------Weibull_Exponential_MLE------------#START

#----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#START

CalculateFitness <- function(open_facility,add_capacity,lev,add_resource,SM_sf,MW_sf,WC_sf,rec_rate,SM_f,MW_f,WC_f){

  F2 <- 0; EIteta <- 0; Cteta <- 0; P0 <- 1; 
  for (l in 1:SCENARIO) {
    
    if (sum(IMPACT[,l]) == sum(IMPACT[,l]*open_facility)) {

      P0 <- P0 - PROB_SCENARIO[l]
    
      sum_unmet <- 0; levelPenalization <- 0; LM <- 0; handlingCost <- 0; lev_teta <- c(rep(0,C))
      for (k in 1:C) {
        unmet <- (N+1) * lev[k] - sum(WC_f[1:W,k,1:(N+1),l])
        sum_unmet <- sum_unmet + unmet
        levelPenalization <- levelPenalization + unmet * PENALIZATION_COSTS[k]
        lev_teta[k] <- sum(WC_f[1:W,k,1:(N+1),l])
      }
    
      F2 <- F2 + PROB_SCENARIO[l] * sum_unmet

      SM_TRANSPORT_EMISSIONS <- 0; MW_TRANSPORT_EMISSIONS <- 0; WC_TRANSPORT_EMISSIONS <- 0
      SM_TRANSPORT_COSTS <- 0; MW_TRANSPORT_COSTS <- 0; WC_TRANSPORT_COSTS <- 0
      SM_HANDLING_COSTS <- 0; MW_HANDLING_COSTS <- 0; WC_HANDLING_COSTS <- 0
      for(n in 1:(N+1)){
        SM_TRANSPORT_EMISSIONS <- SM_TRANSPORT_EMISSIONS + sum(TRANSPORT_EMISSIONS_SM[1:S,1:M] * SM_f[1:S,1:M,n,l])
        MW_TRANSPORT_EMISSIONS <- MW_TRANSPORT_EMISSIONS + sum(TRANSPORT_EMISSIONS_MW[1:M,1:W] * MW_f[1:M,1:W,n,l])
        WC_TRANSPORT_EMISSIONS <- WC_TRANSPORT_EMISSIONS + sum(TRANSPORT_EMISSIONS_WC[1:W,1:C] * WC_f[1:W,1:C,n,l]) 
        SM_TRANSPORT_COSTS <- SM_TRANSPORT_COSTS + sum(TRANSPORT_COSTS_SM[1:S,1:M] * MW_f[1:S,1:M,n,l]) 
        MW_TRANSPORT_COSTS <- MW_TRANSPORT_COSTS + sum(TRANSPORT_COSTS_MW[1:M,1:W] * MW_f[1:M,1:W,n,l]) 
        WC_TRANSPORT_COSTS <- WC_TRANSPORT_COSTS + sum(TRANSPORT_COSTS_WC[1:W,1:C] * WC_f[1:W,1:C,n,l])
        SM_HANDLING_COSTS <- SM_HANDLING_COSTS + sum(HANDLING_COSTS_SM[1:S,1:M] * SM_f[1:S,1:M,n,l]) 
        MW_HANDLING_COSTS <- MW_HANDLING_COSTS + sum(HANDLING_COSTS_MW[1:M,1:W] * MW_f[1:M,1:W,n,l]) 
        WC_HANDLING_COSTS <- WC_HANDLING_COSTS + sum(HANDLING_COSTS_WC[1:W,1:C] * WC_f[1:W,1:C,n,l])
      }
    
      EIteta <- EIteta + (PROB_SCENARIO[l]/(N+1)) * (sum(FOOTPRINT_EMISSIONS[1:S] * SM_f[1:S,1:M,1:(N+1),l]) + sum(MANUFACTURING_EMISSIONS[1:M] * MW_f[1:M,1:W,1:(N+1),l]) + SM_TRANSPORT_EMISSIONS + MW_TRANSPORT_EMISSIONS + WC_TRANSPORT_EMISSIONS)
      Cteta <- Cteta + (PROB_SCENARIO[l]/(N+1)) * (sum(REVENUES*lev_teta)-(sum(PURCHASE_COSTS[1:S] * SM_f[1:S,1:M,1:(N+1),l]) + sum(MANUFACTURING_COSTS[1:M] * MW_f[1:M,1:W,1:(N+1),l]) + SM_TRANSPORT_COSTS + MW_TRANSPORT_COSTS + WC_TRANSPORT_COSTS + SM_HANDLING_COSTS + MW_HANDLING_COSTS + WC_HANDLING_COSTS + levelPenalization + sum(REQUIRED_RESOURCES_TO_RECOVERING[1:(M+W)]*rec_rate[1:(M+W),1:(N+1),l])*USED_RESOURCE_COST))
    }
      
  }
  
  C0 <- P0 *(sum(REVENUES * lev)-(sum(PURCHASE_COSTS[1:S] * SM_sf[1:S,1:M]) + sum(MANUFACTURING_COSTS[1:M] * MW_sf[1:M,1:W]) + sum(TRANSPORT_COSTS_SM[1:S,1:M] * SM_sf[1:S,1:M]) + sum(TRANSPORT_COSTS_MW[1:M,1:W] * MW_sf[1:M,1:W]) + sum(TRANSPORT_COSTS_WC[1:W,1:C] * WC_sf[1:W,1:C]) + sum(HANDLING_COSTS_SM[1:S,1:M] * SM_sf[1:S,1:M]) + sum(HANDLING_COSTS_MW[1:M,1:W] * MW_sf[1:M,1:W]) + sum(HANDLING_COSTS_WC[1:W,1:C] * WC_sf[1:W,1:C])))
  EI0 <- P0 * (sum(FOOTPRINT_EMISSIONS[1:S] * SM_sf[1:S,1:M]) + sum(MANUFACTURING_EMISSIONS[1:M] * MW_sf[1:M,1:W]) + sum(TRANSPORT_EMISSIONS_SM[1:S,1:M] * SM_sf[1:S,1:M]) + sum(TRANSPORT_EMISSIONS_MW[1:M,1:W] * MW_sf[1:M,1:W]) + sum(TRANSPORT_EMISSIONS_WC[1:W,1:C] * WC_sf[1:W,1:C]))
  
  F1 <- 0
  F3 <- C0 + Cteta - sum(FIXED_COSTS[1:(M+W)] * open_facility[1:(M+W)]) - MAINTAINED_RESOURCE_COST * (INITIAL_RESOURCE + add_resource)
  F4 <- EI0 + EIteta

  LJ <- 0; LA <- 0; LD <- 0
  for (z in ZONES) {
    LJ_z <- 0; LA_z <- 0; LD_z <- 0
    for (j in 1:S) {
      if (SUPPLIERS_ZONES[j]==z) {
        LJ_z <- LJ_z + LJR_S[j] * sum(SM_sf[j,])
        LA_z <- LA_z + sum(SM_sf[j,])
      }
    }
    for(j in 1:M){
      if (INSTALLATIONS_ZONES[j]==z) {
        LJ_z <- LJ_z + LJR_M[j] * sum(MW_sf[j,])
        LA_z <- LA_z + sum(MW_sf[j,])
      }
      if(INSTALLATIONS_DATA$Classification[j] == "New") F1 <- F1 + OF_INVESTMENTS[j] * open_facility[j]
    }
    for(j in 1:W){
      if (INSTALLATIONS_ZONES[M+j]==z) {
        LJ_z <- LJ_z + LJR_W[j] * sum(WC_sf[j,])
        LA_z <- LA_z + sum(WC_sf[j,])
      }
    # if(INSTALLATIONS_DATA$Classification[j] == "New") F1 <- F1 + OF_INVESTMENTS[j] * open_facility[j] // PARA SOMAR O CUSTO DE ABRIR W5 
      if(INSTALLATIONS_DATA$Classification[M + j] == "New") F1 <- F1 + OF_INVESTMENTS[j] * open_facility[j]
    }
    for(j in 1:C){
      if (CUSTOMERS_ZONES[j]==z) LD_z <- LD_z + lev[j]/DEMANDS[j]
    }
    LJ <- LJ + UR[z] * LJ_z / (sum(LJR_S[1:S] * SM_sf[1:S,1:M]) + sum(LJR_M[1:M] * MW_sf[1:M,1:W]) + sum(LJR_W[1:W] * WC_sf[1:W,1:C]))
    LA <- LA + (1-GDPI[z]) * LA_z/(sum(SM_sf[1:S,1:M]) + sum(MW_sf[1:M,1:W]) + sum(WC_sf[1:W,1:C]))
    LD <- LD + LD_z * (1-Y[z])
  }
  
  F1 <- F1 + sum(AC_INVESTMENTS[1:(M+W)] * add_capacity[1:(M+W)]) + AR_INVESTMENT * add_resource
  F5 <- LA + LJ + LD 
  
  functions <- c(-F1,-F2,F3,-F4, F5)
  return(functions) #caso multi
  #return(F5) #caso monoobjetivo
}

#----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#END

'
#----------REPLACEMENT POLICY FOR SYSTEMS SUBJECTED TO IMPERFECT REPAIR------------#START
CalculateFitness <- function(ind){
  #---variables to DES---#
  Ns <- 1000
  total_cycles <- 0
  total_cycles_corrective <- 0
  total_failures <- 0
  total_failures_corrective_cycle <- 0
  total_tc <- 0
  total_residue <- 0
  p <- 0

  #----------------------------------------------------Simulation - Start#
  for (ns in 1:Ns){
    time <- 0
    N <- 0
    Nc <- 0
    tc <- 0
    horizon_failures <- 0
    horizon_failures_corrective_cycle <- 0
    horizon_residue <- 0
    
    #----------------------------------------------------Horizon - Start#
    while (time < H){
      failures <- 0
      cycle <- 0
      life <- 0
      inative <- 0
      residue <- 0
      end <- 0
      cycle_corrective <- 0
      
      #Generate time to critical failure: fc
      u <- runif(1)
      fc <- alpha1*((-log(1-u))^(1/beta1))
      
      #----------------------------------------------------cycle - Start#
      while (end == 0){
        #Generate time to non-critical failure: f
        u <- runif(1)
        if (failures==0){
          f <- alpha*((-log(1-u))^(1/beta))
        }else{
          #a <- (q/alpha)*(life-inative)
          #b <- a^beta
          #f <- alpha*(b-log(1-u))^(1/beta))-q*(life-inative)
          a <- (q*(life-inative))^beta
          b <- (alpha^beta)*(-log(1-u))
          f <- (a+b)^(1/beta)-q*(life-inative)
        }
        #-------------------------
        life <- life + f
        if ((life < ind[1]) & ((life-inative)<fc)){
          failures <- failures + 1
          #generate time to repair: r
          u <- runif(1)
          r <- r_time*(-log(1-u))
          #------------------
          life <- life + r
          inative <- inative + r
          if (life >= ind[1]){
            #generate time to preventive replacement: rp
            u <- runif(1)
            if (N>ind[2]) rp <- p2_time*(-log(1-u))
            else rp <- p1_time*(-log(1-u))
            #--------------------------
            cycle <- ind[1] + rp
            inative <- inative - (life - ind[1]) + rp
            residue <- fc + inative - ind[1] - rp
            end <- 1
          }
        }else{
          if(life >= ind[1]){
            if((ind[1])<=(fc+inative)){
              #generate time to preventive replacement: rp
              u <- runif(1)
              if (N>ind[2]) rp <- p2_time*(-log(1-u))
              else rp <- p1_time*(-log(1-u))
              #--------------------------
              cycle <- ind[1] + rp
              inative <- inative + rp
              residue <- fc + inative - ind[1]
              end <- 1
            }else{
              #generate time to corrective replacement: rc
              u <- runif(1)
              if (N>ind[2]) rc <- c2_time*(-log(1-u))
              else rc <- c1_time*(-log(1-u))
              #--------------------------
              cycle <- fc + inative + rc
              inative <- inative + rc
              tc <- fc
              end <- 1
              cycle_corrective <- 1
            }
          }else{
            #generate time to corrective replacement: rc
            u <- runif(1)
            if (N>ind[2]) rc <- c2_time*(-log(1-u))
            else rc <- c1_time*(-log(1-u))
            #--------------------------
            cycle <- fc+inative + rc
            inative <- inative + rc
            end <- 1
            cycle_corrective <- 1
          }
        }
      }
      #----------------------------------------------------cycle - End#
    
      time <- time + cycle
      N <- N + 1
      horizon_failures <- horizon_failures + failures
      if (cycle_corrective == 1){
        Nc <- Nc + 1
        horizon_failures_corrective_cycle <- horizon_failures_corrective_cycle + failures
        tc <- tc + fc
      }
      horizon_residue <- horizon_residue + residue
    }
    #----------------------------------------------------Horizon - End#
    
    total_cycles <- total_cycles + N
    total_cycles_corrective <- total_cycles_corrective + Nc
    total_failures <- total_failures + horizon_failures
    total_failures_corrective_cycle <- total_failures_corrective_cycle + horizon_failures_corrective_cycle
    total_tc <- total_tc + tc
    total_residue <- total_residue + horizon_residue
    if (N>ind[2]) p <- p + (N-ind[2])/N
  }
  #----------------------------------------------------Simulation - End#
  
  #---Parameters estimation via DES---#
  prob_corrective_cycle <- total_cycles_corrective/total_cycles
  prob_preventive_cycle <- 1 - prob_corrective_cycle
  prob_est <- 1 - (p/Ns)
  if (total_cycles_corrective > 0){
    expected_failures_corrective_cycle <- total_failures_corrective_cycle/total_cycles_corrective
    expected_tc <- total_tc/total_cycles_corrective
  }else{
    expected_failures_corrective_cycle <- 0
    expected_tc <- 0
  }
  if (total_cycles_corrective == total_cycles){
    expected_failures_preventive_cycle <- 0
    expected_residue <- 0
  }else{
    expected_failures_preventive_cycle <- (total_failures - total_failures_corrective_cycle)/(total_cycles - total_cycles_corrective)
    expected_residue <- total_residue/(total_cycles-total_cycles_corrective)
  }

  #---fitness calculating---#
  #the effect of the probability of having spare parts#
  c_cost <- c1_cost*prob_est + c2_cost*(1 - prob_est)
  p_cost <- p1_cost*prob_est + p2_cost*(1 - prob_est)
  c_time <- c1_time*prob_est + c2_time*(1 - prob_est)
  p_time <- p1_time*prob_est + p2_time*(1 - prob_est)
  
  #the imperfect repairs costs#
  r1_cost <- (expected_failures_preventive_cycle^1.2)*7000
  r2_cost <- (expected_failures_corrective_cycle^1.2)*7000
  
  #the expected cycle time#
  expected_cycle <- (ind[1] + p_time)*prob_preventive_cycle + (expected_tc + c_time)*prob_corrective_cycle

  objective_cost <- ((r1_cost + p_cost)*prob_preventive_cycle + (r2_cost + c_cost)*prob_corrective_cycle)/expected_cycle
  #objective_failures <- expected_failures_preventive_cycle*prob_preventive_cycle + (expected_failures_corrective_cycle + 1)*prob_corrective_cycle
  objective_failures <- (expected_failures_preventive_cycle*prob_preventive_cycle + (expected_failures_corrective_cycle + 1)*prob_corrective_cycle)/expected_cycle
  #objective_residue <- expected_residue*prob_preventive_cycle
  objective_unavailability <- ((p_time + expected_failures_preventive_cycle*r_time)*prob_preventive_cycle + (c_time + expected_failures_corrective_cycle*r_time)*prob_corrective_cycle)/expected_cycle
  Objective_investment <- ind[2]*s_cost + g_cost
  
  penalty <- 0
  #pen1 <- objective_unavailability*100 - u_max*100
  #pen2 <- as.integer(H/(ind[1]+p1_time))*100 - ind[2]*100
  #if(pen1>0){
    #penalty <- penalty + 10*(pen1^2)
  #}
  #if(pen2>0){
    #penalty <- penalty + pen2^2
  #}
  pen1 <- objective_unavailability - u_max
  pen2 <- as.integer(H/(ind[1]+p1_time)) - ind[2]
  if(pen1>0 | pen2>0){
    penalty <- penalty + 100000
  }

  f1 <- -(objective_cost) - penalty
  f2 <- -(objective_failures) - penalty
  f3 <- -(objective_unavailability) - penalty
  f4 <- -(Objective_investment) - penalty

  #f1 <- round(f1,2)
  #f2 <- round(f2,1)
  #f3 <- round(f3,3)
  
  fitness <- c(f1,f2,f3,f4)
    
  return(fitness)
}
#----------REPLACEMENT POLICY FOR SYSTEMS SUBJECTED TO IMPERFECT REPAIR------------#END
'