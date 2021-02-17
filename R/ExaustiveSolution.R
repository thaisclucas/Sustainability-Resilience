comb <- length(pop[,1])
limit <- matrix(0,nrow=DecVarNumber,ncol=2)
for (k in 1:DecVarNumber){
  limit[k,] <- DefineVariablesBound(k)
  comb <- comb*(limit[k,2]-limit[k,1]+1)
}

fitnessExaust <- matrix(0,nrow=comb,ncol=ObjFunNumber)
individualExaust <- matrix(0,nrow=comb,ncol=DecVarNumber)

#(interval and spare parts problem - paper 2016)#
count = 0
for (j1 in limit[1,1]:limit[1,2]){
  for (j2 in limit[2,1]:limit[2,2]){
    count <- count + 1
    individualExaust[count,] <- c(j1,j2)
    fitnessExaust[count,] <- CalculateFitness(individualExaust[count,])
  }
}
#------------------------------------------------#

for(j1 in 1:(comb-1)){
  for(j2 in (j1+1):comb){
    if (is.na(individualExaust[j1,1])==FALSE & is.na(individualExaust[j2,1])==FALSE){
      dom_1 <- c(rep(0,ObjFunNumber))
      dom_2 <- c(rep(0,ObjFunNumber))
      for (o in 1:ObjFunNumber){
        if(fitnessExaust[j1,o]>fitnessExaust[j2,o]){
          dom_1[o]<-1
        }else{
          if (fitnessExaust[j1,o]<fitnessExaust[j2,o]) dom_2[o]<-1
        }
      }
      if (sum(dom_1)>0 & sum(dom_2)==0){
        individualExaust[j2,] <- NA
        fitnessExaust[j2,] <- NA
      }
      if (sum(dom_1)==0 & sum(dom_2)>0){
        individualExaust[j1,] <- NA
        fitnessExaust[j1,] <- NA
      }
    }
  }
}

for (j in 60533:comb){
  c <- c(pop[j,1],pop[j,2])
  fitnessExaust[j,] <- CalculateFitness(c)
}
#------------------------------------------------#
dec<-c(2,4,3,0)

for(j1 in 1:(comb-1)){
  if (is.na(pop[j1,1])==FALSE){
    for(j2 in (j1+1):comb){
      if (is.na(pop[j2,1])==FALSE){
        dom_1 <- c(rep(0,ObjFunNumber))
        dom_2 <- c(rep(0,ObjFunNumber))
        for (o in 1:ObjFunNumber){
          fit1<-round(fitnessExaust[j1,o],dec[o])
          fit2<-round(fitnessExaust[j2,o],dec[o])
          if(fit1>fit2){
            dom_1[o]<-1
          }else{
            if (fit1<fit2) dom_2[o]<-1
          }
        }
        if (sum(dom_1)>0 & sum(dom_2)==0){
          pop[j2,] <- NA
          fitnessExaust[j2,] <- NA
        }
        if (sum(dom_1)==0 & sum(dom_2)>0){
          pop[j1,] <- NA
          fitnessExaust[j1,] <- NA
          break
        }
      }
    }
  }
}


#-----------------------------------------------------#
write.table(individualExaust, "C:/Users/Carol/Desktop/MultiobjectiveGA_2015.08.04/Results/Exact_nondominated_solutions.csv", row.names = F, col.names = T, sep = ";", dec = ",")
write.table(fitnessExaust, "C:/Users/Carol/Desktop/MultiobjectiveGA_2015.08.04/Results/Exact_Pareto_front.csv", row.names = F, col.names = T, sep = ";", dec = ",")