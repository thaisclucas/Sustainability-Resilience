'for(i in 1:length(aux_fitness)){
  identical()
}'

of_name <- str_c("Results/of",nr,".csv")
ak_name <- str_c("Results/ak",nr,".csv")
level_name <- str_c("Results/level",nr,".csv")
sf_WC_name <- str_c("Results/sf_WC",nr,".csv")
sf_MW_name <- str_c("Results/sf_MW",nr,".csv")
sf_SM_name <- str_c("Results/sf_SM",nr,".csv")
ar_name <- str_c("Results/ar",nr,".csv")
rr_name <- str_c("Results/rr",nr,".csv")
f_WC_name <- str_c("Results/f_WC",nr,".csv")
f_MW_name <- str_c("Results/f_MW",nr,".csv")
f_SM_name <- str_c("Results/f_SM",nr,".csv")
fitness_name <- str_c("Results/fitness",nr,".csv")

write.table(aux_of, of_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_ak, ak_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_level, level_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_sf_WC, sf_WC_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_sf_MW, sf_MW_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_sf_SM, sf_SM_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_ar, ar_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_rr, rr_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_f_WC, f_WC_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_f_MW, f_MW_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_f_SM, f_SM_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_fitness, fitness_name, row.names = F, col.names = T, sep = ";", dec = ".")

'
pareto_solutions_name <- str_c("Results/pareto_solutions",nr,".csv")
fitness_name <- str_c("Results/fitness",nr,".csv")

write.table(aux_individuals, pareto_solutions_name, row.names = F, col.names = T, sep = ";", dec = ".")
write.table(aux_fitness, fitness_name, row.names = F, col.names = T, sep = ";", dec = ".")

checker <- c(rep(0,DecVarNumber))
dif <- c(166,2)

nj <- 0
for(j1 in 1:length(aux_individuals[,1])){
  for(j2 in 1:length(solutions[,1])){
    for(k in 1:DecVarNumber){
      checker[k] <- 0
      #if (aux_individuals[j1,k]==solutions[j2,k]) checker[k] <- 1
      
      if (k==1){
        if ((aux_individuals[j1,k]+dif[1])>=(solutions[j2,k]-dif[1]) & (aux_individuals[j1,k]-dif[1])<=(solutions[j2,k]+dif[1])) checker[k] <- 1
      }else{
      #  if (aux_individuals[j1,k]==solutions[j2,k]) checker[k] <- 1
        if ((aux_individuals[j1,k]+dif[2])>=(solutions[j2,k]-dif[2]) & (aux_individuals[j1,k]-dif[2])<=(solutions[j2,k]+dif[2])) checker[k] <- 1
      }
 
    }
    if (sum(checker)==DecVarNumber){
      nj <- nj + 1
      break
    }
  }
}
'

'
#------------GRP PARAMETERS ESTIMATION PROBLEM--------------#START
b[order_ind[1]]
q[order_ind[1]]
a[order_ind[1]]
f[order_ind[1]]
#------------GRP PARAMETERS ESTIMATION PROBLEM--------------#END
'