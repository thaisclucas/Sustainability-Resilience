#----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#START
GenerateChildren <- function(h1,h2,var){
  temp1 <- var[[h1]]
  temp2 <- var[[h2]]
  var[[h1]] <- temp2
  var[[h2]] <- temp1
  temp <- var
  return(temp)
}
#----------RESILIENT AND SUSTAINABLE OPERATIONS NETWORK DESIGN------------#END

'
GenerateChildren <- function(h1,h2,gen){
  chi1 <- individuals[[h1]]
  chi2 <- individuals[[h2]]
  chi1[gen] <- individuals[[h2]][gen]
  chi2[gen] <- individuals[[h1]][gen]
  fitChi1 <- CalculateFitness(chi1)
  fitChi2 <- CalculateFitness(chi2)
  
  #---Children replace parents---#
  chil = matrix(c(chi1, fitChi1, chi2, fitChi2), nrow=2, ncol=(DecVarNumber+ObjFunNumber), byrow=TRUE)
  
  #---elitist replacement (just for Monoobjective problems) ---#
  #source("R/ElitistReplacement.R")
  
  return(chil)
}
'