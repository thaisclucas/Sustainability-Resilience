ind <- matrix(c(individuals[[h1]],individuals[[h2]],chi1,chi2),nrow=4,ncol=DecVarNumber, byrow=TRUE)
fit <- c(fitness[[h1]],fitness[[h1]],fitChi1,fitChi2)
index <- order(fit, decreasing = TRUE)
first <- index[1]
second <- index[2]
chil <- matrix(c(ind[first,], fit[first], ind[second,], fit[second]), nrow=2, ncol=(DecVarNumber+ObjFunNumber), byrow=TRUE)