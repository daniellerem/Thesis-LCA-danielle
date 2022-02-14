library(poLCA) 
library(confreq) 
library(dplyr) 

options(scipen = 999)

nsim   = 10
nsize  = 5000 
nboot  = 5
nconds = 2

set.seed(123)
load("simbootdat.RData")

source("2a_LCmodel.R")                                                   
                                                      
impdats <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)

init=Sys.time()

for(i in 1:nconds){                                                             # loop over conditions
  cat(i)
  for(j in 1:nsim){                                                             # loop over simulation iterations
    
    for(k in 1:nboot){ 
      
      impdats[[i]][[j]][[k]] = posteriors(bootdat = simbootdat[[i]][[j]],
                                          k = k)
      
      for(l in 1: nsize){                                                       # impute
        impdats[[i]][[j]][[k]][l,"imp"] = which(rmultinom(n  = 1, size = 1,
                      prob=impdats[[i]][[j]][[k]][l,c("1","2","3","4")]) == 1)        
      }
    }
  }
  save(impdats, file = "impdat.RData")
  
}

SimulationTotalTime = Sys.time()-init
SimulationTotalTime
