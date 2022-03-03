library(poLCA) 
library(confreq) 
library(dplyr) 

options(scipen = 999)

nsim   = 10
nsize  = 5000 
nboot  = 5
nconds = 8

set.seed(123)
load("simbootdat.RData")

source("2a_LCmodel.R")                                                   
                                                      
impdats <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)

init=Sys.time()
pb <- txtProgressBar(min = 0, max = nconds, style = 3, width = 50,  char = "=")   

for(i in 1:nconds){                                                             # loop over conditions
 message(paste("\nStart with condition" ,i))
  for(j in 1:nsim){                                                             # loop over simulation iterations
    
    for(k in 1:nboot){ 
      log <- capture.output({                                                   #make sure not all messages are displayed
              impdats[[i]][[j]][[k]] = posteriors(bootdat = simbootdat[[i]][[j]],
                                          k = k)
      })
      for(l in 1: nsize){                                                       # impute
        impdats[[i]][[j]][[k]][l,"imp"] = which(rmultinom(n  = 1, size = 1,
                      prob=impdats[[i]][[j]][[k]][l,c("1","2","3","4")]) == 1)        
      }
    }
    
  }
  save(impdats, file = "impdat5.RData")
  setTxtProgressBar(pb, i)

}

entropy <- c()
for (i in 1:8) {
  entropy[i] <- impdats[[i]][[1]][[1]]$entr
}
entropy
write.csv(entropy, file="ent.csv", sep=";")

SimulationTotalTime = Sys.time()-init
SimulationTotalTime

#impdats
