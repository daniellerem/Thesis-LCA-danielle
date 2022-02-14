library(poLCA) 
library(confreq) 
library(dplyr) 
library(resample)

options(scipen = 999)

nsim   = 10
nsize  = 5000 
nboot  = 5
nconds = 2

set.seed(123)

source("3a_coverage.R")
load("impdat.RData")
load("simbootdat.RData")

pop = c(0.15, 0.34, 0.2975, 0.2125)

prop_x_boot = rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
prop_x      = rep(list(vector("list", nsim)), nconds)
cov_x       = vector("list", nconds)


for(i in 1:nconds){                                                             # loop over conditions
  cat(i)
  for(j in 1:nsim){                                                             # loop over simulation iterations
    
    for(k in 1:nboot){ 
      
      prop_x_boot[[i]][[j]][[k]] = prop.table(table(impdats[[i]][[j]][[k]]$imp))
      
    }
    
    prop_x[[i]][[j]] = colMeans(bind_rows(prop_x_boot[[i]][[j]]))               # class proportions

  }
  
  cov_x[[i]]  = coverage(prop_boot = prop_x_boot[[i]],                          # coverage
                              prop  = prop_x[[i]],
                              pop   = pop,
                              nsize = nsize,
                              nboot = nboot,
                              nsim  = nsim)               
}
