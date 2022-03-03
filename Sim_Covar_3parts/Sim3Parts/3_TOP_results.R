library(poLCA) 
library(confreq) 
library(dplyr) 
library(resample)
library(nnet) #needed for multinomial regression

options(scipen = 999)

nsim   = 10
nsize  = 5000 
nboot  = 5
nconds = 8

set.seed(123)

source("3a_coverage.R")
source("3b_covariates.R")

load("simbootdat.RData")
load("impdat5.RData")
pop = c(0.15, 0.34, 0.2975, 0.2125)

prop_x_boot = rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
entr_boot   = rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
entr_nsim   = rep(list(vector("list", nsim)), nconds)
prop_x_nsim = rep(list(vector("list", nsim)), nconds)
bias_x_nsim = rep(list(vector("list", nsim)), nconds)
bias_x      = vector("list", nconds)
cov_x       = vector("list", nconds)
rmse_x      = vector("list", nconds)
sesd_x      = vector("list", nconds)
ciwidth     = vector("list", nconds)
entr        = vector("list", nconds)

#covariates
covar_boot       = rep(list(rep(list(array(NA,dim = c(4,4,nboot), dimnames=NULL)), nsim)), nconds)
covar_boot_true  = rep(list(rep(list(array(NA,dim = c(4,4,nboot), dimnames=NULL)), nsim)), nconds)
covar_nsim       = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_true  = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_res        = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)
covar_res_true   = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)
covar_bias       = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)

for(i in 1:nconds){                                                             # loop over conditions
  cat(i)
  for(j in 1:nsim){                                                             # loop over simulation iterations
    
    for(k in 1:nboot){ 
      
      prop_x_boot[[i]][[j]][[k]] = prop.table(table(impdats[[i]][[j]][[k]]$imp))
      entr_boot[[i]][[j]][[k]]= median(impdats[[i]][[j]][[k]]$entr)
      #covariates relationships
      covar_boot[[i]][[j]][] = prop.table(table(impdats[[i]][[j]][[k]]$imp, impdats[[i]][[j]][[k]]$Y5))
      covar_boot_true[[i]][[j]][] = prop.table(table(impdats[[i]][[j]][[k]]$trueclass, impdats[[i]][[j]][[k]]$Y5))
      
      
    }
    
    prop_x_nsim[[i]][[j]] = colMeans(bind_rows(prop_x_boot[[i]][[j]]))          # class proportions per iteration
    bias_x_nsim[[i]][[j]] = abs(prop_x_nsim[[i]][[j]] - pop)                    # absolute bias per iteration
    entr_nsim[[i]][[j]] = mean(unlist(entr_boot[[i]][[j]]))          
    covar_nsim[[i]][]   = apply(covar_boot[[i]][[j]], MARGIN = c(1, 2), FUN = mean)     # average over bootstraps
    covar_nsim_true[[i]][]  = apply(covar_boot_true[[i]][[j]], MARGIN = c(1, 2), FUN = mean)     # average over bootstraps
    
  }
  
  #summarize results over simulations
  covar_res[[i]]      = apply(covar_nsim[[i]], MARGIN = c(1, 2), FUN = mean)     # average over nsim
  covar_res_true[[i]] = apply(covar_nsim_true[[i]], MARGIN = c(1, 2), FUN = mean)     # average over nsim
  covar_bias[[i]]     = abs(covar_res_true[[i]]-covar_res[[i]])

  entr[[i]] = mean(unlist(entr_nsim[[i]]))                               # entropy per simulation condition 
  bias_x[[i]] = colMeans(bind_rows(bias_x_nsim[[i]]))                           # average absolute bias 
  cov_x[[i]]  = coverage(prop_boot = prop_x_boot[[i]],                          # total variance, coverage and ciwidth
                              prop  = prop_x_nsim[[i]],
                              pop   = pop,
                              nsize = nsize,
                              nboot = nboot,
                              nsim  = nsim)   
  #how to access the 3 different parts
  cov_x[[i]][[1]]#total variance
  ciwidth[[i]] <-    cov_x[[i]]$totalvariance
  cov_x[[i]][[2]]#coverage
    cov_x[[i]]$coverage
  cov_x[[i]][[3]]#ci95width
  cov_x[[i]]$ci95width
  
  
  
  rmse_x[[i]] = sqrt(colMeans(bind_rows(lapply(bias_x_nsim[[i]],                # rmse
                                               function(x) x^2))))
  
  sesd_x[[i]] = sqrt(cov_x[[i]][[1]][1:4]) / 
    apply(bind_rows(prop_x_nsim[[i]]), 2, sd)                                   # se/sd
  

  
  
}
#write.table(x= cov_x, file = "results28feb.csv", append=T)
#write.table(x=c(bias_x), file = "bias.csv",  sep = ";")
#write.table(x=c(covar_bias), file = "covbias.csv",append=T,  sep = ";")

write.table(x=c(entr), file = "entropy.csv",append=T,  sep = ";")

