coverage = function(prop_boot, prop, pop, nsize, nboot, nsim){
  
  covmat  = matrix(NA, nsim, length(pop))
  cimat   = matrix(NA, nsim, length(pop))
  tvarmat = matrix(NA, nsim, length(pop))
  
  for(j in 1:nsim){
    
    fun_var     = function(prop_boot) (prop_boot*(1-prop_boot))/nsize                 
    res_var     = lapply(prop_boot[[j]], fun_var)                               # within variance
    tvarmat[j,] = colMeans(bind_rows(res_var)) +                                # total variance
      colVars(as.matrix(bind_rows(res_var))) +
      colVars(as.matrix(bind_rows(res_var)))/nboot
    
    ll = prop[[j]] - qt(.975, nsize-1) * sqrt(tvarmat[j,])                      # CI limits
    ul = prop[[j]] + qt(.975, nsize-1) * sqrt(tvarmat[j,])  
    
    covmat[j,] = ll < pop & pop < ul
    cimat[j,]  = ul - ll
    
  }
  
  tvar    = colMeans(tvarmat)
  cov     = colMeans(covmat)                                                    # average over nsim
  ciwidth = colMeans(cimat)
  
  return(list(totalvariance=tvar, coverage=cov, ci95width=ciwidth))
}