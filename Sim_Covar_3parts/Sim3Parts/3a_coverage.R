coverage = function(prop_boot, prop, pop, nsize, nboot, nsim){
  
  covmat = matrix(NA, nsim, length(pop))
  
  for(j in 1:nsim){
    
    fun_var = function(prop_boot) (prop_boot*(1-prop_boot))/nsize                 
    res_var = lapply(prop_boot[[j]], fun_var)                                   # within variance
    tot_var = colMeans(bind_rows(res_var)) +                                    # total variance
      colVars(bind_rows(res_var)) +
      colVars(bind_rows(res_var))/nboot
    
    ll = prop[[j]] - qt(.975, nsize-1) * sqrt(tot_var)                          # CI limits
    ul = prop[[j]] + qt(.975, nsize-1) * sqrt(tot_var)  
    
    covmat[j,] = ll < pop & pop < ul
    
  }

  cov = colMeans(covmat)                                                        # average over nsim
  return(cov)
}