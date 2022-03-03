covariates = function(impdata_dum, prop_boot, prop, pop, nsize, nboot, nsim){
  
  covmat = matrix(NA, nsim, length(pop))
  covarmat_imp_boot = array(NA, dim=c(3, 4,nboot))
  covarmat_true_boot = array(NA, dim=c(3, 4, nboot))
  covarmat_imp = array(NA, dim=c(3, 4,nsim))
  covarmat_true = array(NA, dim=c(3, 4,nsim))
  
  for(j in 1:nsim){
    
    for (k in 1:nboot) {
      covardata = impdata_dum[[j]][[k]]
          

      }
    
    
    #covarmat_imp_var <- 
    #covarmat_true_var <- 
    # res_var     = lapply(prop_boot[[j]], fun_var)                               # within variance
    #tvarmat[j,] = colMeans(bind_rows(res_var)) +                                # total variance
    #  colVars(as.matrix(bind_rows(res_var))) +
    #  colVars(as.matrix(bind_rows(res_var)))/nboot


  }  
  covarmat_imp_res = apply(covarmat_imp, MARGIN = c(1, 2), FUN = mean)          # average over nsim
  covarmat_true_res = apply(covarmat_true, MARGIN = c(1, 2), FUN = mean)        # average over nsim
  
  return(covarmat_imp_res)
}


