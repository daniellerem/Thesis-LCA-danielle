covariates = function(impdata_dum, prop_boot, prop, pop, nsize, nboot, nsim){
  
  covmat = matrix(NA, nsim, length(pop))
  covarmat_imp_boot = array(NA, dim=c(3, 4,nboot))
  covarmat_true_boot = array(NA, dim=c(3, 4, nboot))
  covarmat_imp = array(NA, dim=c(3, 4,nsim))
  covarmat_true = array(NA, dim=c(3, 4,nsim))
  
  for(j in 1:nsim){
    
    for (k in 1:nboot) {
      covardata = impdata_dum[[j]][[k]]
      
      log <- capture.output({ #make sure not all messages are displayed
       covarmat_imp_boot[, , k] = summary(multinom(imp~cat_two+cat_three+cat_four, data = covardata))$coefficients 
      })
      log <- capture.output({ #make sure not all messages are displayed
        covarmat_true_boot[, , k] = summary(multinom(trueclass~cat_two+cat_three+cat_four, data = covardata))$coefficients 
      })
      }
    
    covarmat_imp[, , j] <-  apply(covarmat_imp_boot, MARGIN = c(1, 2), FUN = mean)
    covarmat_true[, , j] <-  apply(covarmat_true_boot, MARGIN = c(1, 2), FUN = mean)
    
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


listimp=array(NA, dim=c(3, 4, 5))
for (conds in 1:nconds) {
  log <- capture.output({ #make sure not all messages are displayed
  tryimps = impdats[[conds]][[6]][[4]]
  listimp[, , conds]=summary(multinom(imp~Y5, data=tryimps))$coefficients
  listtrue[[conds]]=summary(multinom(trueclass~Y5, data=tryimps))$coefficients
  })  
  }
listimp
apply(listimp, MARGIN = c(1, 2), FUN = mean)

tryimps = impdats[[7]][[6]][[4]]




multinom(imp~Y5, data = tryimps)$coefficients   
multi_mo = multinom(imp~Y5, data = tryimps)   
#logit(p2//p1), logit(p3/p1), logit(p4/p1)
#Wald 95% confidence interval is exp(est+-1.96*se)

#make dummies of Y5

z <- summary(multi_mo)$coefficients/summary(multi_mo)$standard.errors #Wald z
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2  # 2-tailed z test
p

exp(coef(multi_mo))

#alternative: multiple logistic regression, one for each pair of outcomes

