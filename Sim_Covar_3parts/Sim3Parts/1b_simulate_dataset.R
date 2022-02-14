simdata = function(nsize,
                    sel_error,
                    meas_error
){
  
  mod1 = poLCA.simdata(N       = nsize,                                         # generate SE part
                       nclass  = 2,
                       probs   = sel_error, 
                       P       = c(0.15, 0.85), 
                       missval = F)
  
  df1 = cbind(mod1$dat,                                                         # store generated data + trueclass
              trueclass=mod1$trueclass)
  
  mod2 = poLCA.simdata(N       = nsize,                                         # generate ME part
                       nclass  = 3,
                       probs   = meas_error,
                       P       = c(0.4,0.35,0.25),
                       missval = F)
  
  df2 = cbind(mod2$dat[,1:ncol(mod2$dat)]+1,                                    # store generated data + trueclass
              sectors = mod2$trueclass+1) 
  
  for(v in 1:ncol(df1)){                                                        # combine meas data into se data
    to_replace = which(df1[,v] == 2)
    df1[,v][to_replace] <- df2[,v][to_replace]
  }
  
  dffreq = df1[,1:ncol(df1)] %>%                                                # aggregate data
    count(Y1, Y2, Y3, Y4, Y5, trueclass)
  
  return(dffreq) 
}
