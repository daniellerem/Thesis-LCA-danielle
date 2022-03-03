posteriors = function(bootdat,
                      k){
  
  
  log <- capture.output({ #make sure not all messages are displayed
      longdat = as.data.frame(fre2dat(bootdat[,c(1:6,k+7)]))                      #column 7 contains the original data
   })
  log <- capture.output({ #make sure not all messages are displayed
  LCA = poLCA(formula = cbind(Y1, Y2, Y3, Y4, Y5) ~ 1,  
              longdat,       
              nclass = 4,      
              nrep = 10)
  })
  order = c(which.max(LCA$probs$Y1[,1]), 
            which.max(LCA$probs$Y1[,2]), 
            which.max(LCA$probs$Y1[,3]),
            which.max(LCA$probs$Y1[,4]))
  
  probs.start.new <- poLCA.reorder(LCA$probs.start, order)
  
  log <- capture.output({ #make sure not all messages are displayed
    LCAr <- poLCA(formula = cbind(Y1, Y2, Y3, Y4, Y5) ~ 1,
                longdat,
                nclass = 4,
                probs.start=probs.start.new)
  })
  entr <- poLCA.entropy(LCAr)
  p.se <- LCAr$P.se
  
  ordat = as.data.frame(fre2dat(bootdat[,c(1:7)]))
  
  posdat = cbind(ordat, 
                 poLCA.posterior(lc = LCAr, 
                                 y = ordat[,-6]), entr)                         #exclude 'trueclass' column from posterior calculation
                                                                                
  return(posdat)
  
}




