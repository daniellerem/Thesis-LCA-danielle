library(poLCA)
library(tidyverse)

load("bootstrap_dataset_RR.RData")

set.seed(123)

# Go to long format
longdat = list(NA)
LCAS    = list(NA)
LCAS2   = list(NA)

nboot = 5


for(i in 1:nboot){
  cat(i)
  # temporarily create a short dataset per bootstrap sample
  datforslice = dfboot[,c(1:5,i+6)]
  colnames(datforslice) = c("Y1","Y2","Y3","Y4","Z1","Freq") # call covar Z1
  # use this as imput for the slice function
  longdat[[i]] = datforslice %>% 
    slice(rep(1:n(), Freq)) %>%  
    select(c("Y1","Y2","Y3","Y4","Z1"))
  
  # latent class model 
  LCAS[[i]] = poLCA(formula = cbind(Y1, Y2, Y3, Y4, Z1) ~ 1, 
                    longdat[[i]],
                    nclass = 4, 
                    nrep = 10)
  
  # now solve label switching issue
  ## first get the order of the classes of Y1 (assume consistence over indicators)
  order = c(which.max(LCAS[[i]]$probs$Y1[,1]),
            which.max(LCAS[[i]]$probs$Y1[,2]),
            which.max(LCAS[[i]]$probs$Y1[,3]),
            which.max(LCAS[[i]]$probs$Y1[,4]))
  
  ## store LC output under a new name
  LCAS2[[i]] = LCAS[[i]]
  
  LCAS2[[i]]$probs$Y1 = LCAS[[i]]$probs$Y1[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y2 = LCAS[[i]]$probs$Y2[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y3 = LCAS[[i]]$probs$Y3[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Y4 = LCAS[[i]]$probs$Y4[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$probs$Z1 = LCAS[[i]]$probs$Z1[,c(as.numeric(paste(order)))]
  LCAS2[[i]]$P        = LCAS[[i]]$P[c(as.numeric(paste(order)))]
  
  # WARNING: other elements in the LC output are not switched!! 
}

save.image("LCAS_RR.RData")