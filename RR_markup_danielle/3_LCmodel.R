#-------------------------------3. LCA STEP + LABEL check and correction -----------#
load("bootstraps_RR.RData")
library(confreq) #used for making bootstrap datasets

#make sure necessary data is still in .RData file
SimData = SimData 
dfboot = dfboot 

#create lists for storage 
bootdata <- list(NA)
LCAS <- list(NA)
LCAS2 <- list(NA)
LCASIM = list(NA) 
LCAS_probs <- list(NA)
LCAS2_probs <- list(NA)


for (sim in 1:2) { 
set.seed(123)
for (i in 1:5) {
  #create dataset per bootstrap sample with the following code:
  bootdata[[i]] <- as.data.frame(confreq::fre2dat(dfboot[[sim]][,c(1:4, (i+5))])) #converge frequency table to dataframe
  #run LC model on each bootstrap sample
  LCAS[[i]] = poLCA(formula = cbind(Y1, Y2, Y3, Y4) ~ 1,  
                    bootdata[[i]],       nclass = 4,      nrep = 10)
#display proportions per class, for each bootstrap sample 
 LCAS_probs[[i]] <-   LCAS[[i]]$P 
#conclusion: we have a label switching problem
#Solution: 
  #column maxima switched label detection algorithm 
 #for each column/response we want to find which class has the highest likelihood
  order1 = c(which.max(LCAS[[i]]$probs$Y1[,1]), #class for which column 1 has the highest probability
            which.max(LCAS[[i]]$probs$Y1[,2]), # "    column 2  "
            which.max(LCAS[[i]]$probs$Y1[,3]),
            which.max(LCAS[[i]]$probs$Y1[,4]))
  
  order2 = c(which.max(LCAS[[i]]$probs$Y2[,1]), #class for which column 1 has the highest probability
             which.max(LCAS[[i]]$probs$Y2[,2]), # "    column 2  "
             which.max(LCAS[[i]]$probs$Y2[,3]),
             which.max(LCAS[[i]]$probs$Y2[,4]))
  
  order3 = c(which.max(LCAS[[i]]$probs$Y3[,1]), #class for which column 1 has the highest probability
             which.max(LCAS[[i]]$probs$Y3[,2]), # "    column 2  "
             which.max(LCAS[[i]]$probs$Y3[,3]),
             which.max(LCAS[[i]]$probs$Y3[,4]))
  
  order4 = c(which.max(LCAS[[i]]$probs$Y4[,1]), #class for which column 1 has the highest probability
             which.max(LCAS[[i]]$probs$Y4[,2]), # "    column 2  "
             which.max(LCAS[[i]]$probs$Y4[,3]),
             which.max(LCAS[[i]]$probs$Y4[,4]))
  

  #Change the order of the classes
  LCAS2[[i]] <- LCAS[[i]] 
  LCAS2[[i]]$probs$Y1 = LCAS[[i]]$probs$Y1[c(as.numeric(paste(order1))),]
  LCAS2[[i]]$probs$Y2 = LCAS[[i]]$probs$Y2[c(as.numeric(paste(order2))),]
  LCAS2[[i]]$probs$Y3 = LCAS[[i]]$probs$Y3[c(as.numeric(paste(order3))),]
  LCAS2[[i]]$probs$Y4 = LCAS[[i]]$probs$Y4[c(as.numeric(paste(order4))),]
  LCAS2[[i]]$P        = sort(LCAS[[i]]$P, decreasing=T)
  # WARNING: other elements in the LC output are not switched!! 
  LCAS2_probs[[i]] <-   LCAS2[[i]]$P #results, to check if label detection algorithm worked
} 
  LCAS2_probs
  #LCASIM[[2]][[4]]$probs  #hier staat de diagonaal met de highest probs de andere kant op...

LCASIM[[sim]] <- LCAS2 #The objects of LCAS2 with reassigned labels for each of the simulations get stored in the list LCASIM (now a list of lists)
}#end of LC model script
save.image(file="poLCA_and_Posteriors_RR.RData")

