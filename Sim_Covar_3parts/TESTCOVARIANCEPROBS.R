#test 



##########################################
#        #Part 2: Apply MILC             #
##########################################

#load necessary packages
library(poLCA) #for LC model 
library(dplyr) #for data manipulation

#simulation parameters
nsim = 2 #specify if different than in "ExecuteSimStudy....R"-script
populationsize = 5000
nboot = 5

#create storage room for simulated results 
ImpSim = list(NA)
ImpVariants = list(NA)

#miscallaneous
options(scipen = 999)
start_time = Sys.time() 

load("SimData.RData")

set.seed(123) #niet binnen de nsim loop! dan krijg je steeds dezelfde resultaten...


  
  for (sim in 1:nsim) { #iteration over number of simulations
    dfboot <- Simboots[[sim]]
    # LCA STEP + LABEL CHECK (and switch if necessary)-----------#
    
    #store results
    bootdata <- list(NA)
    LCAS <- list(NA)
    LCAS2 <- list(NA)
    
    #to test if labels are correct
    LCAS_probs <- list(NA)
    LCAS2_probs <- list(NA)
    LCAS3 = list(NA)
    set.seed(123)
    for (m in 1:5) {
      cat(m)
      bootdata[[m]] <- as.data.frame(confreq::fre2dat(dfboot[,c(1:5, (m+6))])) #converge frequency table to dataframe
      colnames(bootdata[[m]]) = c("Y1","Y2","Y3","Y4","Z1") # call covar Z1
      
      #run LC model on each bootstrap sample
      log <- capture.output({ #make sure not all messages are displayed
        LCAS[[m]] = ((poLCA(formula = cbind(Y1, Y2, Y3, Y4, Z1) ~ 1,  
                            bootdata[[m]],       nclass = 4,      nrep = 10)))   
      })
      
      LCAS_probs[[m]] <-   LCAS[[m]]$P #display proportions per class, for each bootstrap sample
      #conclusion: we have a label switching problem
      #Solution: 
      #column maxima switched label detection algorithm 
      #for each column/response we want to find which class has the highest likelihood
      order = c(which.max(LCAS[[m]]$probs$Y1[,1]), #class for which column 1 has the highest probability
                which.max(LCAS[[m]]$probs$Y1[,2]), # "    column 2  "
                which.max(LCAS[[m]]$probs$Y1[,3]),
                which.max(LCAS[[m]]$probs$Y1[,4]))
      
      LCAS2[[m]] <- LCAS[[m]] #assign values to new object. Next, change the order of the classes
      LCAS2[[m]]$probs$Y1 = LCAS[[m]]$probs$Y1[c(as.numeric(paste(order))),]
      LCAS2[[m]]$probs$Y2 = LCAS[[m]]$probs$Y2[c(as.numeric(paste(order))),]
      LCAS2[[m]]$probs$Y3 = LCAS[[m]]$probs$Y3[c(as.numeric(paste(order))),]
      LCAS2[[m]]$probs$Y4 = LCAS[[m]]$probs$Y4[c(as.numeric(paste(order))),]
      LCAS2[[m]]$probs$Z1 = LCAS[[m]]$probs$Z1[c(as.numeric(paste(order))),]
      LCAS2[[m]]$P        = LCAS[[m]]$P[c(as.numeric(paste(order)))]
      # WARNING: other elements in the LC output are not switched!! 
      LCAS2_probs[[m]] <-   LCAS2[[m]]$P #results, to check if label detection algorithm worked
    }
    
    #compare results as validation
    LCAS_probs #old P's with switched labels
    LCAS2_probs #relabelled P's 
    trueclass <- prop.table(table(df1$trueclass)) #proportions original data
    
    
    ###function to calculate the posterior probabilities and imputations-----------#
    
    # create empty columns to store calculated posteriors
    df1[,c("p1","p2","p3","p4","imp", "imp2")] <- NA 
    
    ssize=5000
    #create storage 
    prob.y.given.x <- array(NA,dim=c(ssize,4))
    prob.y <- c()
    posterior_probs <- array(NA,dim=c(ssize,4))
    
    posterior_function <- function(dataset,  ssize=5000, conditionals, Pclasses){ #need to provide a dataset and the conditional probabilities, P(score|class). Default samplesize is set to 5000
      for(i in 1:ssize){ #nr of observations in dataset
        for(c in 1:4){ #nr of classes
          prob.y.given.x[i,c] <- prod(conditionals[[1]][c,(dataset[i,1])],
                                      conditionals[[2]][c,(dataset[i,2])],
                                      conditionals[[3]][c,(dataset[i,3])],
                                      conditionals[[4]][c,(dataset[i,4])],
                                      conditionals[[5]][c,(dataset[i,5])])
        }}
      for(i in 1:ssize){ 
        prob.y[i] <- Pclasses[1]*prob.y.given.x[i,1]+Pclasses[2]*prob.y.given.x[i,2]+Pclasses[3]*prob.y.given.x[i,3]+Pclasses[4]*prob.y.given.x[i,4] #Pclasses*prob.y.given.x[i,5]
        
        #??: hoe Z1/Y5/covariaat hierin verwerken? welke Pclasses gebruiken? Nu geprobeerd te verdelen
        #mogelijk toch weghalen?
      }
      for(i in 1:ssize){ 
        for(c in 1:4){ #nr of classes
          posterior_probs[i,c] <-  (Pclasses[c]*prob.y.given.x[i,c])/prob.y[i]  
        }}
      posterior_probs<<-posterior_probs #assign result to the environment (to be able to access outside the function)
      #  colnames(posterior_probs) <- c("p1", "p2", "p3", "p4")
    }#end function
    
    #Results
    implist = list(NA) #store results
    for(m in 1:nboot){ #for each bootstrap sample
      implist[[m]] <- df1
      implist[[m]][,(7:10)] <-  posterior_function(dataset = bootdata[[m]], conditionals = LCAS2[[m]]$probs, Pclasses = LCAS2_probs[[m]])
      for (i in 1:ssize) {
        implist[[m]][i,"imp"] = which(rmultinom(1, 1, implist[[m]][i,c("p1","p2","p3","p4")]) == 1)
        implist[[m]][i,"imp2"] = sample(x=c(1:4), replace=T,size=1, prob = implist[[m]][i,c("p1","p2","p3","p4")])
      } #end loop over rows
    } #end loop over bootstraps
    ImpSim[[sim]] <- implist
    
  } #end loop over nsim
  

ImpSim
posterior_probs
imps <- implist[[1]][,c(7:10)]
prop.table(table(rowSums(sapply(imps, unlist))<.99999))
prop.table(table(rowSums(sapply(imps, unlist))==1))
prop.table(table(rowSums(sapply(imps, unlist))>1.00001))
rowSums(sapply(imps, unlist))
