

#per simulatieconditie een list maken met daarin nsim geaggregeerde datasetjes + bootstraps,
#en die steeds een naam geven die duidelijk maakt wat de simulatieconditie is.
#

#de lijsten met errors voor de sim condities in ander script? 

##########################################
#        #Part 1: Simulate data          #
##########################################

#necessary packages
library(poLCA) #for data simulation
library(confreq) #used for making bootstrap datasets
library(dplyr) #for data manipulation


#simulation parameters
nsim = 100 #specify if different than in "ExecuteSimStudy....R"-script
populationsize = 5000
nboot = 5

#create storage room for simulated results 
Simboots = list(NA)
SimVariants = list(NA)
covar_props = c()
covar_sim = c()
true_sim=list(NA)
trueVariants=list(NA)

#miscallaneous
options(scipen = 999)
start_time = Sys.time() 

load("VarErrors.RData")

set.seed(123) #niet binnen de nsim loop! dan krijg je steeds dezelfde resultaten...

for (variant in 1:8) {
  var_errors = variants[[variant]]

for (sim in 1:nsim) { #iteration over number of simulations
  
  #-------------------------------1. DATA SIMULATION-----------#
  

  # poLCA simdata object selection part
  mod1 <- poLCA.simdata(N       = 5000,
                        nclass  = 2,
                        probs   = var_errors[[1]], 
                        P       = c(0.15, 0.85), 
                        missval = F)
  
  # simulated dataset selection part
  df1 <- cbind(mod1$dat,
               trueclass=mod1$trueclass)
  
  # poLCA simdata object measurement part
  mod2 <- poLCA.simdata(N       = 5000, 
                        nclass  = 3,
                        probs   = var_errors[[2]],
                        P       = c(0.4,0.35,0.25),
                        missval = F)
  
  # simulated dataset measurement part
  df2 <- cbind(mod2$dat[,1:ncol(mod2$dat)]+1, 
               sectors = mod2$trueclass+1) 
  
  # combine selection error and measurement error in one set
  for(v in 1:ncol(df1)){
    to_replace = which(df1[,v] == 2)
    df1[,v][to_replace] <- df2[,v][to_replace]
  }
  
  #calculate true scores
  #should be  0.15 & 0.85*(0.4,0.35,0.25) -> 
  #1) 0.15 2) 0.34 3) 0.2975 4) 0.2125
  original_classsizes = c(0.15, 0.34, 0.2975, 0.2125)
  #calculate frequencies of each score pattern
  dffreq <- df1[,1:ncol(df1)-1] %>% 
    count(Y1, Y2, Y3, Y4, Y5)
  

  #-------------------------------2. BOOTSTRAP DATA-----------#
  
                                                                                    #create nboot (=5) bootstrap samples
                                                                                    
                                                                                    # for each profile, sample nboot times, 
                                                                                    # from the total length of the dataset,
                                                                                    # with a probability equal to the observed frequency of that profile
  boots = rmultinom(nboot, 
                    sum(dffreq$n), 
                    dffreq$n/sum(dffreq$n))
  dfboot= cbind(dffreq, boots)
  Simboots[[sim]] <- dfboot
  
covar_sim[[sim]] <-  prop.table(table(df1$Y5))
true_sim[[sim]] <- prop.table(table(df1$trueclass))


} #end loop over nsim
  
  covar_props[[variant]] <- covar_sim
SimVariants[[variant]] <-  Simboots
trueVariants[[variant]] <- true_sim

} #end loop over variants
save(SimVariants, file = "SimData.RData")
save(trueVariants, file = "TrueData.RData")

dfVariants[[2]][[58]]  

