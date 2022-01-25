#R file with all steps for MILC simulation study 

##########################################
#     Questions...                       #
##########################################
#part 4: Covar meenemen in posterior en imputatie berekening of niet?



#necessary packages
library(poLCA) #for data simulation
library(confreq) #used for making bootstrap datasets
library(dplyr) #for data manipulation

#simulation parameters
nsim = 3
populationsize = 5000
nboot = 5

##create storage room


SimData = list(NA) #empty list to store simulated data in
for (sim in 1:nsim) { #iteration over number of simulations
  
#-------------------------------1. DATA SIMULATION-----------#

#Variant A (5% selection error and 5% measurement error)
options(scipen = 999)
set.seed(123)

#5% selection error + strong covar
select_5 <-  list(matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                  matrix(c(0.7,  0.3,  0.3,  0.7),  ncol=2, byrow=T)) #Y5 = covar

#5% measurement error + weak covar
meas_5 <- list(matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(1/3,  1/3,   1/3,   1/3,   1/3,  1/3,   1/3,   1/3,   1/3),  ncol=3, byrow=TRUE)) # covar
# ** note, keep the structure of the covariate very similar to ease up the label switching process

# poLCA simdata object selection part
mod1 <- poLCA.simdata(N       = 5000,
                      nclass  = 2,
                      probs   = select_5, 
                      P       = c(0.15, 0.85), 
                      missval = F)

# simulated dataset selection part
df1 <- cbind(mod1$dat,
             trueclass=mod1$trueclass)

# poLCA simdata object measurement part
mod2 <- poLCA.simdata(N       = 5000, 
                      nclass  = 3,
                      probs   = meas_5,
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



#-------------------------------3. LCA STEP + LABEL CHECK (and switch if necessary)-----------#

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
  LCAS[[m]] = poLCA(formula = cbind(Y1, Y2, Y3, Y4, Z1) ~ 1,  
                    bootdata[[m]],       nclass = 4,      nrep = 10)
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


#---------------------4. function to calculate the posterior probabilities and imputations-----------#

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
      prob.y.given.x[i,c] <- prod(conditionals[[1]][c,(dataset[i,1])],conditionals[[2]][c,(dataset[i,2])],conditionals[[3]][c,(dataset[i,3])],conditionals[[4]][c,(dataset[i,4])]) #conditionals[[5]][c,(dataset[i,5])])
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
  }
}#end loop over bootstraps
slice_sample(implist[[2]], n=5) #sample 5 random rows from the results. 
#NOTE: trueclass is from original data. the imputations are from the bootstrap samples
#      we are interested in the total class proportions, so not in individual true & imputed classes


#-------------------------------5. Results-----------#
imp = list(NA)
imp2 = list(NA)
bias = list(NA)
method = list(NA)

#A. Overall group sizes
trueclass <- prop.table(table(df1$trueclass)) #proportions original data

## i. bias
for(m in 1:5){ #bias 
  imp[[m]] <- prop.table(table(implist[[m]]$imp))
  imp2[[m]] <- prop.table(table(implist[[m]]$imp2))
  bias[[m]] <- trueclass-imp[[m]]
  method[[m]] <- imp[[m]]-imp2[[m]]
}
bias #bias between group sizes of original data and the imputations of the bootstrap data
#pool bias
method #to see whether there is a difference between the two methods to impute the classes in step 4 (conclusion: there is no big difference)
## ii. SE
st.er <- function(x) sd(x)/sqrt(length(x))
st.er()
#average standard error/Standard deviation over all replications  

## iii. coverage CI (prop times population value falls within 95% CI around estimate over all replications)
CI <-  confint(trueclass) #werkt alleen op lm objects
FunConfInt <- function(x){
  interv <- function(x) 1.96*sd(x)/sqrt(length(x))
  lower <- mean(x)-interv(x)
  upper <- mean(x)+interv(x) 
  Confid <- cbind(lower, upper)
  return(Confid)
}
Capture <-  ifelse((CI[1]>0|CI[2]<0), 0, 1)


}#end loop over nsim
## iv. ME


#accuracy = Correct classifications/Total cases

#create confusion matrix? we are interested in population totals, not in classifications right?

#B. Relationship with covariate


#pool$lower   <- pool$qbar - qt(.975, pool$df) * sqrt(pool$t)
#pool$upper   <- pool$qbar + qt(.975, pool$df) * sqrt(pool$t)
#pool$coverage <- pool$lower <= mean(truth1) & mean(truth1) <= pool$upper

