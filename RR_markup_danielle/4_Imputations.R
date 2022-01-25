#--------------4. function to calculate the posterior probabilities and imputations of the classes-----------#
load("poLCA_and_Posteriors_RR.RData")

#assure parameters and data are available to use
SimData = SimData #keep the dataset in the .RData file
LCASIM = LCASIM
ssize=5000

#create lists to store results in
ImpSim = list(NA) 
LCAS2 = list(NA)
implist = list(NA)

for (sim in 1:nsim) { 
  df1 <-  SimData[[sim]]
  LCAS2 <-  LCASIM[[sim]]
# create empty columns to store calculated posteriors
df1[,c("p1","p2","p3","p4","imp")] <- NA 

#create storage for the posteriors
prob.y.given.x <- array(NA,dim=c(ssize,4))
prob.y <- c()
posterior_probs <- array(NA,dim=c(ssize,4))

#Function to calculate posterior membership probabilities and imputations of the classes 
posterior_function <- function(dataset,  ssize=5000, conditionals, Pclasses){ 
  for(i in 1:ssize){ 
    for(c in 1:4){ 
      prob.y.given.x[i,c] <- conditionals[[1]][c,(dataset[i,1])]*conditionals[[2]][c,(dataset[i,2])]*conditionals[[3]][c,(dataset[i,3])]*conditionals[[4]][c,(dataset[i,4])]
    }}
  for(i in 1:ssize){ 
    prob.y[i] <- Pclasses[1]*prob.y.given.x[i,1]+Pclasses[2]*prob.y.given.x[i,2]+Pclasses[3]*prob.y.given.x[i,3]+Pclasses[4]*prob.y.given.x[i,4]
  }
  for(i in 1:ssize){ 
    for(c in 1:4){ 
      posterior_probs[i,c] <-  (Pclasses[c]*prob.y.given.x[i,c])/prob.y[i]  
    }}
  posterior_probs<<-posterior_probs #assign result to global environment 
}#end function
colnames(posterior_probs) <- c("p1", "p2", "p3", "p4")

#Results
for(m in 1:nboot){ 
  implist[[m]] <- df1
  implist[[m]][,(6:9)] <-  posterior_function(dataset = bootdata[[m]], conditionals = LCAS2[[m]]$probs, Pclasses = LCAS2[[m]]$P)
    #create imputations by sampling from the posterior probabilities
    for (i in 1:ssize) {
      implist[[m]][i,"imp"] = sample(x=c(1:4), replace=T,size=1, prob = implist[[m]][i,c("p1","p2","p3","p4")])
      }
  }#end loop over bootstraps

#NOTE: trueclass is from original data. the imputations are from the bootstrap samples
#      we are interested in the total class proportions, so not in individual true & imputed classes
ImpSim[[sim]] <- implist
}#end loop over simulations
save.image("imputations_RR.RData")
