

#........................................#
#        #Part 3: Results                #
#........................................#

#I am now using apply statements. alternative is dplyr pipes.


#create empty lists  ##########################################
resultsvariants=list(NA)
resultsvariants_average=list(NA)
resultsvariants_bias = list(NA)
resultsvariants_sd = list(NA)
resultsvariants_prop.cov.classes = list(NA)
resultsvariants_simvar =list(NA)
resultsvariants_trueclassbias=list(NA)
resultsvariants_capture95CI=list(NA)
Pooled.bias_trueclass=list(NA)
SimProp.classes= list(NA)
SimPooledBias = list(NA)
resultsvariants_pooledbias=list(NA)
Pooled.bias = list(NA)
SimVariances = list(NA)
Capture=list(NA)
imp = list(NA)
bias = list(NA)
st.dev = list(NA)
prop.classes = list(NA)
prop.cov.class = list(NA)
between.var  = list(NA)
var.m  = list(NA)

#SimVariants[[8]][[99]] #dataset voor de 8e conditie en de 99e simulatie iteratie
#ImpVariants[[8]][[3]][[5]] #for 8condities, 100 simulations, and 5 imputations

#........................................#
#        Start summary of results        ####
#........................................#

#load data ##########
original_classsizes = c(0.15, 0.34, 0.2975, 0.2125)
load('ImpSim.RData') #simulated datasets with imputations
names(ImpVariants)=c('A5w5s','A5s5w','B5w20s','B5s20w','C20w5s','C20s5w','D20w20s','D20s20w')
nsim=20

for (variant in 1:8) { 
  ImpSim <- ImpVariants[[variant]] 
for (sim in 1:nsim) {
  implist <-  ImpSim[[sim]]

#A. Overall group sizes ########
original_classsizes = c(0.15, 0.34, 0.2975, 0.2125)

#proportions per class
for(m in 1:5){ #bias 
  imp[[m]] <- prop.table(table(implist[[m]]$imp))
  }

#Pooled proportions and bias ##########
Pooled.prop.classes <- rowMeans(sapply(imp, unlist))                         #class Proportions 
Pooled.bias <- abs(original_classsizes-Pooled.prop.classes)                  #pooled absolute bias

#functions ########
fun.sd.p <- function(prop) sqrt((prop*(1-prop))/5000) #standard deviation
fun.var.p <- function(prop) (prop*(1-prop))/5000      #variance
fun.var.p(0.5) #maximum variance value (var= 0.00005) is determined by sample size (n=5000)
FunConfInt <- function(prop){
  interv <- function(prop) 1.96*sqrt((prop*(1-prop))/5000)
  lower <- prop-interv(prop)
  upper <- prop+interv(prop) 
  Confid <- cbind(lower, prop, upper)
  return(Confid)
}

#Standard Deviation (SD)
original.sd <- fun.sd.p(original_classsizes)
st.dev[[sim]] <- colMeans(apply(sapply(imp, unlist), MARGIN = 1, FUN = fun.sd.p))

#Variances ######## 
for(m in 1:5){ #variances 
  var.m[[m]] <- fun.var.p(imp[[m]]) #variance
  between.var[[m]] <- (imp[[m]]-Pooled.prop.classes)^2              #var between   (parameter uncertainty caused by bootstrapping)
    }
                        
pooled.var.within <- rowMeans(sapply(var.m, unlist))                 #pooled within variance 
pooled.var.between <-   apply(X=sapply(between.var, unlist), MARGIN= 1,FUN = function(x) sum(x)/4)    #pooled between variance 
SimVariances[[sim]] <- pooled.var.between+pooled.var.within

# Coverage CI #####
#nr of times 95% CI of a simulation covers the original class proportions
FunConfInt(original_classsizes)
CI95 <- FunConfInt(Pooled.prop.classes)
CI95[,1]
Capture[[sim]] <-  ifelse((original_classsizes<CI95[,1]|original_classsizes>CI95[,3]), 0, 1)
#Capture2 <-  ifelse((original_classsizes>CI95[,1]&original_classsizes<CI95[,3]), 1, 0)


#pooled mean group sizes per simulation iteration
SimProp.classes[[sim]] <- Pooled.prop.classes 
SimPooledBias[[sim]] <- Pooled.bias

#Simulation summary 
Average <- rowMeans(sapply(SimProp.classes, unlist))  #(averages of simulation results)
SimBias = original_classsizes-Average
Sim_var <- apply(X=sapply(SimProp.classes, unlist), MARGIN =1, FUN = var) #variation over simulations



# B. Relationship w/ covariate ########

##covs
#truecovariate <- prop.table(table(implist[[1]]$Y5)) #proportions original data ??? zijn die er wel?

#covariate distr per class
cov.df <- prop.table(table(covariate=implist[[1]]$Y5, imputed_class=implist[[1]]$imp), margin = 2) 
prop.cov.class[[sim]] <- cov.df

# C. Relationship w/ true variable ########

trueclass <- prop.table(table(implist[[1]]$trueclass)) #proportions original data ??? zijn die er wel?

#proportions and bias
Pooled.bias_trueclass[[sim]] <- abs(trueclass-Pooled.prop.classes)            #pooled absolute bias



} #end loop over simulations
  
  #Results per variant ####
  resultsvariants_average[[variant]] <- Average 
  resultsvariants_bias[[variant]] <- SimBias 
  resultsvariants_pooledbias[[variant]] <- rowMeans(sapply(SimPooledBias, unlist))
  resultsvariants_sd[[variant]] <- rowMeans(sapply(st.dev, unlist))
  resultsvariants_simvar[[variant]] <- Sim_var
  resultsvariants_capture95CI[[variant]] <- apply(X=sapply(Capture, unlist), MARGIN =1 ,FUN = sum)/nsim
  #covariances
  resultsvariants_prop.cov.classes[[variant]] <- prop.cov.class
  #bias from trueclass per simulation
  resultsvariants_trueclassbias[[variant]] <- rowMeans(sapply(Pooled.bias_trueclass, unlist))
} #end loop over variants

 
