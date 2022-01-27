#Simulation Study - WITH COVAR
getwd()
setwd("C:/Users/danie/OneDrive/Statistiek master/Thesis/Thesis-LCA-danielle/Thesis-LCA-danielle/RR_markup_danielle")
start_time = Sys.time() #time, start counting

#load required packages
library(poLCA) #for data simulation
library(dplyr) #for data manipulation
library(confreq) #used for making bootstrap datasets 

#simulation parameters
nsim = 10 #increase to higher number of simulations when you have the time (nsim = 10 runt in 7.7 minuten)
nboot = 5 #FIXED, nboot determines the number of imputations per simulation iteration. 5 is sufficient.
populationsize = 5000 #for the data simulation

set.seed(123)
#execute respective scripts
source("1_SimulateData.R")    #Simulate the data
source("2_BootstrapData.R")   #Bootstrap the data

source("3_LCmodel.R")         #apply LC model to the data
source("4_Imputations.R")     #create imputations by sampling from the posterior probabilities

source("5_Results.R")         #Calculate bias of the overall group sizes (proportions of the classes)

#maak nieuwe 3-deling 

#results
Average #the pooled average class sizes, average over simulations
trueclass #original simulated class sizes
bias = Average-trueclass

end_time = Sys.time()
end_time-start_time #elapsed time for script
save.image("Simulation_Multivariate.RData")
