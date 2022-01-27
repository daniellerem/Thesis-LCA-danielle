#Simulation Study
getwd()
setwd("C:/Users/danie/OneDrive/Statistiek master/Thesis/Thesis-LCA-danielle/Thesis-LCA-danielle/RR_markup_danielle")
library(tictoc)
start_time = Sys.time() #time, start counting
tic("simulation")
#load required packages
library(poLCA) #for data simulation
library(dplyr) #for data manipulation
library(confreq) #used for making bootstrap datasets 

#simulation parameters
nsim = 10 #increase to higher number of simulations when you have the time (nsim = 10 runt in 7.7 minuten)
nboot = 5 #FIXED, nboot determines the number of imputations per simulation iteration. 5 is sufficient.
populationsize = 5000 #for the data simulation

    # NOTES:
    #1) om parameters aan te passen moet die parameter niet in een script gedefinieerd staan
    #2) aangezien (bootstrap) data later weer nodig is, en het losse scripts zijn, heb ik deze een aantal keer bovenaan een script moeten zetten (bvb: "LCASIM = LCASIM") omdat hij anders niet in de .RData file kwam te staan
    #3) in de simulatie werkt het label switching niet meer. Ik ben nog op zoek naar wat er nodig is om het wel te laten werken.
    #4) set.seed(123) aan begin van simulatie studie. niet binnen de loop over nsim

set.seed(123)
#execute respective scripts
source("1_SimulateData.R")    #Simulate the data
source("2_BootstrapData.R")   #Bootstrap the data

source("3_LCmodel.R")         #apply LC model to the data
source("4_Imputations.R")     #create imputations by sampling from the posterior probabilities

source("5_Results.R")         #Calculate bias of the overall group sizes (proportions of the classes)

#maak nieuwe 3-deling 

#results
SimProp.classes #pooled over imputations per simulation
Average <- rowMeans(sapply(SimProp.classes, unlist))
Average #the pooled average class sizes
trueclass #original simulated class sizes
bias = Average-trueclass

end_time = Sys.time()
end_time-start_time #elapsed time for script
save.image("Simulation_Multivariate.RData")
toc()
