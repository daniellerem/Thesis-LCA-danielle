library(poLCA)

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
# ** note, i keep the structure of the covariate very similar to ease up the label switching process

# poLCA simdata object selection part
mod1 <- poLCA.simdata(N       = 5000,
                      nclass  = 2,
                      probs   = select_5, 
                      P       = c(0.2, 0.8), 
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

prop.table(table(df2$sectors))
# combine selection error and measurement error in one set
for(i in 1:ncol(df1)){
  to_replace = which(df1[,i] == 2)
  df1[,i][to_replace] <- df2[,i][to_replace]
}

save.image("simulated_dataset_RR.RData")
