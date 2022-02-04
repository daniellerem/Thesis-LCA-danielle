#########################################################
#        #lists of errors for sim conditions            #
#########################################################



#5% selection error + strong covar
select_5s <-  list(matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                   matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                   matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                   matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                   matrix(c(0.7,  0.3,  0.3,  0.7),  ncol=2, byrow=T)) #Y5 = covar
#5% selection error + weak covar
select_5w <-  list(matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                   matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                   matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                   matrix(c(0.95, 0.05, 0.05, 0.95), ncol=2, byrow=T),
                   matrix(c(0.5,  0.5,  0.5,  0.5),  ncol=2, byrow=T)) #Y5 = covar 
#20% selection error + strong covar
select_20s <- list(matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
                   matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
                   matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
                   matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
                   matrix(c(0.7,  0.3,    0.3,  0.7),  ncol=2, byrow=T)) #Y5 = covar
#20% selection error + weak covar
select_20w <- list(matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
                   matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
                   matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),
                   matrix(c(0.8, 0.2,     0.2, 0.8), ncol=2, byrow=T),  
                   matrix(c(0.5,  0.5,  0.5,  0.5),  ncol=2, byrow=T)) #Y5 = covar 

selection_errors= list(select_5w, select_5s, select_20w, select_20s)

#5% measurement error + weak covar
meas_5w <- list(matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
               matrix(c(1/3,  1/3,   1/3,   1/3,   1/3,  1/3,   1/3,   1/3,   1/3),  ncol=3, byrow=TRUE)) # covar
meas_5s <- list(matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
                matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
                matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
                matrix(c(0.95, 0.025, 0.025, 0.025, 0.95, 0.025, 0.025, 0.025, 0.95), ncol=3, byrow=TRUE), 
                matrix(c(1/2,  1/4,   1/4,   1/4,  1/2,   1/4,   1/4,  1/4,   1/2 ),  ncol=3, byrow=TRUE)) # covar

#20% classification error

meas_20w <- list(matrix(c(0.8,0.1,0.1,    0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3,   byrow=TRUE), # Y1
                matrix(c(0.8,0.1,0.1,    0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3, byrow=TRUE), # Y2
                matrix(c(0.80,0.1,0.1,   0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3, byrow=TRUE), #Y3
                matrix(c(0.80,0.1,0.1,   0.1,0.8,0.1,     0.1,0.1,0.8  ), ncol=3, byrow=TRUE), #Y4
                matrix(c(1/3,  1/3,   1/3,   1/3,   1/3,  1/3,   1/3,   1/3,   1/3),  ncol=3, byrow=TRUE)) # covar

meas_20s <- list(matrix(c(0.8,0.1,0.1,    0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3,   byrow=TRUE), # Y1
                 matrix(c(0.8,0.1,0.1,    0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3, byrow=TRUE), # Y2
                 matrix(c(0.80,0.1,0.1,   0.1,0.8,0.1,     0.1,0.1,0.8 ), ncol=3, byrow=TRUE), #Y3
                 matrix(c(0.80,0.1,0.1,   0.1,0.8,0.1,     0.1,0.1,0.8  ), ncol=3, byrow=TRUE), #Y4
                 matrix(c(1/2,  1/4,   1/4,   1/4,  1/2,   1/4,   1/4,  1/4,   1/2 ),  ncol=3, byrow=TRUE)) # covar
measurement_errors= list(meas_5w, meas_5s, meas_20w, meas_20s) 

##variants
A5w5s = list(select_5w, meas_5s) 
A5s5w = list(select_5s, meas_5w)
B5w20s = list(select_5w, meas_20s)
B5s20w = list(select_5s, meas_20w)
C20w5s = list(select_20w, meas_5s)
C20s5w = list(select_20s, meas_5w)
D20w20s = list(select_20w, meas_20s)
D20s20w = list(select_20s, meas_20w)
variants = list(A5w5s,A5s5w,B5w20s,B5s20w,C20w5s,C20s5w,D20w20s,D20s20w)

save(variants, file = "VarErrors.RData")