selection_error = list(
  list(matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),                      # 5% SE + strong
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.7,  .3,  .3,  .7),  ncol=2, byrow=T)
       ), 
  list(matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),                      # 5% SE + weak
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.95, .05, .05, .95), ncol=2, byrow=T),
       matrix(c(.5,  .5,  .5,  .5),  ncol=2, byrow=T)
       ),
  list(matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),                      # 20% SE + strong
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.7,  .3,  .3,  .7),  ncol=2, byrow=T)
       ), 
  list(matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),                      # 20% SE + weak
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),
       matrix(c(.8,  .2,  .2,  .8),  ncol=2, byrow=T),  
       matrix(c(.5,  .5,  .5,  .5),  ncol=2, byrow=T)
       )
)

measurement_error = list(
  list(matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), # 5% ME + weak
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(1/3,  1/3,  1/3,  1/3, 1/3,  1/3,  1/3,  1/3, 1/3), ncol=3, byrow=TRUE)
  ),
  list(matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), # 5% ME + strong
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.95, .025, .025, .025, .95, .025, .025, .025, .95), ncol=3, byrow=TRUE), 
       matrix(c(.5,  .25,  .25,  .25,  .5,  .25,  .25,  .25,  .5), ncol=3, byrow=TRUE)
  ),
  list(matrix(c(.8,   .1,  .1,  .1,  .8,  .1,  .1,  .1,  .8), ncol=3, byrow=TRUE), # 20% ME + weak
       matrix(c(.8,   .1,  .1,  .1,  .8,  .1,  .1,  .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.8,   .1,  .1,  .1,  .8,  .1,  .1,  .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.8,   .1,  .1,  .1,  .8,  .1,  .1,  .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3), ncol=3, byrow=TRUE)
  ), 
  list(matrix(c(.8,  .1,  .1,  .1, .8,  .1,  .1, .1,  .8), ncol=3, byrow=TRUE), # 20% ME + strong
       matrix(c(.8,  .1,  .1,  .1, .8,  .1,  .1, .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.8,  .1,  .1,  .1, .8,  .1,  .1, .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.8,  .1,  .1,  .1, .8,  .1,  .1, .1,  .8), ncol=3, byrow=TRUE), 
       matrix(c(.5, .25, .25, .25, .5, .25, .25, .25, .5), ncol=3, byrow=TRUE)
  ) 
)

simconds = function(){
  
  A5w5s   = list(selection_error[[1]], measurement_error[[2]]) 
  A5s5w   = list(selection_error[[2]], measurement_error[[1]])
  B5w20s  = list(selection_error[[1]], measurement_error[[4]])
  B5s20w  = list(selection_error[[2]], measurement_error[[3]])
  C20w5s  = list(selection_error[[3]], measurement_error[[2]])
  C20s5w  = list(selection_error[[4]], measurement_error[[1]])
  D20w20s = list(selection_error[[3]], measurement_error[[4]])
  D20s20w = list(selection_error[[4]], measurement_error[[3]])
  
  return(list(A5w5s, 
              A5s5w,
              B5w20s,
              B5s20w,
              C20w5s,
              C20s5w,
              D20w20s,
              D20s20w))}

