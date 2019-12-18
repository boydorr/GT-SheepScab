get_f <- function(x, players.different, num.states)
{  
  x <- x[1:num.states] #Extract the first num.state values from the solution vector
  f <- matrix(c(x,1-x),nrow=2,ncol=num.states,byrow=TRUE)
}

get_g <- function(x, players.different, num.states)
{
  if (players.different == 1) 
  {
    
    x <- x[(num.states+1):(2*num.states)] #Extract the second num.state values from the solution vector
    g <- matrix(c(x,1-x),nrow=2,ncol=num.states,byrow=TRUE)
    
  } else 
  {
    g <- matrix(c(x,1-x),nrow=2,ncol=num.states,byrow=TRUE)
  }
}

##------------------------------------------------------------------------------------
library(codetools)

findGlobals(get_f, merge=FALSE)$variables
findGlobals(get_g, merge=FALSE)$variables