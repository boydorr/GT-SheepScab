find_value <- function(beta, f, g, payoffs, probs, num.states)
{
  
  value.estimate <- matrix(c(0), nrow=num.states, ncol=2, byrow=TRUE) #Changed ncol to num players
  I <- diag(num.states)
  
  M <- I - beta * P_fg(f, g, probs=probs, num.states)
  M.inv <- solve(M)
  
  ## Now solving Mv=b for player 1 and player 2
  for (player in 1:2)
  {
    b <- (1 - beta) * r_fg(player, f, g, payoffs=payoffs, probs=probs, num.states)
    value.estimate[,player] <- M.inv %*% b
  }
  value.estimate
}


##------------------------------------------------------------------------------------
library(codetools)

findGlobals(find_value, merge=FALSE)$variables