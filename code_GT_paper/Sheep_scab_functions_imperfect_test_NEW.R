
calculate_payoff_matrices <- function(theta.H, theta.M, theta.L, P1, P2, P3, P4)
{
  payoffs <- list()
  payoffs$state1.player1 <- matrix( c( profit(theta.H,"DA",P1,P2,P3,P4), profit(theta.H,"DA",P1,P2,P3,P4), 
                                       profit(theta.H,"A",P1,P2,P3,P4),profit(theta.H,"A",P1,P2,P3,P4)),
                                    nrow=2, ncol=2, byrow=TRUE)
  
  payoffs$state1.player2 <- matrix( c( profit(theta.H,"DA",P1,P2,P3,P4), profit(theta.H,"A",P1,P2,P3,P4), 
                                       profit(theta.H,"DA",P1,P2,P3,P4),profit(theta.H,"A",P1,P2,P3,P4)),
                                    nrow=2, ncol=2, byrow=TRUE)
  
  payoffs$state2.player1 <- matrix( c( profit(theta.M,"DA",P1,P2,P3,P4), profit(theta.M,"DA",P1,P2,P3,P4), 
                                       profit(theta.M,"A",P1,P2,P3,P4),profit(theta.M,"A",P1,P2,P3,P4)),
                                    nrow=2, ncol=2, byrow=TRUE)
  
  payoffs$state2.player2 <- matrix( c( profit(theta.M,"DA",P1,P2,P3,P4), profit(theta.M,"A",P1,P2,P3,P4), 
                                       profit(theta.M,"DA",P1,P2,P3,P4),profit(theta.M,"A",P1,P2,P3,P4)),
                                    nrow=2, ncol=2, byrow=TRUE)
  
  payoffs$state3.player1 <- matrix( c( profit(theta.L,"DA",P1,P2,P3,P4), profit(theta.L,"DA",P1,P2,P3,P4), 
                                       profit(theta.L,"A",P1,P2,P3,P4),profit(theta.L,"A",P1,P2,P3,P4)),
                                    nrow=2, ncol=2, byrow=TRUE)
  
  payoffs$state3.player2 <- matrix( c( profit(theta.L,"DA",P1,P2,P3,P4), profit(theta.L,"A",P1,P2,P3,P4), 
                                       profit(theta.L,"DA",P1,P2,P3,P4),profit(theta.L,"A",P1,P2,P3,P4)),
                                    nrow=2, ncol=2, byrow=TRUE)
  payoffs
  
}


profit <- function(theta, action,
                           P1, P2, P3, P4)
{
  if (action == "A") # A means adopt the test; DA means don't adopt
  {
    Expected2.payoff <-  (1-theta)*P3 + theta*P4
  }
  else if  (action == "DA")
  {
    Expected2.payoff <- (1-theta)*P1 + theta*P2
  }
  else
    stop("you've made a mistake")
  
  Expected2.payoff 
}


A <- function(theta, P.Detect, P.False.Positive){
     A <- c()
     A[1] <- (1-P.Detect)*theta
     A[2] <- P.Detect*theta
     A[3] <- P.False.Positive*(1-theta)
     A[4] <- (1-P.False.Positive)*(1-theta)
     A
}

DA <- function(theta){
  DA <- c()
  DA[1] <- theta
  DA[2] <- 0
  DA[3] <- 0
  DA[4] <- 1-theta
  DA
}

##------------------------------------------------------------------------------------
library(codetools)

findGlobals(A, merge=FALSE)$variables
findGlobals(DA, merge=FALSE)$variables
findGlobals(profit, merge=FALSE)$variables
findGlobals(calculate_payoff_matrices, merge=FALSE)$variables
