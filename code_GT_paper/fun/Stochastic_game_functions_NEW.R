##------------------------------------------------------------------------------------
## Define Rk(s) which a matrix of dimension N_actions x N_actions, here 2x2
R_func <- function(player, current.state, payoffs)
{

      payoffs[[2*(current.state-1)+player]]

#   if (player == 1 && current.state == 1) payoffs$state1.player1
#   else if (player == 2 && current.state == 1) payoffs$state1.player2
#   else if (player == 1 && current.state == 2) payoffs$state2.player1
#   else if (player == 2 && current.state == 2) payoffs$state2.player2
  
#   name_of_output <- paste0("payoffs$state",current.state,".player",player) #New 270914
#   name_of_output
#   eval(parse(text=name_of_output))
}

##------------------------------------------------------------------------------------
## Define the pay-offs r1(s,a1,a2) and r2(s,a1,a2) 
r1 <- function(s, action.player1, action.player2, payoffs)
{
  player <- 1
  payoffs[[2*(s-1)+player]][action.player1,action.player2]
#  R_func(player=1, s, payoffs=payoffs)[action.player1, action.player2]
}

r2 <- function(s, action.player1, action.player2, payoffs)
{
  player <- 2
  payoffs[[2*(s-1)+player]][action.player1,action.player2]
#  R_func(player=2, s, payoffs=payoffs)[action.player1, action.player2]
}

##------------------------------------------------------------------------------------
## Define the function p(s'|s,a1,a2) see Notation section at end of 3.1 of Filar and Vrieze
p <- function(s_prime, s, action.player1, action.player2, probs, num.states)
{
#   if (s_prime==1 && s==1)
#     probs$state1.to.state1[action.player1, action.player2]
#   else if (s_prime==2 && s==1)
#     probs$state1.to.state2[action.player1, action.player2]
#   else if (s_prime==1 && s==2)
#     probs$state2.to.state1[action.player1, action.player2]
#   else if (s_prime==2 && s==2)
#     probs$state2.to.state2[action.player1, action.player2]
  
#   name_of_output <- paste0("probs$state",s,".to.state",s_prime) #New 270914
#   name_of_output
#   eval(parse(text=name_of_output))[action.player1, action.player2]
  
  probs[[num.states*(s-1)+s_prime]][action.player1, action.player2]
  
}

##------------------------------------------------------------------------------------
## Define stochastic transition matrix P(f,g) 
## see Notation section at end of 3.1 of Filar and Vrieze
transition.prob <- function(new.state, current.state, f, g, probs, num.states)
{
  prob <- 0
  for(action.player1 in 1:2)
    for(action.player2 in 1:2)
      prob <- prob + 
    p(new.state, current.state, action.player1, action.player2, probs=probs, num.states) *
    f[action.player1, current.state] * g[action.player2, current.state]
  prob
}

## Call stochastic matrix function
P_fg <- function(f, g, probs, num.states)
{
  
#   matrix(c(transition.prob(1, 1, f, g, probs=probs),
#            transition.prob(2, 1, f, g, probs=probs),
#            transition.prob(1, 2, f, g, probs=probs),
#            transition.prob(2, 2, f, g, probs=probs)),
#          nrow=2, ncol=2, byrow=TRUE) ## Rows and columns confused [RR]
   
  P_fg <- matrix(c(0),nrow=num.states,ncol=num.states)  #New 270914
  for (current.state in 1:num.states) {
    for (new.state in 1:num.states) {
      P_fg[current.state,new.state] <- transition.prob(new.state, current.state, f, g, probs=probs, num.states)
    }
  }
  P_fg
  
}

##------------------------------------------------------------------------------------
## Define expected pay-off function rk(f,g) - a column vector of length N_states, here 2
Expected.payoff <- function(state, f, g, player, payoffs)
{
  payoff <- 0
  for (action.player1 in 1:2)
  {
    for (action.player2 in 1:2)
    {
      if (player==1)
      {
        payoff <- payoff +
          r1(state, action.player1, action.player2, payoffs=payoffs) *
          f[action.player1, state] * g[action.player2, state]
      }
      else if (player==2)
      {
        payoff <- payoff +
          r2(state, action.player1, action.player2, payoffs=payoffs) *
          f[action.player1, state] * g[action.player2, state]
      }
    }
  }
  payoff
}

r_fg <- function(player, f, g, payoffs, probs, num.states)
{
#   matrix(c(Expected.payoff(state=1, f, g, player, payoffs=payoffs),
#            Expected.payoff(state=2, f, g, player, payoffs=payoffs)), nrow=2)
  
  r_fg <- matrix(c(0),nrow=num.states)  #New 270914
  for (state in 1:num.states){
    r_fg[state] <- Expected.payoff(state, f, g, player, payoffs=payoffs)
  }
  r_fg
  
}

##------------------------------------------------------------------------------------
## Specify function to be minimised - a scalar
objective.function <- function(beta, value, f, g, payoffs, probs, num.states)
{
  One_vec_row <- matrix(1, ncol=2)
  sum <- 0
  for (player in 1:2)
  {
    full.payoff <- value[, player]
    immediate <- (1 - beta) * r_fg(player, f, g, payoffs=payoffs, probs=probs, num.states) ## Added 1 - beta term
    discounted.future <- beta * P_fg(f, g, probs=probs, num.states) %*% value[, player]
    sum <- sum + One_vec_row %*% abs(full.payoff - immediate - discounted.future) ## Gone for absolute value to minimise for each player which fits with Thuijsmans notes
  }
  sum
}

##------------------------------------------------------------------------------------
## Set functions need for constraints

## Define T(s,v) which is a N_state x N_state matrix, here 2x2
T_func <- function(current.state, value.vec, probs, num.states)
{
  mat <- mat.or.vec(2, 2)
  
  for (action1 in 1:2)
  {
    for (action2 in 1:2)
    {
      for (new.state in 1:num.states)  #new on 27/0914  
      {
        mat[action1, action2] <- mat[action1, action2] + 
          p(new.state, current.state, action1, action2, probs=probs, num.states) *
          value.vec[new.state]
      }
    }
  }
  mat
}

##------------------------------------------------------------------------------------
## Define constraint functions
Constraint.a <- function(state, beta, value, f, g, payoffs, probs, num.states)
{
  One_vec_row <- matrix(1, ncol=2)
  
  f_vec <- matrix(f[, state], ncol=2)  # f_vec is row vector of action probs in given state for player 1
  value.vec <- value[, 2]              # value vector for player 2
  
  immediate <- (1 - beta) * f_vec %*% R_func(player=2, state, payoffs=payoffs) # Added (1 - beta) term
  discounted.future <- beta * f_vec %*% T_func(state, value.vec, probs=probs, num.states)
  full.payoff <- value.vec[state] * One_vec_row
  immediate + discounted.future - full.payoff
}

Constraint.b <- function(state, beta, value, f, g, payoffs, probs, num.states)
{
  One_vec_col <- matrix(1, nrow=2)
  g_vec <- matrix(g[, state], nrow=2) # g_vec is col vector of action probs in given state for player 2
  value.vec <- value[, 1]             # value vector for player 1
  
  immediate <- (1 - beta) * R_func(player=1, state, payoffs=payoffs) %*% g_vec # Added (1 - beta) term
  discounted.future <- beta * T_func(state, value.vec, probs=probs, num.states) %*% g_vec
  full.payoff <- value.vec[state] * One_vec_col
  immediate + discounted.future - full.payoff
}

##------------------------------------------------------------------------------------
library(codetools)

findGlobals(r1, merge=FALSE)$variables
findGlobals(r2, merge=FALSE)$variables
findGlobals(p, merge=FALSE)$variables
findGlobals(transition.prob, merge=FALSE)$variables
findGlobals(P_fg, merge=FALSE)$variables
findGlobals(objective.function, merge=FALSE)$variables
findGlobals(T_func, merge=FALSE)$variables
findGlobals(R_func, merge=FALSE)$variables
findGlobals(Expected.payoff, merge=FALSE)$variables
findGlobals(r_fg, merge=FALSE)$variables
findGlobals(Constraint.a, merge=FALSE)$variables
findGlobals(Constraint.b, merge=FALSE)$variables

