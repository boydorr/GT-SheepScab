source("fun/Stochastic_game_functions_NEW.R")         
source("fun/Stochastic_game_solving_for_value_NEW.R") 
source("fun/Stochastic_game_define_f_g_NEW.R") 
source("fun/Stochastic_game_get_time_NEW.R")

##################################################################
Solve_game_for_Nash_equilibrium <- function(probs, payoffs, beta_steps, num.states, 
                                            players.different, 
                                            name_of_scenario, probs.Pareto, payoffs.Pareto,
                                            my_eval_f=my_eval_f){

  output.index <-0
  answer <- list()
  
  for (beta in beta_steps)
  {
    cat("\n****************************************\n")
    cat(c("Valuing the future parameter, beta=", beta))
    output.index <- output.index+1
    Output <- algorithm(payoffs=payoffs, probs=probs, num.states=num.states, 
                        players.different=players.different, beta=beta, my_eval_f = my_eval_f)
    res <- Output[["Result"]]
    converged <- Output[["Solution_converged"]]
    
    f <- get_f(x=res$solution,players.different=players.different, num.states=num.states)
    g <- get_g(x=res$solution,players.different=players.different, num.states=num.states)
    value <- find_value(beta=beta, f=f, g=g, payoffs=payoffs.Pareto, probs=probs.Pareto, 
                        num.states=num.states)
    
    cat("\nNash equilibrium solution details:\n")
    cat("\nNash equilibrium strategy given as vector containing:\n") 
    cat("probability of action 1 in state 1 for player 1\n")
    cat("probability of action 1 in state 2 for player 1\n")
    cat("probability of action 1 in state ... for player 1\n")
    cat("..then similarly for player 2 if we have allowed the players to differ\n")
    #    print(res)  #Use this output if you want lots of details from nloptr
    print(res$solution) #Use this output if you DON'T want lots of details from nloptr
    cat("\nColumn 1 of value matrix gives values for player 1 starting in state 1, state 2 etc ")
    cat("\nColumn 2 of value matrix gives values for player 2 starting in state 1, state 2 etc ")
    cat("\nValue=\n")
    print(value)
    cat(c("\nOutputs for Nash equilibrium for scenario:", as.character(name_of_scenario,"\n")))

    m <- P_fg(f=f, g=g, probs=probs.Pareto, num.states=num.states)
    if (m[1,1]>1-1e-7 & m[1,2]<1e-7 & m[2,1]<1e-7 & m[2,2]>1-1e-7) {identity="yes"} else {identity="no"}
    time.in.state <- get_time(m=m, num.states=num.states)
    
    answer[[output.index]] <- list(new_beta = beta, x_vector = res$solution, 
                                   new_time = time.in.state, new_converged = converged,
                                   new_value = value,
                                   objective = res$objective)
  }
  
  Game_solution_NEW <- answer

}


##################################################################
#Specify the objective function (derived from stochastic game theory) that is to be optimised
#by the optimisation algorithm
my_eval_f <- function(x, payoffs, probs, num.states, players.different, beta)
{
  f <- get_f(x=x, players.different=players.different, num.states=num.states)
  g <- get_g(x=x, players.different=players.different, num.states=num.states)
  
  value <- find_value(beta=beta, f=f, g=g, payoffs=payoffs, probs=probs, num.states=num.states)
  
  Ca <- Constraint.a(state=1, beta=beta, value=value, f=f, g=g, payoffs=payoffs, probs=probs, 
                     num.states=num.states)       
  for (state in 2:num.states){
    Ca <- cbind(Ca,Constraint.a(state=state, beta=beta, value=value, f=f, g=g, payoffs=payoffs, 
                                probs=probs, num.states=num.states))
  }
  
  Cb <- Constraint.b(state=1, beta=beta, value=value, f=f, g=g, payoffs=payoffs, probs=probs, 
                     num.states=num.states)
  for (state in 2:num.states){
    Cb <- cbind(Cb,Constraint.b(state=state, beta=beta, value=value, f=f, g=g, payoffs=payoffs, 
                                probs=probs, num.states=num.states))
  }
  max(Ca,Cb,0)
}


##################################################################
#Specify function that calls the optimisation routine to solve for the Nash equilibrium
#for a given value of beta
algorithm <- function(payoffs, probs, num.states, players.different, beta, my_eval_f)
  {

  #Specify parameters required by nloptr
  opts <- list(algorithm="NLOPT_LN_COBYLA", xtol_rel=1.0e-7, maxeval=200, "print_level"=0,
               stopval=1.0e-7)
  
  #If the algorithm fails to converge, count.max gives the number of times we will try to
  #find a solution, starting from different random initial conditions
  count.max <- 500
  #Specify the range and dimension of the solution vector
  lower.bound <-  c(rep(0.0,num.states+num.states*players.different))
  upper.bound <-  c(rep(1.0,num.states+num.states*players.different))
  
  target.value.of.objective.function <- 1
  count <-0
  
  #Make optimisation rerun until it converges on sufficiently small obj func
  while(target.value.of.objective.function >= opts$stopval && count <= count.max) 
  {
    x0 <- lower.bound + (upper.bound-lower.bound)*(c(runif(num.states+num.states*players.different))) 
    res <- nloptr( x0=x0, eval_f=my_eval_f,
                   lb=lower.bound, ub=upper.bound, opts=opts, 
                   payoffs=payoffs, probs=probs, num.states=num.states, 
                   players.different=players.different, beta=beta)
    target.value.of.objective.function <- res$objective
    count <- count+1
#     cat(c("\ncount = ",count))
#     cat(c("\nIterations by nloptr = ",res$iterations))
#     cat("\n")
    converged <- (count != count.max)
  }
#  cat("Convergence target achieved before count reaches count.max?\n")
#  print(converged)
  #Return solution for this value of beta
  list(Result=res, Solution_converged=converged)
}


##------------------------------------------------------------------------------------
library(codetools)

findGlobals(Solve_game_for_Nash_equilibrium, merge=FALSE)$variables
findGlobals(my_eval_f, merge=FALSE)$variables