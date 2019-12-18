source("fun/Stochastic_game_functions_NEW.R")         
source("fun/Stochastic_game_solving_for_value_NEW.R") 
source("fun/Stochastic_game_define_f_g_NEW.R") 
source("fun/Stochastic_game_get_time_NEW.R")

##################################################################
#Specify the objective function (derived from stochastic game theory) that is to be optimised
#by the optimisation algorithm
eval_f <- function(x, payoffs, probs, num.states, players.different, beta, optimal.value)
{
  f <- get_f(x=x, players.different=players.different, num.states=num.states)
  g <- get_g(x=x, players.different=players.different, num.states=num.states)
  value <- find_value(beta=beta, f=f, g=g, payoffs=payoffs, probs=probs, num.states=num.states)
  -(sum(value)-sum(optimal.value))
}

##################################################################
Solve_game_for_Pareto_optimum <- function(probs, payoffs, beta_steps, num.states, 
                                          players.different,
                                          name_of_scenario, num.iterations, eval_f=eval_f)
{
  opts <- list(algorithm="NLOPT_LN_COBYLA", xtol_rel=1.0e-7, maxeval=200, "print_level"=0,
               stopval=1.0e-7)
  
  answer <- list()
  output.index <-0
  
  cat("\n****************************************\n")
  cat("\nNow calculate Pareto optimum solution for each value of beta\n")
  cat("\n****************************************\n")
  for (beta in beta_steps)
  {
    cat(c("Valuing the future parameter, beta=", beta, "\n"))
    output.index <-output.index+1
    
    Boundary_solutions <- permutations(2,num.states*(1+players.different),0:1,repeats.allowed=TRUE)
    num.boundary.solutions <- 2^(num.states*(1+players.different))
    #We randomise the boundaray conditions to ensure that the optimal solutions don't tend
    #always occur with higher payments for one player rather than the other
    Boundary_solutions <- Boundary_solutions[permute(seq(1:num.boundary.solutions)),]
    
    for (iteration in 1:num.iterations)
    {
      lower.bound <-  c(rep(0.0,num.states+num.states*players.different))
      upper.bound <-  c(rep(1.0,num.states+num.states*players.different))
      

      if (iteration <=  2^(num.states*(1+players.different)  ) )  {
        x0 <- as.numeric(Boundary_solutions[iteration,])
      } else 
      {
        x0 <- lower.bound + (upper.bound-lower.bound)*(c(runif(num.states+num.states*players.different))) 
      }
      #Calculate value
      f <- get_f(x=x0, players.different=players.different, num.states=num.states)
      g <- get_g(x=x0, players.different=players.different, num.states=num.states)
      value <- find_value(beta=beta, f=f, g=g, payoffs=payoffs, probs=probs, num.states=num.states)
      m <- P_fg(f=f, g=g, probs=probs, num.states=num.states)
      time.in.state <- get_time(m=m, num.states=num.states)
      
      #Assess whether optimal
      if (iteration==1) {
        optimal.iteration=iteration
        optimal.value <- value
        optimal.x <- x0
        optimal.time.in.state <- time.in.state
      } else
      {
        if (iteration >  2^(num.states*(1+players.different)  ) ){
          res <- nloptr( x0=x0, eval_f=eval_f,
                         lb=lower.bound, 
                         ub=upper.bound, opts=opts, 
                         payoffs=payoffs, probs=probs, num.states=num.states, 
                         players.different=players.different, beta=beta, optimal.value=optimal.value)
          x0=res$solution
          epsilon <- 0.00001   #A fix to ensure that if the boundary is the solution it happens in the first number of                              #Boundary solutions iterations
          x0[which(x0<epsilon)] <- epsilon
          x0[which(x0>1-epsilon)] <- 1-epsilon
          #        print(x0)
          #        x0 <- lower.bound + (upper.bound-lower.bound)*(c(runif(num.states+num.states*players.different))) 
          f <- get_f(x=x0, players.different=players.different, num.states=num.states)
          g <- get_g(x=x0, players.different=players.different, num.states=num.states)
          value <- find_value(beta=beta, f=f, g=g, payoffs=payoffs, probs=probs, num.states=num.states)
          m <- P_fg(f=f, g=g, probs=probs, num.states=num.states)
          time.in.state <- get_time(m=m, num.states=num.states)}
        
        if ( sum(value) >=  sum(optimal.value) )  {
          optimal.iteration <- iteration
          optimal.value <- value
          optimal.x <- x0
          optimal.time.in.state <- time.in.state
        } 
      }
    }
    cat("\nPareto optima solution details:\n")
    cat("\nOptimal strategy given as:\n") 
    cat("probability of action 1 in state 1 for player 1\n")
    cat("probability of action 1 in state 2 for player 1\n")
    cat("probability of action 1 in state ... for player 1\n")
    cat("..then similarly for player 2 if we have allowed the players to differ\n")
    print(optimal.x)
    cat("\nColumn 1 of value matrix gives values for player 1 starting in state 1, state 2 etc ")
    cat("\nColumn 2 of value matrix gives values for player 2 starting in state 1, state 2 etc ")
    cat("\nValue=\n")
    print(optimal.value)
    cat("\n****************************************\n")
    
    answer[[output.index]] <- list(new_beta = beta, x_vector = optimal.x, 
                                   new_time = optimal.time.in.state, new_value = optimal.value,
                                   new_iteration = optimal.iteration)
  }
  
  Pareto_solution <- answer
}

##------------------------------------------------------------------------------------
library(codetools)

findGlobals(Solve_game_for_Pareto_optimum, merge=FALSE)$variables
findGlobals(eval_f, merge=FALSE)$variables