rm(list = ls())

library('nloptr')
library('gtools')  #We need this library for the function 'permutations'
source("Generate_plots.R")
source("fun/Stochastic_game_nloptr4.R")
source("fun/Stochastic_game_pareto_algorithm.R")
##################################################################
#Source desired scenario
source("Sheep_scab_inputs.R")

##################################################################
# The function Parameter_specification extracts the probs and payoffs from the input file,
# e.g. Sheep_scab_inputs.R, which draws on the function ADAS_parameters.R
# where the baseline values from the Scottish Government ADAS report are stored.

# In case of imperfect test there can be distinctions between the payoffs and transition
# probabilities used by the farmer to calculate the Nash equilibrium and the Pareto optimum
# which depends on the real values of these quantities.

# If farmer.epidemiologist is TRUE the farmer accounts for changed payoffs due the imperfect test (see below).
# We may choose TRUE or FALSE here when calculating the Nash equilibrium.
# The default is TRUE when calculating the Pareto optimum.

# If perceived.transitions is TRUE the transitions between states accounted for by the farmer, 
# are those perceived by the farmer i.e. a positive test means the farmers assumes the future 
# plays out as though the flock IS currently infected.
# The default is TRUE when we calculate the Nash equilibrium.
# The default is FALSE  when we calculate the Pareto optimum.

##################################################################
name_of_scenario <- "Sheep_scab"

num.states <- 3
# Say whether players different or not
players.different <- 0

# Extract the parameters we need to calculate the Nash equilibrium
Parameters <- Parameter_specification(farmer.epidemiologist = TRUE, perceived.transitions = FALSE)
probs.Nash <- Parameters[["probs"]]
payoffs.Nash <- Parameters[["payoffs"]]

# We will also want the _actual_ values (long term payoffs) asociated with the farmer's decision
# so for this reason we also extract the probs and payoffs relevant to the Pareto optimum
Parameters <- Parameter_specification(farmer.epidemiologist = TRUE, perceived.transitions = FALSE)
probs.Pareto <- Parameters[["probs"]]
payoffs.Pareto <- Parameters[["payoffs"]]

##################################################################
#Solve the game for the Nash equilibrium for all values of beta
#Specify the number of beta steps for the Nash equilibrium solution

if (players.different==0){
  beta_steps <- seq(0.0,0.99,by=0.0025)
} else {
  beta_steps <- seq(0.0,0.99,by=0.05)
}

Game_solution_NE <- Solve_game_for_Nash_equilibrium(probs.Nash, payoffs.Nash, beta_steps, num.states, 
                                                    players.different,name_of_scenario, 
                                                    probs.Pareto, payoffs.Pareto, my_eval_f=my_eval_f)

# ##################################################################
#Solve the game for the Pareto optimum for all values of beta
#Specify the number of beta steps for the Pareto optimum solution
if (players.different==0){
  beta_steps <- seq(0.0,0.99,by=0.01)
  num.iterations=100
} else {
  beta_steps <- seq(0.0,0.99,by=0.05)
  num.iterations=100
}

Game_solution_PO <- NA #We need this default if Game_solution_PO) isn't calculated
Game_solution_PO <- Solve_game_for_Pareto_optimum(probs.Pareto, payoffs.Pareto, beta_steps, num.states, 
                                                  players.different,
                                                  name_of_scenario, num.iterations,
                                                  eval_f = eval_f)

# ##################################################################
#Plot the solution

plot_long_run_NEW(Solution=Game_solution_NE, num.states=num.states, 
                  players.different=players.different,
                  my_lwd=5, plot.default.cex=1.0,
                  sub.title="",
                  state_names = c("High risk", "Medium risk", "Low risk"), plot.order=c(3, 2, 1), 
                  plot.Action.1 = FALSE, 
                  action.names = c("Don't adopt new diagnostic test", "Adopt new diagnostic test"),
                  overlay.pareto.if.exists=TRUE, pareto2=Game_solution_PO)

plot_prevalences_NEW(Solution=Game_solution_NE, num.states=num.states, 
                     players.different=players.different,
                     my_lwd=5, plot.default.cex=1.0, 
                     legend_text=" ", sub.title="", leg="",
                     theta.H=Parameters[["theta.H"]], 
                     theta.M=Parameters[["theta.M"]], 
                     theta.L=Parameters[["theta.L"]],
                     overlay.pareto.if.exists=TRUE,
                     pareto2=Game_solution_PO)

plot_profits_NEW(Solution=Game_solution_NE, num.states=num.states,
                 players.different=players.different,
                 my_lwd=5, plot.default.cex=1.0,
                 legend_text=" ", sub.title="", leg="",
                 theta.H=Parameters[["theta.H"]],
                 theta.M=Parameters[["theta.M"]],
                 theta.L=Parameters[["theta.L"]],
                 payoffs=Parameters[["payoffs"]], Parameters[["payoffs.Subsidy"]],
                 overlay.pareto.if.exists=TRUE,
                 pareto2=Game_solution_PO)

plot_values_NEW(Game_solution_NE,num.states=num.states,  
               players.different=players.different,
               my_lwd=5, plot.default.cex=1.0, 
               legend_text="", sub.title="", 
               state_names = c("High risk", "Medium risk", "Low risk"), plot.order=c(3,2,1), leg="",
               overlay.pareto.if.exists=TRUE, pareto2=Game_solution_PO)

plot_times_NEW(Game_solution_NE,num.states=num.states,  
               players.different=players.different,
               my_lwd=5, plot.default.cex=1.0, 
               legend_text="", sub.title="", 
               state_names = c("High risk", "Medium risk", "Low risk"), plot.order=c(3, 2, 1), leg="",
               overlay.pareto.if.exists=TRUE, pareto2=Game_solution_PO)

# pdf(file="Sheepscab_plot_perfect test.pdf", height=3.8, width = 10.5)
plot_probabilities_NEW(Solution=Game_solution_NE, num.states=num.states, 
                       players.different=players.different,
                       my_lwd=5, plot.default.cex=1.0,
                       legend_text="Nash eq", legend_title=" ",
                       state_names = c("High risk", "Medium risk", "Low risk"), plot.order=c(3, 2, 1), 
                       plot.Action.1 = FALSE, 
                       action.names = c("Don't adopt new diagnostic test", "Adopt new diagnostic test"),
                       overlay.pareto.if.exists=TRUE, pareto2=Game_solution_PO,
                       chunk.data = "yes")
#dev.off()
###################################################################


