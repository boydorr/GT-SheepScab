source("Sheep_scab_functions_imperfect_test_NEW.R")
source("Sheep_scab_ADAS_parameters_NEW.R")

###################################################
Parameter_specification <- function(farmer.epidemiologist, perceived.transitions){

  ###################################################
  #Basic costs of diagnosis, disease and treatment from ADAS_parameters.R
  key_costs <- ADAS_costs()
  Cost.diagnosis.per.head <- key_costs[["Cost.diagnosis.per.head"]]
  Cost.per.head.dipped <- key_costs[["Cost.per.head.dipped"]]
  Cost.per.head.injected <- key_costs[["Cost.per.head.injected"]]
  Disease.cost.per.infected <- key_costs[["Disease.cost.per.infected"]]


  #Specify revenues per head for healthy (R.H), subclinical (R.M) and clincally infected animals (R.L)
  R.H <- 20  #the £20 is arbitrary and changing this just adds a constant to all costs without changing the solution
  R.L <- R.H - Disease.cost.per.infected  #Subtract off the cost of clinical infected
  subclinical.severity <- 0.15   # This the proportion of the Disease.cost.per.infected incurred by subclinical infection
  # We don't know what this is so different values need to be explored
  R.M <-R.H-subclinical.severity*Disease.cost.per.infected
  R.H.weight <- 0.8              #These weights say that a tested (with new test) and treated animal
  R.M.weight <- 0.2              #is associated with a revenue not quite as high as a completely healthy
  #animal. We specify it as a weighted mean of the healthy and subclinical revenue
  R.Tested_and_Treated <- R.H.weight*R.H+R.M.weight*R.M


  #Assume treatment costs correspond to those for dipping
  C.Treat <- Cost.per.head.dipped
  #Cost of diagnosis at clincial stage if farmer does not use test
  C.Diagnose <- Cost.diagnosis.per.head
  #Estimated cost of new test compared to current cost of diagnosis
  C.Test <-1.6*C.Diagnose ######SM: this is the default

  # #SM: try different test costs;
  #C.Test <-0.065*C.Diagnose
  #Allows us to examine ultimate profits if the farmers have the test cost subsidised
  #Just ignore for the time being
  C.Test.Subsidy <- 0 #1.6*C.Diagnose-C.Test

  #Specify proability of infection in low, medium and high risk states
  theta.L <- 0.0138
  theta.H <- 0.585
  theta.M <- 0.5*(theta.L+theta.H)
  phi.1 <- 132/27*0.075      #Expected proportion of flock infected in flock with scab (clinical plus subclinical)
  phi.2 <- 0.075 #27/160     #Ratio of clinical to subclinical in flock with scab 0.075 from ADAS;#
  #other numbers from Burgess paper

  # These test sensivities and specificities are only used in the case that "the farmer is an epidemiologist"
  # in which case the fact that the test process isn't perfect is allowed to feed into the payoffs - by this
  # I mean that the payoffs account for the fact that a "negative" flock will sometimes still
  # progress to disease and incur the costs of treating the disease at the clinical stage
  # Se.individual.test and Sp.individual.test are the test sensitivities and specificities at the individual
  # animal level. These are converted to P.Detect and P.False.Positive which are respectively, the
  # the probabilities of correctly detecting infection in an infected flock and the probability of falsely
  # identifying infection in an uninfected flock
  Se.individual.test <- 1   #Assume 100% sensitivity for now, otherwise use 0.982
  Sp.individual.test <- 1   #Assume 100% specificity for now, otherwise use 0.965
  Assumed.prevalence <- 1.0 #The flock level sensitivity and specificity will depend on the assumed prevalence
  #Default=0.2; set to 1.0 to replicate perfect test, administered to all sheep
  num.tested <- 12

  # When farmer.epidemiologist == TRUE this means the farmer accounts for the way payoffs are
  # affected either by the test being imperfect
  # Calculate P
  if (farmer.epidemiologist == TRUE) {
    P.False.Positive <- 1-(1-(1-Assumed.prevalence)*(1-Sp.individual.test))^num.tested #include actual test values
    P.Detect <- 1-(1-Assumed.prevalence*Se.individual.test)^num.tested       #input actual values
  } else {
    P.False.Positive <- 0.0
    P.Detect <- 1.0
  }

  PD <- P.Detect
  PF <- P.False.Positive


  ###################################################
  #Specify payoffs under the 4 possible outcomes for a flock
  # 1)	clinical infection is observed and treated
  # 2)	subclinical infection is correctly identified and treated
  # 3)	subclinical is present is incorrectly identified and treated
  # 4)	no infection is correctly identified

  P1 <- R.H
  P2 <- (1 - phi.1)*R.H + (phi.1-phi.2)*R.M  + phi.2*R.L  - (C.Diagnose+C.Treat)
  P3 <- R.H - C.Test - P.False.Positive*C.Treat
  P4 <- P.Detect*   ( (1 - phi.1)*R.H + phi.1*R.Tested_and_Treated - (C.Test+C.Treat) ) +
    (1-P.Detect)*( (1 - phi.1)*R.H + (phi.1-phi.2)*R.M  + phi.2*R.L  - (C.Test+C.Diagnose+C.Treat) )

  # Again just ignore the subsidy bit - it goes into a plot at some point but doesn't enter in to
  # any calculations
  P1.Subsidy <- 0
  P2.Subsidy <- 0
  P3.Subsidy <-  - C.Test.Subsidy
  P4.Subsidy <-  - C.Test.Subsidy

  ###################################################
  #Define mtrices H, M and L
  if (perceived.transitions == TRUE) {
    H <- matrix( c( 1,  1, 1, 1,
                    1,  0, 0, 0,
                    1,  0, 0, 0,
                    1,  0, 0, 0),
                 nrow=4, ncol=4, byrow=TRUE)

    M <- matrix( c( 0,  0, 0, 0,
                    0,  1, 1, 1,
                    0,  1, 1, 1,
                    0,  1, 1, 0),
                 nrow=4, ncol=4, byrow=TRUE)

    L <- matrix( c( 0,  0, 0, 0,
                    0,  0, 0, 0,
                    0,  0, 0, 0,
                    0,  0, 0, 1),
                 nrow=4, ncol=4, byrow=TRUE)
  } else {
    H <- matrix( c( 1,  1, 1, 1,
                    1,  0, 0, 0,
                    1,  0, 0, 0,
                    1,  0, 0, 0),
                 nrow=4, ncol=4, byrow=TRUE)

    M <- matrix( c( 0,  0, 0, 0,
                    0,  1, 1, 1,
                    0,  1, 0, 0,
                    0,  1, 0, 0),
                 nrow=4, ncol=4, byrow=TRUE)

    L <- matrix( c( 0,  0, 0, 0,
                    0,  0, 0, 0,
                    0,  0, 1, 1,
                    0,  0, 1, 1),
                 nrow=4, ncol=4, byrow=TRUE)
  }

  ###################################################
  ##Calculate payoff matrices and transition probabilities

  payoffs <- calculate_payoff_matrices(theta.H=theta.H, theta.M=theta.M, theta.L=theta.L,
                                       P1=P1, P2=P2, P3=P3, P4=P4)

  payoffs.Subsidy <- calculate_payoff_matrices(theta.H=theta.H, theta.M=theta.M, theta.L=theta.L,
                                               P1=P1.Subsidy, P2=P2.Subsidy, P3=P3.Subsidy, P4=P4.Subsidy)


  probs <- list()
  probs$state1.to.state1 <- matrix(c(
    DA(theta.H) %*% H %*% DA(theta.H),       DA(theta.H) %*% H %*% A(theta.H,PD,PF),
    A(theta.H,PD,PF) %*% H %*% DA(theta.H),  A(theta.H,PD,PF) %*% H %*% A(theta.H,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)

  probs$state1.to.state2 <- matrix(c(
    DA(theta.H) %*% M %*% DA(theta.H),       DA(theta.H) %*% M %*% A(theta.H,PD,PF),
    A(theta.H,PD,PF) %*% M %*% DA(theta.H),  A(theta.H,PD,PF) %*% M %*% A(theta.H,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)

  probs$state1.to.state3 <- matrix(c(
    DA(theta.H) %*% L %*% DA(theta.H),       DA(theta.H) %*% L %*% A(theta.H,PD,PF),
    A(theta.H,PD,PF) %*% L %*% DA(theta.H),  A(theta.H,PD,PF) %*% L %*% A(theta.H,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)


  probs$state2.to.state1 <- matrix(c(
    DA(theta.M) %*% H %*% DA(theta.M),       DA(theta.M) %*% H %*% A(theta.M,PD,PF),
    A(theta.M,PD,PF)  %*% H %*% DA(theta.M),  A(theta.M,PD,PF) %*% H %*% A(theta.M,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)

  probs$state2.to.state2 <- matrix(c(
    DA(theta.M) %*% M %*% DA(theta.M),        DA(theta.M)%*% M %*% A(theta.M,PD,PF),
    A(theta.M,PD,PF) %*% M %*% DA(theta.M),  A(theta.M,PD,PF) %*% M %*% A(theta.M,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)

  probs$state2.to.state3 <- matrix(c(
    DA(theta.M) %*% L %*% DA(theta.M),        DA(theta.M)%*% L %*% A(theta.M,PD,PF),
    A(theta.M,PD,PF) %*% L %*% DA(theta.M),  A(theta.M,PD,PF) %*% L %*% A(theta.M,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)

  probs$state3.to.state1 <- matrix(c(
    DA(theta.L) %*% H %*% DA(theta.L),       DA(theta.L) %*% H %*% A(theta.L,PD,PF),
    A(theta.L,PD,PF) %*% H %*% DA(theta.L),  A(theta.L,PD,PF) %*% H %*% A(theta.L,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)

  probs$state3.to.state2 <- matrix(c(
    DA(theta.L) %*% M %*% DA(theta.L),        DA(theta.L)%*% M %*% A(theta.L,PD,PF),
    A(theta.L,PD,PF) %*% M %*% DA(theta.L),  A(theta.L,PD,PF) %*% M %*% A(theta.L,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)

  probs$state3.to.state3 <- matrix(c(
    DA(theta.L) %*% L %*% DA(theta.L),        DA(theta.L)%*% L %*% A(theta.L,PD,PF),
    A(theta.L,PD,PF) %*% L %*% DA(theta.L),  A(theta.L,PD,PF) %*% L %*% A(theta.L,PD,PF) ),
    nrow=2, ncol=2, byrow=TRUE)

  # We are now gathering all the information about parameters into a single list called
  # Combined so that we have one thing for the function to return to the outside world
  Combined <- list()
  Combined[["probs"]] <- probs
  Combined[["payoffs"]] <- payoffs
  Combined[["payoffs.Subsidy"]] <- payoffs.Subsidy
  Combined[["Cost.diagnosis.per.head"]] <- Cost.diagnosis.per.head
  Combined[["Cost.per.head.dipped"]] <- Cost.per.head.dipped
  Combined[["Cost.per.head.injected"]] <- Cost.per.head.injected
  Combined[["Disease.cost.per.infected"]] <- Disease.cost.per.infected

  Combined[["phi.1"]] <- phi.1
  Combined[["phi.2"]] <- phi.2
  Combined[["Se.individual.test"]] <- Se.individual.test
  Combined[["Sp.individual.test"]] <- Sp.individual.test
  Combined[["Assumed.prevalence"]] <- Assumed.prevalence
  Combined[["num.tested"]] <- num.tested
  Combined[["P.Detect"]] <- P.Detect
  Combined[["P.False.Positive"]] <- P.False.Positive
  Combined[["Default.farmer.epidemiologist.Nash"]] <- farmer.epidemiologist
  Combined[["Default.perceived.transitions.Nash"]] <- perceived.transitions

  Combined[["R.H"]] <- R.H
  Combined[["subclinical.severity"]] <- subclinical.severity
  Combined[["R.M"]] <- R.M
  Combined[["R.L"]] <- R.L

  Combined[["R.H.weight"]] <- R.H.weight
  Combined[["R.M.weight"]] <- R.M.weight
  Combined[["R.Tested_and_Treated"]] <- R.Tested_and_Treated

  Combined[["C.Diagnose"]] <- C.Diagnose
  Combined[["C.Treat"]] <- C.Treat
  Combined[["C.Test/C.Diagnose"]] <- C.Test/C.Diagnose

  Combined[["theta.H"]] <- theta.H
  Combined[["theta.L"]] <- theta.L
  Combined[["theta.M"]] <- theta.M

  Combined[["P1"]] <- P1
  Combined[["P2"]] <- P2
  Combined[["P3"]] <- P3
  Combined[["P4"]] <- P4

  Combined

}

##------------------------------------------------------------------------------------
library(codetools)

findGlobals(Parameter_specification, merge=FALSE)$variables
