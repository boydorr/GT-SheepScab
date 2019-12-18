Extract_Nash_for_plotting <- function(Solution)
{
  #Extract the data to plot from the list format of Solution and convert to dataframes
  #First extract the vector of beta values and solution vector, which gives the probabilities of action 1
  #in each state:
  #For the fishing game, action 1 is being 'moderate' and action 2 is being greedy
  #For the sheep scab game, action 1 is 'non-adoption' and action 2 is adoption
  #The p vectors give the solution as the probability of action 1 in state 1, state 2 and state 3 etc,
  #For the sheep scab game state is high risk, state 2 is medium and state 3 is low risk
  #For the fishing game state 1 is plentiful fish and state 2 is few fish
  beta_values <- c()
  converged_values <- c()
  p.vectors.list <- list()
  time.vectors.list <- list()
  discounted.sum.list <- list()
  objective.vector <- c()
  for (i in 1:length(Solution)){
    beta_values[i] <- Solution[[i]]$new_beta
    converged_values[i] <- Solution[[i]]$new_converged
    p.vectors.list[[i]] <- Solution[[i]]$x_vector
    time.vectors.list[[i]] <- Solution[[i]]$new_time
    objective.vector[i] <- Solution[[i]]$objective
    discounted.sum.list[[i]] <- Solution[[i]]$new_value
  }
  
  #Convert lists to dataframes for plotting
  df_p_vector <- data.frame(matrix(unlist(p.vectors.list), 
                                   nrow=length(p.vectors.list), 
                                   byrow=TRUE))
  
  #Extract the proportion of the time spent in each state under the Nash equilibrium
  df_time <- data.frame(matrix(unlist(time.vectors.list), 
                               nrow=length(time.vectors.list), 
                               byrow=TRUE))
  
  df_discounted_sum <- data.frame(matrix(unlist(discounted.sum.list), 
                                  nrow=length(discounted.sum.list),
                                  byrow=TRUE))
  
  list(df_discounted_sum=df_discounted_sum, df_p_vector=df_p_vector, df_time=df_time, beta_values=beta_values, 
       converged_values=converged_values)
}

Extract_pareto_for_plotting <- function(Solution)
{
  #Extract the data to plot from the list format of Solution and convert to dataframes
  #First extract the vector of beta values and solution vector, which gives the probabilities of action 1
  #in each state:
  #For the fishing game, action 1 is being 'moderate' and action 2 is being greedy
  #For the sheep scab game, action 1 is 'non-adoption' and action 2 is adoption
  #The p vectors give the solution as the probability of action 1 in state 1, state 2 and state 3 etc,
  #For the sheep scab game state is high risk, state 2 is medium and state 3 is low risk
  #For the fishing game state 1 is plentiful fish and state 2 is few fish
  beta_values <- c()
  p.vectors.list <- list()
  time.vectors.list <- list()
  discounted.sum.list <- list()
  iteration <- c()
  for (i in 1:length(Solution)){
    beta_values[i] <- Solution[[i]]$new_beta
    p.vectors.list[[i]] <- Solution[[i]]$x_vector
    time.vectors.list[[i]] <- Solution[[i]]$new_time
    iteration[i] <- Solution[[i]]$new_iteration
    discounted.sum.list[[i]] <- Solution[[i]]$new_value
  }
  
  #Convert lists to dataframes for plotting
  df_p_vector <- data.frame(matrix(unlist(p.vectors.list), 
                                   nrow=length(p.vectors.list), 
                                   byrow=TRUE))
  
  #Extract the proportion of the time spent in each state under the Nash equilibrium
  df_time <- data.frame(matrix(unlist(time.vectors.list), 
                               nrow=length(time.vectors.list), 
                               byrow=TRUE))
  
  df_discounted_sum <- data.frame(matrix(unlist(discounted.sum.list), 
                                         nrow=length(discounted.sum.list),
                                         byrow=TRUE))
  
  list(df_discounted_sum=df_discounted_sum, df_p_vector=df_p_vector, df_time=df_time, 
       beta_values=beta_values, iteration = iteration)
}


#############################################################################
plot_long_run_NEW <- function(Solution, num.states, 
                                   players.different, 
                                   my_lwd, plot.default.cex,
                                   sub.title,
                                   state_names, plot.order, 
                                   plot.Action.1, action.names,
                                   overlay.pareto.if.exists,
                                   pareto2=NA, pareto.exists=is.list(pareto2))
{
  #Extract the data to plot from the list format of Solution and convert to dataframes
  Extracted_data <- Extract_Nash_for_plotting(Solution)
  df_p_vector <- Extracted_data$df_p_vector
  df_discounted_sum <- Extracted_data$df_discounted_sum
  df_time     <- Extracted_data$df_time
  beta_values <- Extracted_data$beta_values
  converged_values <- Extracted_data$converged_values
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
    Extracted_pareto_data <- Extract_pareto_for_plotting(pareto2)
    df_p_vector_pareto <- Extracted_pareto_data$df_p_vector
    df_time_pareto     <- Extracted_pareto_data$df_time
    beta_values_pareto <- Extracted_pareto_data$beta_values
    
  }
  
  #Choose whether to plot the probability of action 1 or action 2. 
  #For the fishing game, action 1 is being 'moderate' and action 2 is being greedy
  #For the sheep scab game, action 1 is 'non-adoption' and action 2 is adoption
  if (plot.Action.1 == TRUE) { name.action <- action.names[1] } else {
    df_p_vector <- 1-df_p_vector
    if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {df_p_vector_pareto <- 1- df_p_vector_pareto} 
    name.action <- action.names[2]
  }
  
  
  ####################################################################
  #Plot the time spent in each state, using the state number as the plotting symbol
  #For the sheep scab game, state 1 is high risk, state 2 is medium risk and state 3 is low risk 
  #For the fishing game, state 1 is plentiful fish and state 2 is few fish
  col_vec <- c("blue", "orange", "red")
  file_name <- "testing"
  par(mfrow=c(1,1))
  state <- plot.order[1]
  plot(beta_values, df_time[, state], xlim=c(0,1), ylim=c(0,1),
       xlab="Discount factor, beta", ylab=paste0("Time spent in each state in long run"),
       col=col_vec[state], col.lab="black", pch=as.character(eval(state)), cex=0.5)
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
    points(beta_values_pareto, df_time_pareto[,state], col="red",pch=as.character(eval(state)))
  }
  for (state in plot.order[2:num.states]) 
  { points(beta_values, df_time[,state], pch=as.character(eval(state)), col=col_vec[(state)], cex=0.5 )
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
    points(beta_values_pareto, df_time_pareto[,state], col="red",pch=as.character(eval(state)))
  }
  }
  
  legend("topright", pch=c(as.character(eval(plot.order))), col=col_vec[plot.order], 
         legend=c(state_names[plot.order]), text.col=col_vec[plot.order] )
  mtext(sub.title, 3, line=0.5)
  

}


#############################################################################
plot_probabilities_NEW <- function(Solution, num.states, 
                                   players.different, 
                                   my_lwd, plot.default.cex,
                                   legend_text, legend_title,
                                   state_names, plot.order, 
                                   plot.Action.1, action.names,
                                   overlay.pareto.if.exists,
                                   pareto2=NA, pareto.exists=is.list(pareto2),
                                   chunk.data)
{
  #Extract the data to plot from the list format of Solution and convert to dataframes
  Extracted_data <- Extract_Nash_for_plotting(Solution)
  df_p_vector <- Extracted_data$df_p_vector
  df_discounted_sum <- Extracted_data$df_discounted_sum
  df_time     <- Extracted_data$df_time
  beta_values <- Extracted_data$beta_values
  converged_values <- Extracted_data$converged_values
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
  Extracted_pareto_data <- Extract_pareto_for_plotting(pareto2)
  df_p_vector_pareto <- Extracted_pareto_data$df_p_vector
  df_time_pareto     <- Extracted_pareto_data$df_time
  beta_values_pareto <- Extracted_pareto_data$beta_values
  }
  
  #Choose whether to plot the probability of action 1 or action 2. 
  #For the fishing game, action 1 is being 'moderate' and action 2 is being greedy
  #For the sheep scab game, action 1 is 'non-adoption' and action 2 is adoption
  if (plot.Action.1 == TRUE) { name.action <- action.names[1] } else {
    df_p_vector <- 1-df_p_vector
    if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {df_p_vector_pareto <- 1- df_p_vector_pareto} 
    name.action <- action.names[2]
  }
  
  #Because we are going to break the solution into chunks and stick them back together it mkes
  #sense to attach the beta column to the p_vector
  df_p_vector_plus_beta <- cbind(df_p_vector, data.frame(beta_values)) #Add beta column
  
  ################################## 
  # Set up plot window defaults
  par(mfrow=c(1+players.different, num.states), 
      cex=plot.default.cex, 
      cex.lab=1.0*plot.default.cex, 
      cex.axis=1.0*plot.default.cex,
      cex.main=0.9*plot.default.cex, 
      cex.sub=1*plot.default.cex)
  col_vec <- c("blue", "blue", "blue")
  
  for (player in 1:(1+players.different))
  {
    for (state in plot.order) {
      which.state <- state_names[state]
      
      if (plot.Action.1 == TRUE){ my_ylab=paste( "Prob of action ", "'", action.names[1], "'", collapse = "" ) 
      } else {my_ylab=paste( "Prob of action ", "'", action.names[2], "'", collapse = "" )} 
      
      #If both players are the same we are going to split the data into chunks to be plotted with a full and dashed line
      #It is too complicated when the players are different
      if( (chunk.data == "yes") & (players.different == 0) ){
        epsilon <- 0.01
        chunk_a <- df_p_vector_plus_beta[df_p_vector_plus_beta[,state] < epsilon,]
        chunk_b <- df_p_vector_plus_beta[df_p_vector_plus_beta[,state] > 1 - epsilon,]
        chunk_c <- df_p_vector_plus_beta[df_p_vector_plus_beta[,state] > epsilon & df_p_vector_plus_beta[,state] < 1-epsilon ,]
        chunk_c <- chunk_c[order(chunk_c$beta),]
        new_chunks <- bind_chunks(chunk_a, chunk_b, chunk_c, state)
        chunk_a <- new_chunks$chunk_a
        chunk_b <- new_chunks$chunk_b
        chunk_c <- new_chunks$chunk_c
        
        plot(chunk_a$beta, chunk_a[,state + (player-1)*num.states],
             xlab="Discount factor, beta",
             ylab=my_ylab,xlim=c(0,1), ylim=c(0,1), col=col_vec[1],col.lab="black",
             type="l", lwd=my_lwd, yaxt="n",xaxt="n")
        lines(chunk_c$beta, chunk_c[,state + (player-1)*num.states],
              lty=2, col=col_vec[2],lwd=my_lwd,type="l")
        lines(chunk_b$beta, chunk_b[,state + (player-1)*num.states],
              col=col_vec[3],lwd=my_lwd,type="l")
        
      } else {
        plot(df_p_vector_plus_beta$beta, df_p_vector_plus_beta[,state + (player-1)*num.states],
             xlab="Discount factor, beta",
             ylab=my_ylab,xlim=c(0,1), ylim=c(0,1), col=col_vec[1],col.lab="black",
             type="p", pch=20, yaxt="n",xaxt="n")
      }      
      
      axis(1,at=c(0.0,0.5,1.0), las=1)
      axis(2,at=c(0.0,0.5,1.0), las=1)
      title(main=paste0(which.state))
      legend(0.18,0.9, cex=0.9*plot.default.cex, legend=legend_text, col=col_vec, lty=1, 
             lwd=my_lwd, bty="n", text.col=col_vec, title=legend_title, title.col="black", xjust=0.5)
      
      ################################## 
      if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
        points(beta_values_pareto, df_p_vector_pareto[,state], col="red", pch=1, cex=1)
        legend(0.01,0.5, cex=0.9*plot.default.cex,legend="Social optimum", col="red", pch=1,
               bty="n", text.col="red")
      }
    }
  }
}
#############################################################################
plot_times_NEW <- function(Solution, num.states, 
                           players.different,
                           my_lwd, plot.default.cex,
                           legend_text, sub.title, 
                           state_names, plot.order, leg,
                           overlay.pareto.if.exists,
                           pareto2=NA, pareto.exists=is.list(pareto2))
{
  #Extract the data to plot from the list format of Solution and convert to dataframes
  Extracted_data <- Extract_Nash_for_plotting(Solution)
  df_p_vector <- Extracted_data$df_p_vector
  df_discounted_sum <- Extracted_data$df_discounted_sum
  df_time     <- Extracted_data$df_time
  beta_values <- Extracted_data$beta_values
  converged_values <- Extracted_data$converged_values
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
    Extracted_pareto_data <- Extract_pareto_for_plotting(pareto2)
    df_p_vector_pareto <- Extracted_pareto_data$df_p_vector
    df_time_pareto     <- Extracted_pareto_data$df_time
    beta_values_pareto <- Extracted_pareto_data$beta_values
  }
  
  ################################################### 
  #Plot the times spent in different states
  par(mfrow=c(1,num.states),cex=plot.default.cex,cex.lab=1/plot.default.cex,cex.axis=1/plot.default.cex,
      cex.main=1/plot.default.cex, cex.sub=1/plot.default.cex)
  
  diff <- c()
  for (k in plot.order){
    if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
    diff[k] <- max( max(df_time[,k], df_time_pareto[,k]) - min(df_time[,k], df_time_pareto[,k]))
    } else {
      diff[k] <- max( max(df_time[,k]) - min(df_time[,k]))
    }
  }
  max.diff <- max(diff)
  
  for (k in plot.order){
    y_max <- min( 1, 0.5*(min(df_time[,k])+max(df_time[,k])) + max.diff    )
    y_min <- max( 0, 0.5*(min(df_time[,k])+max(df_time[,k])) - max.diff    )
    
    plot(beta_values, df_time[,k], col="blue",type="p",pch=20,cex=0.5,
         ylim=c(y_min,y_max), lwd=my_lwd,
         xlab="Discount factor, beta",
         ylab=paste0("Proportion of time spent in state"),
         main=paste0(state_names[k], " state"))
    mtext(sub.title, 3, line=0.5)
    
    if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ){ 
          points(beta_values_pareto, df_time_pareto[,k], col="red")
    }
  }
}

#############################################################################
plot_values_NEW <- function(Solution, num.states, 
                           players.different,
                           my_lwd, plot.default.cex,
                           legend_text, sub.title, 
                           state_names, plot.order, leg,
                           overlay.pareto.if.exists,
                           pareto2=NA, pareto.exists=is.list(pareto2))
{
  #Extract the data to plot from the list format of Solution and convert to dataframes
  Extracted_data <- Extract_Nash_for_plotting(Solution)
  df_p_vector <- Extracted_data$df_p_vector
  df_discounted_sum <- Extracted_data$df_discounted_sum
  df_time     <- Extracted_data$df_time
  beta_values <- Extracted_data$beta_values
  converged_values <- Extracted_data$converged_values
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
    Extracted_pareto_data <- Extract_pareto_for_plotting(pareto2)
    df_p_vector_pareto <- Extracted_pareto_data$df_p_vector
    df_time_pareto     <- Extracted_pareto_data$df_time
    beta_values_pareto <- Extracted_pareto_data$beta_values
    df_discounted_sum_pareto <- Extracted_pareto_data$df_discounted_sum
    iteration_pareto <- Extracted_pareto_data$iteration
  }

  ################################################### 
  #Plot the times spent in different states
  par(mfrow=c(1,num.states),cex=plot.default.cex,cex.lab=1/plot.default.cex,cex.axis=1/plot.default.cex,
      cex.main=1/plot.default.cex, cex.sub=1/plot.default.cex)
  
  for (k in plot.order){
    if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ){
      ylim=c( 0.999*min(df_discounted_sum[,k],df_discounted_sum_pareto[,k] ), 
              1.001*max(df_discounted_sum[,k],df_discounted_sum_pareto[,k] ) )
    } else {
      ylim=c( 0.999*min(df_discounted_sum[,k] ), 
              1.001*max(df_discounted_sum[,k]) )
      print(c("ylim",ylim,min(df_discounted_sum[,k]),max(df_discounted_sum[,k])))
    }

    plot(beta_values, df_discounted_sum[,k], col="blue",type="p",pch=20,cex=0.5,
         ylim=ylim,
         lwd=my_lwd,
         xlab="Discount factor, beta",
         ylab=paste0("Long term value"),
         main=paste0(state_names[k], " state"))
    mtext(sub.title, 3, line=0.5)
    
    if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ){ 
      points(beta_values_pareto, df_discounted_sum_pareto[,k], col="red")
    }
  }
}

#############################################################################
plot_prevalences_NEW <- function(Solution, num.states, 
                                 players.different,
                                 my_lwd, plot.default.cex, 
                                 legend_text, sub.title, leg,
                                 theta.H, theta.M, theta.L,
                                 overlay.pareto.if.exists,
                                 pareto2=NA, pareto.exists=is.list(pareto2))
{
  Extracted_data <- Extract_Nash_for_plotting(Solution)
  df_p_vector <- Extracted_data$df_p_vector
  df_discounted_sum <- Extracted_data$df_discounted_sum
  df_time     <- Extracted_data$df_time
  beta_values <- Extracted_data$beta_values
  converged_values <- Extracted_data$converged_values
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
    Extracted_pareto_data <- Extract_pareto_for_plotting(pareto2)
    df_p_vector_pareto <- Extracted_pareto_data$df_p_vector
    df_time_pareto     <- Extracted_pareto_data$df_time
    beta_values_pareto <- Extracted_pareto_data$beta_values
  }
  
  df_prevalence <- theta.H*df_time$X1+theta.M*df_time$X2+theta.L*df_time$X3
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
  df_prevalence_pareto <- theta.H*df_time_pareto$X1+theta.M*df_time_pareto$X2+theta.L*df_time_pareto$X3
  }
  
  ###################################################  
  par(mfrow=c(1, 1), cex=plot.default.cex,cex.lab=1/plot.default.cex,cex.axis=1/plot.default.cex,
      cex.main=1/plot.default.cex, cex.sub=1/plot.default.cex)
  
  plot(beta_values,df_prevalence, col="blue",type="p",pch=20,cex=0.5, 
       ylim=c(0,0.1), lwd=my_lwd, 
       xlab="Discount factor, beta", ylab="Proportion infected farms",
       yaxt="n", main="Equilibrium prevalence")
  axis(2, at=seq(0.0, 0.1,by=0.05), las=2)
  mtext(sub.title, 3, line=0.5)
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
points(beta_values_pareto, df_prevalence_pareto, col="red")
    }
}

#############################################################################
plot_profits_NEW <- function(Solution, num.states,
                             players.different,
                             my_lwd, plot.default.cex, 
                             legend_text, sub.title, leg,
                             theta.H=theta.H, theta.M=theta.M, theta.L=theta.L,
                             payoffs=payoffs, payoffs.Subsidy,
                             overlay.pareto.if.exists,
                             pareto2=NA, pareto.exists=is.list(pareto2))
{
  Extracted_data <- Extract_Nash_for_plotting(Solution)
  df_p_vector <- Extracted_data$df_p_vector
  df_discounted_sum <- Extracted_data$df_discounted_sum
  df_time     <- Extracted_data$df_time
  beta_values <- Extracted_data$beta_values
  converged_values <- Extracted_data$converged_values
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
    Extracted_pareto_data <- Extract_pareto_for_plotting(pareto2)
    df_p_vector_pareto <- Extracted_pareto_data$df_p_vector
    df_time_pareto     <- Extracted_pareto_data$df_time
    beta_values_pareto <- Extracted_pareto_data$beta_values
  }
  
  payoffs$state1.player1 <- payoffs$state1.player1+payoffs.Subsidy$state1.player1
  payoffs$state2.player1 <- payoffs$state2.player1+payoffs.Subsidy$state2.player1
  payoffs$state3.player1 <- payoffs$state3.player1+payoffs.Subsidy$state3.player1
  
  df_profit <- df_time$X1*(payoffs$state1.player1[1,1]*df_p_vector[,1] + 
                             payoffs$state1.player1[2,2]*(1-df_p_vector[,1])) +
    df_time$X2*(payoffs$state2.player1[1,1]*df_p_vector[,2] + payoffs$state2.player1[2,2]*(1-df_p_vector[,2])) +
    df_time$X3*(payoffs$state3.player1[1,1]*df_p_vector[,3] + payoffs$state3.player1[2,2]*(1-df_p_vector[,3])) 
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
  df_profit_pareto <- df_time_pareto$X1*(payoffs$state1.player1[1,1]*df_p_vector_pareto[,1] + 
                             payoffs$state1.player1[2,2]*(1-df_p_vector_pareto[,1])) +
    df_time_pareto$X2*(payoffs$state2.player1[1,1]*df_p_vector_pareto[,2] + payoffs$state2.player1[2,2]*(1-df_p_vector_pareto[,2])) +
    df_time_pareto$X3*(payoffs$state3.player1[1,1]*df_p_vector_pareto[,3] + payoffs$state3.player1[2,2]*(1-df_p_vector_pareto[,3])) 
  }
  
  ###################################################     
  par(mfrow=c(1, 1),cex=plot.default.cex,cex.lab=1/plot.default.cex,cex.axis=1/plot.default.cex,
      cex.main=1/plot.default.cex, cex.sub=1/plot.default.cex)
  
  plot(beta_values,df_profit, col="blue",type="p",pch=20,cex=0.5,
       ylim=c(19.9,20), lwd=my_lwd,
       xlab="Discount factor, beta", ylab="Profit per head",
       main="Profit"
  )
  axis(2, at=seq(0.0, 0.1,by=0.05), las=2)
  mtext(sub.title, 3, line=0.5)
  
  if ( (overlay.pareto.if.exists==TRUE) & (pareto.exists==TRUE) ) {
  points(beta_values_pareto, df_profit_pareto, col="red")
  }
}


#############################################################################
bind_chunks <- function(chunk_a, chunk_b, chunk_c, state)
{
  if (nrow(chunk_c) < 2) {
    if (nrow(chunk_a) > 0 && nrow(chunk_b) > 0)
      if (max(chunk_b$beta) < max(chunk_a$beta))
        chunk_c <- rbind(chunk_b[which.max(chunk_b$beta),],
                         chunk_a[which.min(chunk_a$beta),])
      else
        chunk_c <- rbind(chunk_a[which.max(chunk_a$beta),],
                         chunk_b[which.min(chunk_b$beta),])
  } else {
    if (nrow(chunk_a) > 0) {
      if (chunk_c$beta[which.min(chunk_c[,state])] <= chunk_c$beta[which.max(chunk_c[,state])])
      {
        if (min(chunk_c$beta) <= max(chunk_a$beta))
        {
          chunk_c <- rbind(chunk_a[which.max(chunk_a$beta),], chunk_c)
        } else {
          chunk_c <- rbind(chunk_a[which.min(chunk_a$beta),], chunk_c)
        }
      } else {
        if (min(chunk_c$beta) >= max(chunk_b$beta))
        {
          chunk_c <- rbind(chunk_c, chunk_a[which.min(chunk_a$beta),])
        } else {
          chunk_c <- rbind(chunk_c, chunk_a[which.max(chunk_a$beta),])
        }
      }
    }
    if (nrow(chunk_b) > 0) {
      if (chunk_c$beta[which.min(chunk_c[,state])] > chunk_c$beta[which.max(chunk_c[,state])])
      {
        if (min(chunk_c$beta) >= max(chunk_b$beta))
        {
          chunk_c <- rbind(chunk_b[which.max(chunk_b$beta),], chunk_c)
        } else {
          chunk_c <- rbind(chunk_b[which.min(chunk_b$beta),], chunk_c)
        }
      } else {
        if (min(chunk_c$beta) <= max(chunk_b$beta))
        {
          chunk_c <- rbind(chunk_c, chunk_b[which.min(chunk_b$beta),])
        } else {
          chunk_c <- rbind(chunk_c, chunk_b[which.max(chunk_b$beta),])
        }
      }
    }
  }
  list(chunk_a=chunk_a, chunk_b=chunk_b, chunk_c=chunk_c)
}


#############################################################################

##------------------------------------------------------------------------------------
library(codetools)

findGlobals(plot_probabilities_NEW, merge=FALSE)$variables
findGlobals(plot_times_NEW, merge=FALSE)$variables
findGlobals(plot_prevalences_NEW, merge=FALSE)$variables
findGlobals(plot_profits_NEW, merge=FALSE)$variables
findGlobals(bind_chunks, merge=FALSE)$variables



