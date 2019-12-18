get_time <- function(m, num.states)
{
  time.in.state <- c(0)
  eigen(t(m))
  eigen_vec1 <- eigen(t(m))$vectors[,1]
  for (i in 1:num.states) {
    time.in.state[i] <- eigen_vec1[i]/(sum(eigen_vec1)) 
  }   
  time.in.state
}

##------------------------------------------------------------------------------------
library(codetools)

findGlobals(get_time, merge=FALSE)$variables
