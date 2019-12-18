ADAS_costs <- function(){
  
  #Sheep scab parameters from page 136 and 137 of ADAS report
  #############################################################
  #Treatment costs
  
  Sheep.population <- 7608100
  Flocks           <- 15682
  Flock.size <- Sheep.population/Flocks
  Flock.size
  
  Prop.flocks.infected <- 0.165
  Num.infected.flocks <- Prop.flocks.infected*Flocks
  Num.infected.flocks 
  
  Num.flocks.diagnosed <- 647
  Cost.diagnosis.per.flock <- 100
  Cost.diagnosis <- Num.flocks.diagnosed*Cost.diagnosis.per.flock
  Cost.diagnosis
  
  Prop.sheep.dipped <- 0.08
  Flocks.dipped <- Prop.sheep.dipped*Flocks
  Flocks.dipped
  
  Prop.dipped.own <- 0.04
  Sheep.dipped.own <- Prop.dipped.own*Sheep.population
  
  Cost.per.flock.disposal.license <-140
  Cost.per.flock.certificate <- 90
  Flocks.dipped.own <- Sheep.dipped.own/Flock.size
  Flocks.dipped.own
  Cost.disposal <- Flocks.dipped.own*Cost.per.flock.disposal.license
  Cost.disposal
  Cost.certificate <- Flocks.dipped.own*Cost.per.flock.certificate
  Cost.certificate
  
  Cost.per.head.dipped.own <- 0.52
  Cost.dipped.own <- Cost.per.head.dipped.own*Sheep.dipped.own
  Cost.dipped.own
  
  Prop.dipped.contract <- 0.04
  Sheep.dipped.contract <- Prop.dipped.contract*Sheep.population
  Cost.per.head.dipped.contract <- 0.7
  Cost.dipped.contract <- Cost.per.head.dipped.contract*Sheep.dipped.contract
  Cost.dipped.contract
  
  Prop.sheep.injected <- 1-Prop.sheep.dipped
  Num.sheep.injected <- Prop.sheep.injected*Sheep.population
  Num.sheep.injected
  Cost.per.head.injected <- 0.65
  Cost.injected <- Num.sheep.injected*Cost.per.head.injected
  Cost.injected
  
  Total.cost <- Cost.injected+Cost.dipped.contract+Cost.dipped.own+Cost.diagnosis+Cost.certificate+Cost.disposal
  Total.cost
  
  #######################################################
  #Now look at disease costs
  Prop.flocks.infected <- 0.165
  Sheep.in.flocks.affected <- Prop.flocks.infected*Sheep.population
  Sheep.in.flocks.affected
  Prop.sheep.infected <-0.075
  Num.sheep.infected <- Prop.sheep.infected*Sheep.in.flocks.affected
  Num.sheep.infected
  
  Prop.lambs.infected <- (45572/94150)
  Prop.lambs.infected
  Prop.ewes.infected <- (48578/94150)
  Prop.ewes.infected
  
  Prop.dying <- 0.01
  Value.of.ewe <- 70
  Value.of.lamb <- 40
  
  Cost.ewes.dead <- Num.sheep.infected*Prop.ewes.infected*Value.of.ewe*Prop.dying
  Cost.ewes.dead
  
  Cost.lambs.dead <- Num.sheep.infected*Prop.lambs.infected*Value.of.lamb*Prop.dying
  Cost.lambs.dead
  
  Prop.weight.lost <- 0.3
  Revenue.lost.lamb.weight <- Prop.weight.lost*Num.sheep.infected*Prop.lambs.infected*Value.of.lamb
  Revenue.lost.lamb.weight
  
  Prop.wool.lost <- 0.25
  Value.wool.per.ewe <- 1.4
  Revenue.lost.ewes.wool <- Prop.wool.lost*Num.sheep.infected*Prop.ewes.infected*Value.wool.per.ewe
  Revenue.lost.ewes.wool
  
  Revenue.lost <- Revenue.lost.lamb.weight+Revenue.lost.ewes.wool+Cost.ewes.dead+Cost.lambs.dead
  Revenue.lost
  
  #######################################################
  #My additional calculations
  Cost.diagnosis.per.head <- Cost.diagnosis.per.flock/Flock.size
  Cost.diagnosis.per.head
  
  Cost.per.head.dipped <- (Prop.dipped.own*Cost.per.head.dipped.own +
                             Prop.dipped.contract*Cost.per.head.dipped.contract)/(Prop.sheep.dipped)
  Cost.per.head.dipped 
  Cost.per.head.injected
  
  Disease.cost.per.infected <- Revenue.lost/(Num.sheep.infected)
  Disease.cost.per.infected
  
  out <- list(Cost.diagnosis.per.head=Cost.diagnosis.per.head,
              Cost.per.head.dipped=Cost.per.head.dipped,
              Cost.per.head.injected=Cost.per.head.injected,
              Disease.cost.per.infected=Disease.cost.per.infected)
  
}

##------------------------------------------------------------------------------------
library(codetools)

findGlobals(ADAS_costs, merge=FALSE)$variables