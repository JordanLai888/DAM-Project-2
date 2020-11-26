
#####################SIMULATION####################

RiskNeutral<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0) #18
RiskAdverse<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0) #17
RiskProne<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0) #19

simulation<-function(ProbY){
  Total<-0
  TurnCounter<-0
  while (Total<21){
    TurnCounter<-TurnCounter+1
    RandomNumberGenerator<-sample(1:6,1)
    if (Total==0){
      Total<-Total+RandomNumberGenerator
    }
    if (ProbY[Total]==1){
      Total<-Total+RandomNumberGenerator
    }
    if (ProbY[Total]==0){
      return(Total)
    }
  }
}
simulation(RiskNeutral)


SimulationResults<-cbind(rep(0,1000000))
for (i in 1:1000000){
  SimulationResults[i]<-simulation(RiskAdverse)
}
table(SimulationResults)

SimulationProbabilities<-as.matrix(cbind(table(SimulationResults)[1]/1000000,
  table(SimulationResults)[2]/1000000,
  table(SimulationResults)[3]/1000000,
  table(SimulationResults)[4]/1000000,
  table(SimulationResults)[5]/1000000,
  table(SimulationResults)[6]/1000000
))
SimulationProbabilities

#Prints the final 100 values instead of all of them as it takes too long
ProbabilityEqulibriumValues<-function(ProbY,n){
  SimulationResults1<-as.matrix(cbind(rep(0,20),rep(0,20),rep(0,20),rep(0,20),rep(0,20),rep(0,20)))
  for (j in (n-20):n){
    SimulationResults<-cbind(rep(0,j))
    for (i in 1:j){
      SimulationResults[i]<-simulation(ProbY)
    }
    SimulationResults1[j-n-19,1]<-as.numeric(table(SimulationResults)[1]/j)
    #SimulationResults1[j,2]<-as.numeric(table(SimulationResults)[2]/j)
    #SimulationResults1[j,3]<-as.numeric(table(SimulationResults)[3]/j)
    #SimulationResults1[j,4]<-as.numeric(table(SimulationResults)[4]/j)
    #SimulationResults1[j,5]<-as.numeric(table(SimulationResults)[5]/j)
    #SimulationResults1[j,6]<-as.numeric(table(SimulationResults)[6]/j)
  }
    SimulationResults1[is.na(SimulationResults1)] <- 0
    print(SimulationResults1)
    
    par(mfrow=c(1,1))
    GraphForY1<-plot(x<-seq(1,10),y<-tail(SimulationResults1[,1],10),xlab="Number Of Trials",
                     ylab="Prob. of achieving 18")
    print(GraphForY1)
    #GraphForY2<-plot(x<-seq(1,100),y<-tail(SimulationResults1[,2],100),xlab="Number Of Trials",
                     #ylab="Prob. of achieving Y2")
    #print(GraphForY2)
    #GraphForY3<-plot(x<-seq(1,100),y<-tail(SimulationResults1[,3],100),xlab="Number Of Trials",
                     #ylab="Prob. of achieving Y3")
    #print(GraphForY3)
    #GraphForY4<-plot(x<-seq(1,100),y<-tail(SimulationResults1[,4],100),xlab="Number Of Trials",
                     #ylab="Prob. of achieving Y4")
    #print(GraphForY4)
    #GraphForY5<-plot(x<-seq(1,100),y<-tail(SimulationResults1[,5],100),xlab="Number Of Trials",
                     #ylab="Prob. of achieving Y5")
    #print(GraphForY5)
    #GraphForY6<-plot(x<-seq(1,100),y<-tail(SimulationResults1[,6],100),xlab="Number Of Trials",
                     #ylab="Prob. of achieving Y6")
    #print(GraphForY6)
}
ProbabilityEqulibriumValues(RiskNeutral,1000000)
