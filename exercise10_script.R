setwd("~/Documents/junior/ICB_HW/ICB2019_Exercise10/")

#constants defined by the problem
rN <- 0.1
rM <- 0.1
K <- 1000000
timesteps <- 1000
M0 <- 0
N0 <- 1
#create vectors
N=numeric(length=timesteps)
M=numeric(length=timesteps)
N[1] <- N0
M[1] <- M0

#simulate growth of nonmutant cells
for (t in 1:(timesteps-1)){
  N[t+1] <- N[t] + (rN*N[t]*(1-((N[t]+M[t])/K)))
  if (N[t]>=100){
    N[t] <- 99
    M[t] <- 1
    x <- t
    break
  }
}
#first mutation occurs, simulate growth of both mutant and
# nonmutant cells until equilibrium
for (t in x:(timesteps-1)){
  N[t+1] <- N[t] + (rN*N[t]*(1-((N[t]+M[t])/K)))
  M[t+1] <- M[t] + (rM*M[t]*(1-((N[t]+M[t])/K)))
  if (M[t] == M[t-1]){
    y <- t
    break
  }
}
#simulate drug treatment
rN <- -0.1
for (t in y:(timesteps-1)){
  N[t+1] <- N[t] + (rN*N[t]*(1-((N[t]+M[t])/K)))
  M[t+1] <- M[t] + (rM*M[t]*(1-((N[t]+M[t])/K))*0.5)
}
#plot
library(ggplot2)
library(reshape2)
df<-data.frame(time=1:timesteps,nonmutant=N)
df2<-data.frame(time=1:timesteps,mutant=M)
cells <- merge(df,df2,by="time")
cellsMelted <- reshape2::melt(cells, id.var='time')
ggplot(data=cellsMelted,aes(x=time,y=value,col=variable)) + 
  geom_line() +
  labs(y = "number of cells", color = "type of cell",
  title = "number of mutant vs. nonmutant cells over time") +
  theme_classic()
  
  
  
  
  
  
