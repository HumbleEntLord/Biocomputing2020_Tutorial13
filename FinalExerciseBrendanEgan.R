## N(t+1)=Nt+rnNt(1-(Nt+Mt)/K)
## M(t+1)=Mt+rmMt(1-(Nt+Mt)/K)

rn = 0.1 #Growth of non-resistant cells without drug
rm = 0.1 #Growth of resistant cells without drug
rnd = -0.1 #Growth of non-resistant cells in presence of drug
rmd = 0.05 # Growth of resistant cells (50%) in presence of drug
K = 1000000 

# time horizon
times<- 1:400
#Store output
output <-matrix(data=NA,nrow = length(times),ncol = 4)
output[,1]=times

#Initial population sizes
output[1,2]=99 
output[1,3]=1
output[1,4]=100
#For loop to iteratively update the population
for(i in times[-1]){ ##times[-1] gets rid of only the first entry, making output[(i-1),2] not produce an error
  if(times[i]<=150){ #before drug introduction
  output[i,2]=output[(i-1),2]+rn*output[(i-1),2]*(1-output[(i-1),4]/K)
  output[i,3]=output[(i-1),3]+rm*output[(i-1),3]*(1-output[(i-1),4]/K)
  output[i,4]=output[i,2]+output[i,3]
}else{ #drug introduced after time 150
  output[i,2]=output[(i-1),2]+rnd*output[(i-1),2]*(1-output[(i-1),4]/K)
  output[i,3]=output[(i-1),3]+rmd*output[(i-1),3]*(1-output[(i-1),4]/K)
  output[i,4]=output[i,2]+output[i,3]
}
}
#Output dataframe
outputDF<-data.frame(time=output[,1],nonresistant=output[,2],mutant=output[,3],total=output[,4])

tail(outputDF)

#Time to plot
library(ggplot2)
ggplot()+
  geom_line(data = outputDF, aes(x=time,y=nonresistant),color='blue')+ #nonresistant strain line blue
  geom_line(data = outputDF, aes(x=time,y=mutant),color='red')+ #resistant strain line red
  geom_line(data = outputDF, aes(x=time,y=total),color='black')+ #total line black
  xlab('time')+
  ylab('cells')



