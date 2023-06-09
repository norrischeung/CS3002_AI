mydata=read.csv('C:\\Users\\noriss\\Documents\\Brunel\\Year3\\AI\\forestfires.csv', sep=",")
plot(mydata)
plot(mydata$temp,mydata$wind)
plot(mydata[,9],mydata[,11])
hist(mydata$temp)
plot(mydata$temp,type="l")
plot(mydata$X,mydata$Y,col=mydata$temp)
meantemp=mean(mydata$temp)
write.csv(meantemp,file="Output.csv")
plot(mydata$temp,mydata$ISI)
lmfire=line(mydata$ISI~mydata$temp)
abline(coef(lmfire))
