installed.packages("neuralnet")
library(neuralnet)
#XOR gate input data
trainin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));
#XOR gate output data
trainout = rbind(0, 1, 1, 0);
#Combined XOR gate data
XORdat=cbind(trainout,trainin)
#train a neural network on the XOR data
set.seed(2)
NN = neuralnet(XORdat[,1]~., XORdat[,-1], hidden = c(3,3) , threshold = 0.001, stepmax = 1e+05, linear.output = FALSE) #change the number of hidden layer
#visualise the NN
plot(NN)
#train it
testin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1,-1));
testout=rbind(0,1,1,0)
predict_testNN = compute(NN, testin)
predict_testNN$neurons
predict_testNN$net.result
predict_out = as.numeric(predict_testNN$net.result>0.5)
predict_out
#Q2
#train wine
winedata = read.csv('C:\\Users\\noriss\\Documents\\Brunel\\Year3\\3002 AI\\winedata2.csv',sep=",")
winedata=na.omit(winedata)
winein = winedata$Alcohol[1:65]
winetrainin=cbind(winein)
wineout = winedata$WineClass[1:65]
winetrainout=cbind(wineout)

winetrain=cbind(winetrainout,winetrainin)
NN = neuralnet(winetrain[,1]~., winetrain[,-1], hidden = c(3,3) , threshold = 0.001, stepmax = 1e+05, linear.output = FALSE)

#test wine
winetest = winedata$Alcohol[66:130]
winetestin = cbind(winetest)
wineout=winedata$WineClass-1
wineout = wineout[66:130]
winetestout=cbind(wineout)

predict_winetestNN = compute(NN,winetestin)
predict_winetestNN$neurons
predict_winetestNN$net.result
predict_wineout = as.numeric(predict_winetestNN$net.result>0.5)
predict_wineout
accuracy =0
for(x in 1:65)
{
  if(winetestout[x]==predict_wineout[x])
  accuracy=accuracy+1
}
accuracy=accuracy/65
print(accuracy)
