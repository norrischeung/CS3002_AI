winedata = read.csv('C:\\Users\\noriss\\Documents\\Brunel\\Year3\\3002 AI\\winedata.csv',sep=",")
wineclass = winedata[,1] #seperate class into wineclass
winevalues = winedata[,-1] #seperate value into winevalues
#set up training set (1-100)
wineclassTrain = wineclass[1:100]
winevaluesTrain = winevalues[1:100,]
#test set (101-178)
wineclassTest = wineclass[100:178]
winevaluesTest = winevalues[100:178,]
#decision tree
#install.packages("rpart")
library(rpart)
fit<-rpart(wineclassTrain~.,method = "class", data = winevaluesTrain) #predict wineclass using all other variable
plot(fit, uniform = TRUE, main="Decision Tree for WineData3")
text(fit, use.n = TRUE, all = TRUE, cex=.8)
#test classifier on the test set
treepred<-predict(fit, winevaluesTest, type = 'class') #calculate the prediction for each testcase
#compare to the actual test class values
n = length(wineclassTest) #no. of test case
ncorrect = sum(treepred==wineclassTest) #no.of correctly predicted
accuracy = ncorrect/n
print(accuracy)
#confusion matrix
table_mat = table(wineclassTest, treepred)
print(table_mat)
#pruning (cp=0.1)
pfit<-prune(fit, cp=0.1)
plot(pfit, uniform = TRUE, main = "Pruned Decision Tree for WineData3")
text(pfit, use.n = TRUE, cex=.8)
#K nearest neighbour (KNN)
library(class)
knn3pred = knn(winevaluesTrain, winevaluesTest, wineclassTrain, k=3) #generate predict class
#calculate accuracy
n = length(wineclassTest) #no. of test cases
ncorrect = sum(knn3pred==wineclassTest) #no. of correctly predicted
accuracy = ncorrect/n
print(accuracy)
