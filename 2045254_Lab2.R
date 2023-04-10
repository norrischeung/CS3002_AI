seeds = read.csv('C:\\Users\\noriss\\Documents\\Brunel\\Year3\\3002 AI\\seeds_dataset_class.csv',sep=",")
#Q1
seeds_rand = seeds[sample(150,150),] #mix up the order
seedsclass = seeds_rand[,1] #seperate class into wineclass
seedsvalues = seeds_rand[,-1] #seperate value into winevalues
#set up training set (1-100)
seedsclassTrain = seedsclass[1:100]
seedsvaluesTrain = seedsvalues[1:100,]
#test set (101-150)
seedsclassTest = seedsclass[101:150]
seedsvaluesTest = seedsvalues[101:150,]
#decision tree
library(rpart)
fit<-rpart(seedsclassTrain~.,method = "class", data = seedsvaluesTrain) #predict seedsclass using all other variable
plot(fit, uniform = TRUE, main="Decision Tree for SeedsData")
text(fit, use.n = TRUE, all = TRUE, cex=.8) 
#test classifier on the test set
treepred<-predict(fit, seedsvaluesTest, type = 'class') #calculate the prediction for each testcase
#compare to the actual test class values
n = length(seedsclassTest) #no. of test case
ncorrect = sum(treepred==seedsclassTest) #no.of correctly predicted
accuracy = ncorrect/n
print(accuracy)
#confusion matrix
table_mat = table(seedsclassTest, treepred)
print

#Q2
arr= array(dim=c(1,5))
for (i in 1:5) {
  pfit<-prune(fit, cp=i/10)
  plot(pfit, uniform = TRUE, main = "Pruned Decision Tree for SeedsData3")
  text(pfit, use.n = TRUE, cex=.8)
  p_treepred<-predict(pfit, seedsvaluesTest, type = 'class')
  p_n = length(seedsclassTest) #no. of test case
  p_ncorrect = sum(p_treepred==seedsclassTest) #no.of correctly predicted
  p_accuracy = p_ncorrect/p_n
  arr[1,i]=p_accuracy
}
print(arr)

#Q3
#Scatterplot
plot(seeds$Area,seeds$Perimeter,col=treepred)

#K nearest neighbour (KNN)
library(class)

#Q4
arr= array(dim=c(2,5))
for (i in 1:5) {
  pfit<-prune(fit, cp=i/10)
  plot(pfit, uniform = TRUE, main = "Pruned Decision Tree for SeedsData3")
  text(pfit, use.n = TRUE, cex=.8)
  p_treepred<-predict(pfit, seedsvaluesTest, type = 'class')
  p_n = length(seedsclassTest) #no. of test case
  p_ncorrect = sum(p_treepred==seedsclassTest) #no.of correctly predicted
  p_accuracy = p_ncorrect/p_n
  arr[1,i]=p_accuracy
  knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=i) #generate predict class
  n_knn = length(seedsclassTest) #no. of test cases
  ncorrect_knn = sum(knn3pred==seedsclassTest) #no. of correctly predicted
  accuracy_knn = ncorrect_knn/n_knn
  arr[2,i]=accuracy_knn
}
print(arr)
