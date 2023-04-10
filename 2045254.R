mydata = read.csv('C:\\Users\\noriss\\Documents\\Brunel\\Year3\\3002 AI\\seeds_dataset.csv',sep=",") #read data
plot(mydata) #plot graph
#Hierarchical
mydata = na.omit(mydata) # deletion of missing data
mydata = scale(mydata) # standardize variables
d<-dist(mydata,method = "euclidean") #distance matrix
fit<-hclust(d,method="average") #hclust to perform clustering
plot(fit) #plot dendogram
Hgroups<-cutree(fit,k=3) #cut tree into 3 cluster
rect.hclust(fit, k=3, border="red") #draw the dendrogram with red borders
plot(mydata, col=Hgroups) #plot scatterplot
#kmean
for(i in 2:5)
{
  fit<-kmeans(mydata,i) #3 cluster solution
}
Kgroups = fit$cluster 
plot(mydata, col=Kgroups)#plot scatterplot
source("C:\\Users\\noriss\\Documents\\Brunel\\Year3\\3002 AI\\WK_R.r")
#wk = WK_R(Kgroups,Hgroups)

#Q1 
seedreal = read.csv('C:\\Users\\noriss\\Documents\\Brunel\\Year3\\3002 AI\\seeds_real.csv',sep=",") #read data
fit_realk<-kmeans(seedreal,3) #3 cluster solution
real_k = fit_realk$cluster
wk1 = WK_R(Kgroups,real_k)
#Q2
wk2 = WK_R(Hgroups,seedreal$Real)
wk2

length(Hgroups)
length(seedreal$Real)
#Q5
wk_data = read.csv('C:\\Users\\noriss\\Documents\\Brunel\\Year3\\3002 AI\\lab1_wk.csv',sep=",")
plot(wk_data)