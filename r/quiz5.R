library(ISLR)
library(MASS)
data(Auto)
p<-seq(1,10)
mse<-NULL
n<-dim(Auto)[1]
sam<-sample(n,0.5*n)
train_set<-Auto[sam,]
test_set<-Auto[-sam,]
for (i in 1:10)
{
  polm<-lm(mpg~poly(horsepower,p[i]),train_set)
  pred<-predict(polm,test_set)
  mse[i]<-sum((pred-test_set$mpg)^2)/n/0.5
}
plot(p,mse,xlim=c(0,10),ylim=c(15,30),type="b",col="blue",
     xlab="Degree of Polynomial",ylab="Mean Square Error",main="Validation Process")



plot(c(0,10),c(15,30),type="n",xlab="Degree of Polynomial"
     ,ylab="Mean Square Error",main="Validation Process")
validation<-function(times)
{
  p<-seq(1,10)
  mse<-NULL
  n<-dim(Auto)[1]
  sam<-sample(n,0.5*n)
  train_set<-Auto[sam,]
  test_set<-Auto[-sam,]
  for (i in 1:10)
  {
    polm<-lm(mpg~poly(horsepower,p[i]),train_set)
    pred<-predict(polm,test_set)
    mse[i]<-sum((pred-test_set$mpg)^2)/n/0.5
  }
  lines(p,mse,col=times+1)
  return (mse);
}
for (j in 1:10)
  validation(j)


library(ISLR)
library(boot)
data(Auto)
n=dim(Auto)[1]
mse<-NULL
p<-seq(1,10)
for (i in 1:10)
{
  polm<-glm(mpg~poly(horsepower,p[i]),data=Auto)
  cv.err <- cv.glm (Auto, polm,K=n)
  mse[i]<-cv.err$delta[1]
}
plot(p,mse,type="b",xlim=c(0,10),ylim=c(15,30),xlab="Degree of Polynomial"
     ,ylab="Mean Square Error",main="LOOCV",col="blue")



library(ISLR)
library(boot)
data(Auto)
plot(c(0,10),c(15,30),type="n",xlab="Degree of Polynomial"
     ,ylab="Mean Square Error",main="5-fold CV")
k_folds<-function(times,k)
{
  mse<-NULL
  p<-seq(1,10)
  for (i in 1:10)
  {
    polm<-glm(mpg~poly(horsepower,p[i]),data=Auto)
    cv.err <- cv.glm (Auto, polm,K=k)
    mse[i]<-cv.err$delta[1]
  }
  lines(p,mse,col=times+1)
  return(mse)
}
for (j in 1:10 )
  k_folds(j,5)


library(ISLR)
library(MASS)
pop.sim<-function()
{
  data<-(mvrnorm(100,mu=c(0,0),Sigma=matrix(data=c(1,0.5,0.5,1.25),nrow=2)))
  alpha<-(var(data[,2])-var(data[,1],data[,2]))/(var(data[,1])+var(data[,2])-2*var(data[,1],data[,2]))
  return (alpha)
}
data(Portfolio)
alpha<-NULL
alpha<-replicate(1000,pop.sim())
hist(alpha,col="darkgoldenrod1",main="Simulation of population")
abline(v=0.6,col="orchid1",lwd=4)



bootstrap<-function()
{
  data(Portfolio)
  n<-dim(Portfolio)[1]
  sam<-sample(n,replace=TRUE)
  data<-Portfolio[sam,]
  alpha<-(var(data[,2])-var(data[,1],data[,2]))/(var(data[,1])+var(data[,2])-2*var(data[,1],data[,2]))
  return (alpha)
}
alpha1<-NULL
alpha1<-replicate(1000,bootstrap())
hist(alpha1,col="steelblue1",main="bootstrap of portforlio")
abline(v=0.6,col="orchid1",lwd=4)




boxplot(alpha,alpha1,names=c("TRUE","Bootstrap"),col=c("darkgoldenrod1","steelblue1"),main="boxplot of two scenarios")
abline(0.6,0,col="orchid1",lwd=4)


