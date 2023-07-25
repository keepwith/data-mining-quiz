library(MASS)
library(randomForest)
library(fastAdaboost)
library(tree)
library(gbm)
data_gen<-function(n=2000)
{
  X<-data.frame(mvrnorm(n=n,mu=rep(0,10),Sigma=diag(10)))
  Y<-data.frame(apply(X^2,1,sum))
  Y<-ifelse(Y<=qchisq(p=0.5,df=10),-1,1)
  Y[Y>=qchisq(p=0.5,df=10)]<-1
  Y<-factor(Y,levels=c("-1","1"))
  return (cbind(Y,X))
}


err<-matrix(data=rep(0,1000),ncol=200)
train<-data_gen(n=2000)
test<-data_gen(n=10000)
for(i in 1:200)
{

  
  #single tree
  singletree<-tree(Y~.,data=train,split = c("deviance","gini"))
  pred<-predict(singletree,test,type="class")
  table(test$Y,pred)
  err[1,i]=sum(test$Y!=pred)/dim(test)[1]
  
  #single stump
  singlestump<-prune.tree(singletree,best=2)
  pred<-predict(singlestump,test,type="class")
  table(test$Y,pred)
  err[2,i]=sum(test$Y!=pred)/dim(test)[1]
  
  #bagging
  bag<-randomForest(Y~.,data=train,mtry=10,ntree=i)
  pred<-predict(bag,test,type="class")
  table(test$Y,pred)
  err[3,i]=sum(test$Y!=pred)/dim(test)[1]
  
  #randomforest
  rf<-randomForest(Y~.,data=train,ntree=i)
  pred<-predict(rf,test,type="class")
  table(test$Y,pred)
  err[4,i]=sum(test$Y!=pred)/dim(test)[1]
  
  #adaboostm1
  boost<-adaboost(Y~.,data=train,nIter=i)
  pred<-predict(boost,test,type="class")
  table(test$Y,pred$class)
  err[5,i]=sum(test$Y!=pred$class)/dim(test)[1]
}

xindex<-seq(1,200)
plot(xindex,err[1,],type="l",ylim=c(0,0.5),lty=2,col=2,xlab="number of trees",ylab="err")
lines(xindex,err[2,],lty=2,col=3)
lines(xindex,err[3,],col=4)
lines(xindex,err[4,],col=5)
lines(xindex,err[5,],col=6)
legend("topright",
       legend=c("single tree","single stump","bagging","randomforest","boosting"),
       col=c(2:6),
       lty=1,
       lwd=2,
       cex=0.8)

