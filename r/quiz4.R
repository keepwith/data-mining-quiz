library(ggplot2)
mu1<--1.5
mu2<-1.5
sd<-1
x1<-data.frame(x=rnorm(n=100,mean=mu1,sd=sd))
x2<-data.frame(x=rnorm(n=100,mean=mu2,sd=sd))
xbar1<-mean(x1$x)
xbar2<-mean(x2$x)
x1$class<-"1"
x2$class<-"2"
dat<-rbind(x1,x2)
bp=ggplot(dat,aes(x,fill=class))+geom_histogram(alpha=0.5,binwidth=0.6)
bp1=bp+geom_vline(aes(xintercept=(xbar1+xbar2)/2))
bp1+geom_vline(aes(xintercept=(mu1+mu2)/2),linetype="dashed")



library(MASS)
x1<-data.frame(label=rep(0,100),data=rnorm(100,-1.5,1))
x2<-data.frame(label=rep(1,100),data=rnorm(100,1.5,1))
x<-rbind(x1,x2)
hist(x1$data,col=2,xlim=c(-5,5),ylim=c(0,30),xlab="data",main="Histogram",breaks = seq(-5,5,0.5))
hist(x2$data,col=3,xlim=c(-5,5),ylim=c(0,30),xlab="data",main="Histogram",density=10,breaks = seq(-5,5,0.5),add=TRUE)
m<-lda(label~.,x)$scaling
par(new=TRUE)
abline(v=m,col=1,lty=2)





library(kknn)
library(MASS)
data.g<-function(n,mu1=c(0,0),mu2=c(3,4),Sigma1,Sigma2)
{
  x1<-mvrnorm(n=100,mu1,Sigma1)
  x2<-mvrnorm(n=100,mu2,Sigma2)
  x<-rbind(x1,x2)
  y<-as.factor(rep(c(0,1),each=100))
  data<-data.frame(x,y)
  return (data)
}


predic<-function(test,Sigma1,Sigma2){
  pred<-NULL
  train<-data.g(200,Sigma1=Sigma1,Sigma2=Sigma2)
  x<-train[,1:2]
  y<-train[,3]
  pred[1]<-mean(knn(x,test[,1:2],y,k=1)!=test[,3])
  cv.kknn=train.kknn(y~.,data=train,kernel="rectangular",kmax=15,kcv=5)
  pred[2]<-mean(knn(x,test[,1:2],y,k=cv.kknn$best.parameter$k)!=test[,3])
  glmf<-glm(y~.,family=binomial,data=train)
  pglmp<-predict(glmf,test[,1:2],type="response")
  pglm<-ifelse(pglmp>0.5,1,0)
  pred[3]<-mean(pglm!=test[,3])
  pred[4]<-mean(predict(lda(y~.,data=train),newdata=test[,1:2])$class!=test[,3])
  pred[5]<-mean(predict(qda(y~.,data=train),newdata=test[,1:2])$class!=test[,3])
  return (pred)
}

Sigma<-matrix(c(2,0,0,2),2)
test<-data.g(100,mu1=c(0,0),mu2=c(3,4),Sigma1=Sigma,Sigma2=Sigma)
pre<-t(replicate(200,predic(test,Sigma,Sigma)))
colnames(pre)<-c("knn-1","knn-cv","logistic","lda","qda")
boxplot(pre,col=2:6)

Sigma<-matrix(c(2,1,1,2),2)
test<-data.g(100,mu1=c(0,0),mu2=c(3,4),Sigma1=Sigma,Sigma2=Sigma)
pre<-t(replicate(200,predic(test,Sigma,Sigma)))
colnames(pre)<-c("knn-1","knn-cv","logistic","lda","qda")
boxplot(pre,col=2:6)


