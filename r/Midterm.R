library(MASS)
X_gen<-function(n=10,p=5)
{
  rho<-0.75
  sigma<-matrix(nrow=p,ncol=p)
  for (i in 1:p)
    for (j in 1:p)
      sigma[i,j]=rho^abs(i-j)
  Mu<-rep(0,times=p)
  X<-mvrnorm(n=n,mu=Mu,Sigma=sigma)
  return (X)
}

test_gen<-function(n=100,p=200)
{
  beta<-c(rep(2,5),0.5,0.6,0.7,0.8,0.9,rep(0,190))
  e<-rnorm(n,0,1)
  X<-X_gen(n,p)
  y<-X%*%beta+e
  return (cbind(y,X))
}
test_data<-test_gen(n=100,p=200)

train_data<-test_gen(n=100,p=200)
library(leaps)
regfit.full<-regsubsets(train_data[,1]~.,data=data.frame(train_data),nvmax=50,method="forward")
reg.summary<-summary(regfit.full)
loc<-which.min(reg.summary$bic[1:80])
coef(regfit.full,loc)

TPR<-function(coef)
{
  tp<-0
  for(i in 2:11)
    if (coef[i]!=0)
      tp<-tp+1
    tpr<-tp/10
    return (tpr)
}

sub<-function()
{
  train_data<-test_gen(n=100,p=200)
  library(leaps)
  regfit.full<-regsubsets(train_data[,1]~.,data=data.frame(train_data),nvmax=200,method="forward")
  reg.summary<-summary(regfit.full)
  loc<-which.min(reg.summary$bic[1:80])
  return(coef(regfit.full,loc))
}

tpr<-NULL
fnr<-NULL
for(i in 1:100)
{
  tpr[i]<-TPR(sub())
  fnr[i]<-1-tpr[i]
}
mean(tpr)
sd(tpr)
mean(fnr)
sd(fnr)

library(glmnet)
library(ncvreg)
tpr<-matrix(data=rep(0,300),ncol=3)
fnr<-matrix(data=rep(0,300),ncol=3)
for (p in 1:100)
{
  train_data<-test_gen(n=100,p=200)
  train_y<-train_data[,1]
  train_x<-train_data[,-1]
  grid<-10^seq(10,-2,length=100)
  j<-1
  #ridge
  ridge<-cv.glmnet(train_x,train_y,alpha=0,lambda=grid,nfolds=5)
  
  #lasso
  lasso<-cv.glmnet(train_x,train_y,alpha=1,lambda=grid,nfolds=5)
  tpr[p,j]<-TPR(coef(lasso))
  fnr[p,j]<-(1-tpr[p,j])
  j<-j+1
  
  #adaptive lasso
  w<-1/abs(coef(lasso)[-1])
  adalasso<-cv.glmnet(train_x,train_y,alpha=1,penalty.factor=w,nfolds=5)
  tpr[p,j]<-TPR(coef(adalasso))
  fnr[p,j]<-(1-tpr[p,j])
  j<-j+1
  
  #mcp
  mcp<-cv.ncvreg(train_x,train_y,family="gaussian",penalty="MCP",nfolds=5)
  tpr[p,j]<-TPR(coef(mcp))
  fnr[p,j]<-(1-tpr[p,j])
}

apply(tpr,2,mean)
apply(tpr,2,sd)

apply(fnr,2,mean)
apply(fnr,2,sd)

colnames(tpr)=c("lasso","adapt-lasso","MCP")
colnames(fnr)=c("lasso","adapt-lasso","MCP")
boxplot(tpr)
boxplot(fnr)