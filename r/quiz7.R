library(MASS)
library(glmnet)
library(ncvreg)

data_gen<-function(Rho)
{
  rho<-Rho
  sigma<-matrix(nrow=100,ncol=100)
  for (i in 1:100)
    for (j in 1:100)
      sigma[i,j]=rho^abs(i-j)
  Mu<-rep(0,times=100)
  X<-mvrnorm(n=100,mu=Mu,Sigma=sigma)
  beta<-rep(0,times=100)
  for(i in 1:5)
    beta[i]=1
  for(i in 6:10)
    beta[i]=0.5
  e<-rnorm(100,0,1)
  y=X%*%beta+e
  return (cbind(y,X))
}

TPR<-function(coef)
{
  tp<-0
  for(i in 2:11)
    if (coef[i]!=0)
      tp<-tp+1
  tpr<-tp/10
  return (tpr)
}

FPR<-function(coef)
{
  fp<-0
  for(i in 12:101)
    if (coef[i]!=0)
      fp<-fp+1
  fpr<-fp/90
  return(fpr)
}

tpr<-matrix(data=rep(0,18),ncol=6,nrow=3)
fpr<-matrix(data=rep(0,18),ncol=6,nrow=3)
predict_mse<-matrix(data=rep(0,18),ncol=6,nrow=3)
rho<-c(0.1,0.5,0.9)
for (p in 1:100)
{
  for(i in 1:3)
  {
    train_y<-NULL
    train_x<-NULL
    train_data<-data_gen(rho[i])
    train_y<-train_data[,1]
    train_x<-train_data[,-1]
    test_y<-NULL
    test_x<-NULL
    test_data<-data_gen(rho[i])
    test_y<-test_data[,1]
    test_x<-test_data[,-1]
    grid<-10^seq(10,-2,length=100)
    
    j<-1
  #ridge
    ridge<-cv.glmnet(train_x,train_y,alpha=0,lambda=grid)
    predict_mse[i,j]<-predict_mse[i,j]+mean((predict(ridge,test_x)-test_y)^2)
    j<-j+1
    
  #lasso
    lasso<-cv.glmnet(train_x,train_y,alpha=1,lambda=grid)
    tpr[i,j]<-tpr[i,j]+TPR(coef(lasso))
    fpr[i,j]<-fpr[i,j]+FPR(coef(lasso))
    predict_mse[i,j]<-predict_mse[i,j]+mean((predict(lasso,test_x)-test_y)^2)
    j<-j+1
  
  #adaptive lasso
    w<-1/abs(coef(lasso)[-1])
    adalasso<-cv.glmnet(train_x,train_y,alpha=1,penalty.factor=w)
    tpr[i,j]<-tpr[i,j]+TPR(coef(adalasso))
    fpr[i,j]<-fpr[i,j]+FPR(coef(adalasso))
    predict_mse[i,j]<-predict_mse[i,j]+mean((predict(adalasso,test_x)-test_y)^2)
    j<-j+1
    
  #elastic-net
    elastic<-cv.glmnet(train_x,train_y,alpha=0.5,lambda=grid)
    tpr[i,j]<-tpr[i,j]+TPR(coef(elastic))
    fpr[i,j]<-fpr[i,j]+FPR(coef(elastic))
    predict_mse[i,j]<-predict_mse[i,j]+mean((predict(elastic,test_x)-test_y)^2)
    j<-j+1
    
  
  #mcp
    mcp<-cv.ncvreg(train_x,train_y,family="gaussian",penalty="MCP")
    tpr[i,j]<-tpr[i,j]+TPR(coef(mcp))
    fpr[i,j]<-fpr[i,j]+FPR(coef(mcp))
    predict_mse[i,j]<-predict_mse[i,j]+mean((predict(mcp,test_x)-test_y)^2)
    j<-j+1
  
  #scad
    scad<-cv.ncvreg(train_x,train_y,family="gaussian",penalty="SCAD")
    tpr[i,j]<-tpr[i,j]+TPR(coef(scad))
    fpr[i,j]<-fpr[i,j]+FPR(coef(scad))
    predict_mse[i,j]<-predict_mse[i,j]+mean((predict(mcp,test_x)-test_y)^2)
    j<-j+1
  }
}

tpr<-tpr/100
fpr<-fpr/100
predict_mse<-predict_mse/100

colnames(tpr)=c("ridge","lasso","adapt-lasso","elastic-net","MCP","SCAD")
colnames(fpr)=c("ridge","lasso","adapt-lasso","elastic-net","MCP","SCAD")
colnames(predict_mse)=c("ridge","lasso","adapt-lasso","elastic-net","MCP","SCAD")
rownames(tpr)=c("rho=0.1","rho=0.5","rho=0.9")
rownames(fpr)=c("rho=0.1","rho=0.5","rho=0.9")
rownames(predict_mse)=c("rho=0.1","rho=0.5","rho=0.9")
tpr
fpr
predict_mse

boxplot(tpr[,-1],col=2:6,main="TPR")
boxplot(fpr[,-1],col=2:6,main="FPR")
boxplot(predict_mse,col=2:7,main="Predict_MSE")


