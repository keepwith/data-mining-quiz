library(MASS)
library(scatterplot3d)
mean<-(c(2,2))
sigma<-matrix(c(2,0,0,2),nrow=2)
x<-mvrnorm(100,mean,sigma)
e<-rnorm(100,0,1)
y<-2+3*x[,1]+1.5*x[,2]+e
s3d<-scatterplot3d(x[,1],x[,2],y,
                   main="Scatter Plot of Y X1 X2",
                   xlab="x1",ylab="x2",zlab="y",
                   scale.y = 0.5)
coef<-coef(lm(y~x[,1]+x[,2]))
s3d$plane3d(coef)

######1000 次的结果
lm.sim<-function(alpha,beta,gamma,
                 xmean=c(2,2),sigma=matrix(c(2,0,0,2),nrow=2),
                 emean=0,evar=1)
{    
  x<-mvrnorm(100,xmean,sigma)
  e<-rnorm(100,emean,evar)
  y<-alpha+beta*x[,1]+gamma*x[,2]+e
  coe<-coef(lm(y~x[,1]+x[,2]))
  return (coe)
}
sigma1=matrix(c(2,0,0,2),nrow=2)
sigma2=matrix(c(2,1,1,2),nrow=2)
sigma3=matrix(c(2,1.9,1.9,2),nrow=2)
coef1=replicate(1000,lm.sim(alpha=2,beta=3,gamma=1.5,
                            xmean=c(2,2),sigma=sigma1,
                            emean=0,evar=1))
coef1<-t(coef1)
boxplot(coef1)
apply(coef1,2,mean)
apply(coef1,2,var)

coef2=replicate(1000,lm.sim(alpha=2,beta=3,gamma=1.5,
                            xmean=c(2,2),sigma=sigma2,
                            emean=0,evar=1))
coef2<-t(coef2)
boxplot(coef2)
apply(coef2,2,mean)
apply(coef2,2,var)

coef3=replicate(1000,lm.sim(alpha=2,beta=3,gamma=1.5,
                            xmean=c(2,2),sigma=sigma3,
                            emean=0,evar=1))
coef3<-t(coef3)
boxplot(coef3)
apply(coef3,2,mean)
apply(coef3,2,var)


###############
beta0<-NULL
beta1<-NULL
beta2<-NULL
for(i in 1:1000){
  x<-mvrnorm(100,mu=c(1,1),Sigma=sigma)
  x1<-x[,1]
  x2<-x[,2]
  e<-rnorm(100,0,sqrt(2))
  y<-2+3*x1+1.5*x2+e
  beta<-coef(lm(y~x1+x2))
  beta0[i]=beta[1]
  beta1[i]=beta[2]
  beta3[i]=beta[3]
}

boxplot(beta 0)
boxplot(beta 1)
boxplot(beta 2)
m<-cbind(beta0,beta1,beta2)
apply(m,2,mean)
apply(m,2,median)


################
lm<-sim2<-function(alpha,beta,sigma,emean=0,evar=1)
{
  x<-mvrnorm(100,mu=c(1,1).Sigma=sigma)
  e<-rnorm(100,emean,evar)
  y<-alpha+x%*%beta+e
  lm.re<-lm(y~x)
  coe<-coef(lm.re)
  return (coe)
}

lm.sim2(alpha=2,beta=c(3,1,5),sigma=matrix(c(2,1,1,2),2,2))
sigma<-matrix(c(2,1.95,1.95,2),2,2)
coef2<-replicate(1000,lm.sim2(alpha=2,beta=c(3,1.5),sigma=sigma))
summary(t(coef2))
apply(t(coef2),2,var)
boxplot(t(coef2),col=2:4)



