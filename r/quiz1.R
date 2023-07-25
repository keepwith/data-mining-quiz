lm.sim<-function(alpha,beta,xmean=2,xvar=2,emean=0,evar=1)
{
  x<-rnorm(100,xmean,sqrt(xvar))
  e<-rnorm(100,emean,sqrt(evar))
  y<-alpha+beta*x+e
  lm.re<-lm(y~x)
  coe<-coef(lm.re)
  return(coe)
}

lm.sim(alpha=2,beta=3)
coef1<-replicate(100,lm.sim(alpha=2,beta=3))
t(coef1)
boxplot(t(coef1),col=2:3)
summary(t(coef1))
