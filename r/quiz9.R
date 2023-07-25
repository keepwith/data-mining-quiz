library (splines)
library(ISLR)
attach(Wage)
agelims<-range(age)
age.grid<-seq(from=agelims[1],to=agelims[2])
fit=lm(wage~ns(age,df=4),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=TRUE)
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Cubic Spline")
lines(age.grid,pred$fit,lwd =2,col=2)
lines(age.grid,pred$fit+2*pred$se ,lty ="dashed",col=2)
lines(age.grid,pred$fit-2*pred$se ,lty ="dashed",col=2)


quantile(age,probs=c(0.25,0.5,0.75))
fit=lm(wage~bs(age,knots=c(33.75,42.00,51.00)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=TRUE)
lines(age.grid,pred$fit,lwd =2,col=3)
lines(age.grid,pred$fit+2*pred$se ,lty ="dashed",col=3)
lines(age.grid,pred$fit-2*pred$se ,lty ="dashed",col=3)

fit=smooth.spline(age,wage,cv=TRUE)
pred=predict(fit,newdata=list(age=age.grid))
res<-(fit$yin-fit$y)/(1-fit$lev)      
sigma<-sqrt(var(res))                 
upper<-fit$y+2.0*sigma*sqrt(fit$lev)
lower<-fit$y-2.0*sigma*sqrt(fit$lev)
lines(pred$x,pred$y,lwd =2,col=4)
matlines(fit$x,cbind(upper,lower),lwd=1,col=4,lty="dashed")

fit=smooth.spline(age,wage,df=16)
pred=predict(fit,newdata=list(age=age.grid))
res<-(fit$yin-fit$y)/(1-fit$lev)      
sigma<-sqrt(var(res))                 
upper<-fit$y+2.0*sigma*sqrt(fit$lev)
lower<-fit$y-2.0*sigma*sqrt(fit$lev)
lines(pred$x,pred$y,lwd =2,col=5)
matlines(fit$x,cbind(upper,lower),lwd=1,col=5,lty="dashed")
legend("topright",
       legend=c("natural cubic spline","cubic spline","smoothing spline CV","smooth spline degree 16"),
       col=c(2:5),
       lty=1,
       lwd=2,
       cex=0.8)




library(ISLR)
attach(Wage)
agelims<-range(age)
age.grid<-seq(from=agelims[1],to=agelims[2])
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=0.2,data=Wage)
fit2=loess(wage~age,span=0.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),
        col=2,lwd =2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),
        col=4,lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5") ,
          col=c("red","blue"),lty=1,lwd=2,cex=0.8)




agelims<-range(age)
age.grid<-seq(from=agelims[1],to=agelims[2])
fit=glm(I(wage>250)~ns(age,df=4),data=Wage,family=binomial)
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=1/(1+exp(-se.bands.logit))
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,0.2))
points(jitter(age),I((wage>250)/5),cex=0.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd =2,col="red")
matlines(age.grid,se.bands,lwd=1,col="red",lty=3)


quantile(age,probs=c(0.25,0.5,0.75))
fit=glm(I(wage>250)~bs(age,knots=c(33.75,42.00,51.00)),data=Wage,family=binomial)
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=1/(1+exp(-se.bands.logit))
lines(age.grid,pfit,lwd =2,col="green")
matlines(age.grid,se.bands,lwd=1,col="green",lty=4) 










library(tree)
library(ISLR)
data(Hitters)
Hitters <- na.omit(Hitters)
sam<-sample(263,132)
train<-Hitters[sam,]
test<-Hitters[-sam,]
tree.hits<-tree(Salary~.,data=train)
plot(tree.hits)
text(tree.hits,pretty=0)







library(ISLR)
library(tree)
data(Hitters)
Hitters <- na.omit(Hitters)
Hitters$Salary=log(Hitters$Salary)
sub<-c(1:7,16,19)
sam<-sample(263,132)
train<-Hitters[sam,sub]
test<-Hitters[-sam,sub]

tree.hits<-tree(Salary~.,data=train)
plot(tree.hits)
text(tree.hits,pretty=0)

cv.hitters<-cv.tree(tree.hits)
cv.hitters
train.mse<-NULL
test.mse<-NULL
cv.mse<-NULL
train.mse[1]<-var(train$Salary)
test.mse[1]<-mean((mean(train$Salary)-test$Salary)^2)
tree.size<-c(1:10)
library(caret)
folds=createFolds(y=train$Salary,6)
error<-0
for(j in 1:6)
{
  train_cv<-train[-folds[[j]],]
  test_cv<-train[folds[[j]],]
  error<-error+mean((mean(train_cv$Salary)-test_cv$Salary)^2)
}
cv.mse[1]<-error/6
for(i in 2:10)
{
  error<-0
  for(j in 1:6)
  {
    train_cv<-train[-folds[[j]],]
    test_cv<-train[folds[[j]],]
    cvtree<-tree(Salary~.,data=train_cv)
    prunecvtree=prune.tree(cvtree,best=i)
    error<-error+mean((predict(prunecvtree,newdata=test_cv)-test_cv$Salary)^2)
  }
  cv.mse[i]<-error/6
  tree.hits<-tree(Salary~.,data=train)
  prune.hits=prune.tree(tree.hits,best=i)
  train.mse[i]<-mean((predict(prune.hits,train)-train$Salary)^2)
  test.mse[i]<-mean((predict(prune.hits,test)-test$Salary)^2)
}


plot_error <- function(x, y, sd, len = 1, col = "black") {
       len <- len * 0.05 
       arrows(x0 = x, y0 = y, x1 = x, y1 = y - sd, col = col, angle = 90, length = len)
       arrows(x0 = x, y0 = y, x1 = x, y1 = y + sd, col = col, angle = 90, length = len)
  }

plot(tree.size,cv.mse,type="b",ylim=c(0,1),col="green",pch=19)
lines(tree.size,train.mse,type="b",col="black",pch=19)
lines(tree.size,test.mse,type="b",col="brown",pch=19)
plot_error(tree.size,cv.mse,sd=sd(cv.mse),col="green")
plot_error(tree.size,train.mse,sd=sd(train.mse),col="black")
plot_error(tree.size,test.mse,sd=sd(test.mse),col="brown")




prune.hits=prune.tree(tree.hits,best=3)
plot(prune.hits)
text(prune.hits,pretty=0)

tree.pred=predict(prune.hits,test)
plot(tree.pred,test$Salary,col=3)
abline(0,1,col=2)















