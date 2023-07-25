library(ISLR)
library(leaps)
data(Credit)

Credit<-Credit[,-1]
dim(Credit)
regfit.full=regsubsets(Balance~.,data=Credit,nvmax=11)
reg.summary=summary(regfit.full)
reg.summary
### dummy variable

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="b")
loc<-which.min(reg.summary$cp)
points(loc,reg.summary$cp[loc],col="red",cex=2,pch=4)
coef(regfit.full,loc)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="b")
loc<-which.min(reg.summary$bic)
points(loc,reg.summary$bic[loc],col="red",cex=2,pch=4)
coef(regfit.full,loc)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted R2",type="b")
loc<-which.max(reg.summary$adjr2)
points(loc,reg.summary$adjr2[loc],col="red",cex=2,pch=4)
coef(regfit.full,loc)



library(caret)
mse<-rep(0,11)
folds=createFolds(y=Credit$Balance,10)
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars=names(coefi)
  mat[, names(coefi)] %*% coefi
}

for (i in 1:10)
{
  train_cv<-Credit[-folds[[i]],]
  test_cv<-Credit[folds[[i]],]
  regfit.full=regsubsets(Balance~.,data=Credit[-folds[[i]],],nvmax=11)
  for(j in 1:11)
  {
    pre<-predict(regfit.full,test_cv,id=j)
    mse[j]<-mse[j]+mean((pre-test_cv$Balance)^2)
  }
}

mse<-mse/10
plot(mse,xlab="Number of Variables",ylab="MSE",type="b")
loc<-which.min(mse)
points(loc,mse[loc],col="red",cex=2,pch=3)
