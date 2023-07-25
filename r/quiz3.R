library(ISLR)
data(Default)
View(Default)
str(Default)
attach(Default)
table(default)
sum(default=="Yes")/length(default)
table(default,student)
par(mfrow=c(1,3))
boxplot(balance~default,col=c(2,6),data=Default)
boxplot(income~default,col=c(2,6),data=Default)
col2<-as.numeric(default)+1
par(mfrow=c(1,1))
plot(balance,income,pch=3,col=col2)


summary(glm(formula= default~student,data=Default,family = binomial))
summary(glm(formula= default~balance,data=Default,family = binomial))
summary(glm(formula= default~income,data=Default,family = binomial))


glm1<-glm(formula= default~balance+income+student,data=Default,family = binomial)
summary(glm1)

predict(glm1,newdata=data.frame(balance=2500,income=50000,student="Yes"),type="response")
detach()

n<-dim(Default)[1]
sam<-sample(n,0.7*n)
T1<-Default[sam,]
V1<-Default[-sam,]
glm2<-glm(default~balance+income+student,data=T1,family=binomial)
prob<-predict(glm2,type="response")
pred<-rep("No",dim(T1)[1])
pred[prob>0.5]="Yes"
table(pred,T1$default)

vprob<-predict(glm2,newdata=V1,type="response")
vpred<-rep("No",dim(V1)[1])
vpred[vprob>0.5]="Yes"
table(vpred,V1$default)
sum(vpred=="Yes"&V1$default=="Yes")/sum(V1$default=="Yes")
sum(vpred=="No"&V1$default=="No")/sum(V1$default=="No")

