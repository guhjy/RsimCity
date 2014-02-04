library(simCity)

covv<-matrix(c(1,.3,.3,1),nrow=2)
rownames(covv)<-c("x","q")
colnames(covv)<-c("x","q")
covv

myfunc<-function(x) {
  10*x
}
mycut<-function(x) cut(x,5,labels=F)

model<-simc.model()
y ~ 1 ; edist=rbinom 10 .5
#
model
make.data(model,10,c(2))
myfunc<-function(x) x*x
model<-simc.model()
y ~ 1 ; edist=rbinom(10,..)
m ~ y +x ; options=beta 
z ~ y ; edist=rnorm(0,1);apply=round(2),addmissing(.1)
z~ I(myfunc(z)); apply=addmissing(.03);options=trans 
w~ .(y*y)
#
model
exogenous(model)
endogenous(model)
equations(model)
excov(model)
coef<-c(.1,.1,.1,.1,.1,.1)
q<-make.data(model,100,coef)
q
expcoefs<-list(list(c(.5,.2),c(.5),1),
               list(c(.6,.2),c(.5),1),
               list(c(.7,.2),c(.5),c(1,1))            
)
expcoefs

model<-simc.model(excov=covv)
y ~ x+q , edist=rbinom 10 .5
#
model
model<-simc.model(excov=covv)
y ~ x+q ; edist=rbinom(10, .5)
#
model
make.data(model,10,c(1,2))

rmnorm(10,as.numeric(model$exmeans),model$excov)