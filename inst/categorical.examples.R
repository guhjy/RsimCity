library(simCity)

########### use of categorical.ortho ###########

s<-list(a=c(0:1),b=c(0:2),c=c(0:1)) # a 2 X 3 X 2 design
mm<-simc.model()
y~1+ a1+b1+b2+c1+a1b1+a1b2+a1c1+b1c1+b2c1+a1b1c1+a1b2c1
#
exdist(mm)<-categorical.ortho
excov(mm)<-NULL
exmeans(mm)<-s
d<-make.data(mm,96,rep(1,12))
head(d)
table(a=d$a,b=d$b,c=d$c)

########### use of categorical: e 2X2 design fix cell sizes, unbalanced design ###########

mm<-simc.model()
y~1+ a1+b1+a1b1
#
dat<-matrix(c(a=c(1,0,1,1,0),b=c(0,0,1,1,1)),ncol=2)
colnames(dat)<-c('a','b')
dat
mm
exdist(mm)<-categorical
exmeans(mm)<-dat
d<-make.data(mm,100,rep(1,4))
head(d)
table(d$a,d$b)

####### balanced 3x2 design #########à
mm<-simc.model()
y~1+ a2+b2+b3+a2b2+a2b3
#

dat<-matrix(c(rep(c(1,2),each=3),rep(c(1,2,3),2)),ncol=2)
colnames(dat)<-c('a','b')
dat
categorical.formula(dat)
exdist(mm)<-categorical
exmeans(mm)<-dat
d<-make.data(mm,120,rep(1,6))
head(d)
table(d$a,d$b)
            


########### use of categorical: e 2X2 random cell size design ###########

rcategorical<-function(n,info,cov) {
dat<-data.frame(a=rbinom(n,1,.5),b=rbinom(n,1,.5))
categorical(n,dat)
}
mm<-simc.model()
y~1+ a1+b1+a1b1
#
exdist(mm)<-rcategorical
d<-make.data(mm,100,rep(1,4))
head(d)
table(d$a,d$b)

########### use of categorical: e 2X2 random design, experiment over unbalance of the groups ###########
### run a lm for each sample, save the coefficients and repeat for different set of structural coefficients 
rcategorical<-function(n,info,cov) {
  dat<-data.frame(a=rbinom(n,1,info[1]),b=rbinom(n,1,info[2]))
  categorical(n,dat)
}
oneblockfun<-function(simdata,arguments) {
mlm<-lm(y~a1+b1+a1b1,data=simdata)## do something usefull on this
dat<-matrix(c(arguments$model$exmeans,t(coef(mlm))),nrow=1)
colnames(dat)<-c('p1','p2',names(coef(mlm)))
data.frame(dat)
}
mm<-simc.model()
y~1+ a1+b1+a1b1
#
exdist(mm)<-rcategorical

exper<-experiment(mm,N=c(20,30,40),rep=1,fun.oneblock=oneblockfun)
coefs(exper)<-list(c(2,1,2,3),c(2,1.1,2.1,3.1),c(2,1.2,2.2,3.3))
proportions<-list(c(.5,.2),c(.5,.3),c(.5,.4),c(.5,.5))
res<-lapply(proportions, function(e) {
  exmeans(mm)<-e  
  exper<-update(exper,mm)
  run(exper)
})
res
do.call('rbind',res)

########### model with categorical and continuous variables ##########
myexdist<-function(n,info,cov) {
  catdata<-categorical(n,info,list(a='contr.treatment',b='contr.treatment'))
  condata<-rmnorm(n,varcov=as.matrix(cov))
  cbind(catdata,condata)                
}
myexdist(10,e,myexcov)

myexcov<-data.frame(x=c(1,0),z=c(0,1))
d<-categorical(100,myexcov)
myexcov<-data.frame(x=c(1,0),z=c(0,1))
e<-expand.grid(myexcov)
d<-categorical(110,e)
d
table(d$x,d$z)
rownames(myexcov)<-colnames(myexcov)
e<-expand.grid(a=c(1,2),b=c(1,2,3))
mm<-simc.model(exdist=myexdist,exmeans=e,excov=myexcov)
y~1+a2+b2+b3+a2b2+a2b3+x+z
#
d<-make.data(mm,250,rep(1,8))
head(d)
summary(lm(y~a2+b2+b3+a2b2+a2b3+x+z,data=d))
options('contrasts')
summary(lm(y~factor(a)*factor(b)+x+z,data=d))
cor(d$z,d$x)

## Same model, but now we generate the continous variables as correlated 
myexcov<-data.frame(x=c(1,.4),z=c(.4,1))
excov(mm)<-myexcov
d<-make.data(mm,250,rep(1,8))
head(d)
summary(lm(y~a2+b2+b3+a2b2+a2b3+x+z,data=d))
options('contrasts')
summary(lm(y~factor(a)*factor(b)+x+z,data=d))
cor(d$z,d$x)



########### Unbalanced 2X2 design, exact counts per cell  ##########

s<-list(a=c(0:1),b=c(0:2)) # a 2 X 3 to be reduced to 2 X 2
e<-expand.grid(s)
e[e$b==2,2]<-1
## check the design out

table(e$a,e$b)
d<-categorical(250,e)
head(d)
table(d$a,d$b)

## set up and run the actual model

mm<-simc.model(exdist=categorical,exmeans=e)
y~1+a1+b1+a1b1
#
d<-make.data(mm,250,rep(1,8))
head(d)
summary(lm(y~a1+b1+a1b1,data=d))
summary(lm(y~factor(a)*factor(b),data=d))

########### run a simulation on t-test with etherogenous variances  ##########
library(car)

s<-list(a=c(0:1)) # one way design
ethero<-function(n,x) {
   r<-lapply(x, function(e) ifelse(e==0,rnorm(1,0,2),rnorm(1,0,1)))
   unlist(r)
}
mm<-simc.model()
y~a1;edist=ethero(a1)
#
## check everything is ok
equations(mm,c(2))
exdist(mm)<-categorical.ortho
exmeans(mm)<-s
d<-make.data(mm,100,c(2))
leveneTest(d$y,d$a,center="mean")
var(d$y[d$a==0])
var(d$y[d$a!=0])


## set up and run the experiment

oneblockfun<-function(simdata,arguments) {  
  data.frame(coef=arguments$coefs,
             p1=t.test(simdata$y~simdata$a,var.equal=T)$p.value,
             p2=t.test(simdata$y~simdata$a,var.equal=F)$p.value)  
}

myexp<-experiment(mm,rep=10,N=c(20,40,60,80),fun.oneblock=oneblockfun)
coefs(myexp)<-list(0,.5,1)
run(myexp)


