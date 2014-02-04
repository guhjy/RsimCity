
sim.variable <- function (distr=c('type'='norm','mean'=0,'std'=1),scale=NULL) 
{
p<-paste('#n#',distr[2],distr[3],sep=',')  
string<-paste("r",distr['type'],"(",p,')',sep="")
l<-c('type'=distr,'string'=string,scale=scale)
structure(list(type=distr,string=string,scale=scale), class="sim.variable")   
}

sim.sample <- function (n,obj,scale=TRUE) 
{
#  if (length(obj) == 0) stop('sim.sample: no obj')
  if(class(obj)!='sim.variable') stop(paste('sim.sample: obj must be a simVariable object (not a',class(obj),')'))
  s<-gsub('#n#',n, obj['string'])
  v<-eval(parse(text = s) )
  if (scale == TRUE ) {v<-scalerange(obj,v)}
  v
  
}



