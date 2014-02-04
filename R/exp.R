experiment<-function(model,N=NULL,rep=1,fun.oneblock=.return.data,fun.aggregate=NULL) {
  structure(list(model=model,
                 N=N,
                 rep=rep,
                 fun.oneblock=fun.oneblock,
                 fun.aggregate=fun.aggregate,
                 coefs=NULL,
                 coefs.dim=NULL)
            ,class="simCity.experiment")  
}


"coefs<-"<-function(exper,coefsobj,mode="by.equation",...) {

  by.term<-function() {
    if (class(arg$value)=='matrix') arg$value<-lapply(1:dim(arg$value)[2], function(e) arg$value[,e]) 
    dims<-lapply(arg$value,function(e) dim(as.matrix(e)))
    dims<-do.call('rbind',dims)
    maxl<-max(dims[,1])
    ow <- options("warn")
    options(warn=-1)
    am<-lapply(arg$value,function(e) matrix(as.matrix(e),ncol=dim(as.matrix(e))[2],nrow=maxl))
    options(ow)
    do.call('cbind',am)
    
      
  }
  arg<-list(...)    
  stopifnot(mode %in% c("by.equation","by.term.match","by.term.factorial"))
  if (mode=="by.equation") {
     if (class(arg$value)=='matrix') arg$value<-lapply(1:dim(arg$value)[1], function(e) arg$value[e,]) 
      exper$coefs<-lapply(arg$value,unlist)
  }
  if (mode=="by.term.match") {
    am<-by.term()
    exper$coefs<-lapply(1:dim(am)[1], function(i) am[i,])
  }
  if (mode=="by.term.factorial") {
    if (class(arg$value)=='matrix') arg$value<-lapply(1:dim(arg$value)[2], function(e) arg$value[,e]) 
    am<-expand.grid(arg$value)
    exper$coefs<-lapply(1:dim(am)[1], function(i) as.numeric(unlist(am[i,])))
  }
  if (!.check.coeflist.dim(exper$coefs,dim(exper$model)))
      stop('Coefficients dimensions are wrong, each run should have ',paste(dim(exper$model),collapse='+'),'=',sum(dim(exper$model)),' coefficients')
  
  exper
}

.check.coeflist.dim<-function(coefs,dims) {
  all(unlist(lapply(coefs,function(e) all(length(e)==sum(dims)))))
  
}
