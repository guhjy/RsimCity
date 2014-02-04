
exogenous<-function(model) {
  stopifnot(class(model)=='simCity.model')
  unique(model$exogenous)
}
endogenous<-function(model) {
  stopifnot(class(model)=='simCity.model')
  unique(model$endogenous)
}

equations<-function(model,coefs=NULL) {
  stopifnot(class(model)=='simCity.model')
  if (is.null(coefs)) lapply(model$equations, function(e) e[[1]])
  else mapply(model$equations,.split.in.list(unlist(coefs),dim(model)),"N", FUN=.sp, SIMPLIFY=FALSE)
  
}


excov<-function(model) model$excov 


"excov<-"<-function(model,...) {
  arg<-list(...)
  model$excov<-arg$value
  model
}

exdist<-function(model) model$exdist 

"exdist<-"<-function(model,...) {
  arg<-list(...)
  model$exdist<-arg$value
  model
}

exmeans<-function(model) model$exmeans 

"exmeans<-"<-function(model,...) {
  arg<-list(...)
  model$exmeans<-arg$value
  model
}

.getendogenous<-function(eq) {
  a<-all.vars(as.formula(eq$equation))
  a[1]
}

.getModelDimension<-function(eqs) {
  unlist(lapply(eqs,function(e) {
    e<-gsub("|",":",e,fixed=T)
    l<-length(attr(terms(as.formula(e[[1]])),'term.labels'))
    i<-length(grep("\\~1\\+",.nospace(e[[1]])))
    l<-l+i
    ifelse(l==0,1,l)
    
  }))
  
}

.renumerate <-function(d) names(d)<-letters[1:length(d)]


.make.excov<-function(exogenous,excov) {
    excov<-matrix(0,nrow=length(exogenous),ncol=length(exogenous))
    diag(excov)<-rep(1,length(exogenous))    
    rownames(excov)<-colnames(excov)<-exogenous
    excov
}

.make.exmeans<-function(exogenous,exmeans) {
  if (length(exogenous)>0 && is.null(exmeans)) {
    exmeans<-rep(0,length(exogenous))
  }    
  exmeans
}

.make.exdist<-function(model) {
  if (is.null(model$exdist)) {
    model$exdist<-rmnorm
    model$exmeans<-rep(0,length(model$exogenous))    
    model$excov<-.make.excov(model$exogenous,NULL)    
  }
  
  model
}

.splitmyline<-function(l) {
  s<-strsplit(l,';',fixed=T)  
}

.getesogenous<-function(eq) {
  all.vars(as.formula(eq$equation))
}



.getoptions<-function(lines) {
  #s<-lines[-1]
  s<-lapply(lines,function(ss){
    model=list()
    model$equation<-ss[[1]][1]
    for (i in ss[[1]]) {
      o<-strsplit(i,'=',fixed=T)
      tar<-.nospace(o[[1]][1])
      if(tar=='edist') model$edist<-o[[1]][[2]]
      if(tar=='options') model$options<-unlist(strsplit(o[[1]][[2]],","))
      if(tar=='apply') {
      opt<-.nospace(o[[1]][[2]])
      opt<-unlist(str_split(gsub('),','#@',opt),'@'))
      opt<-gsub('#',')',opt)
      model$apply=opt      
      }
    }
    model
  })
  
  s
}

