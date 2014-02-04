make.data<-function(model,N,coefs=1) {
  dat<-data.frame(n_=1:N)    
   if (length(model$exogenous)>0) {
     model<-.make.exdist(model)
     dat<-cbind(model$exdist(N,model$exmeans,model$excov),dat)
   }
    
     icoefs<-.split.in.list(unlist(coefs),dim(model))
  
  lines<-mapply(model$equations,icoefs,N, FUN=.sp, SIMPLIFY=FALSE)
  if (simc.options('verbose'))  if (length(model$endogenous)!=length(unique(model$endogenous))) warning('A variable appears more than once as an outcome ',paste(model$endogenous,collapse=",")," Only the last one will be considered")
  j<-0  
  for (l in lines) {
    j<-j+1
    oldnames<-names(dat)
    dat<-cbind(as.data.frame(eval(parse(text=l),as.data.frame(dat))),dat)  
    names(dat)<-c(model$endogenous[j],oldnames)
    for (opt in model$equations[[j]]$apply) {
        func<-gsub('(',paste('(',model$endogenous[j],',',sep='',collapse=''),opt,fixed=T)
        dat[,model$endogenous[j]]<-eval(parse(text=func),as.data.frame(dat))
      }
    }

  dat[unique(names(dat))]    
}
.formatDist<-function(eq,n) {
  if (('trans' %in% eq$options)) eq$edist<-0
  if (('beta' %in% eq$options)) eq$edist<-c("rnorm(0,1)")  
  if (is.null(eq$edist)) eq$edist<-c("rnorm(0,1)")
  if (!is.numeric(eq$edist)) {
    eq$edist<-gsub('(',paste('(',n,',',sep=''),eq$edist,fixed=T)
  }
  eq$edist
}

.sp<-function(l,x,n) {
  
  eq<-str_split(str_split(.nospace(l$equation),'~')[[1]][2],"\\+")[[1]]
  
  if (('beta' %in% l$options)) {
                  if (any(abs(x)>=1)) warning('With option "beta" a coefficent -1<b<1 is required. Results may be unreliable')
                  x<-x/sqrt(1-x^2)
                  eq<-sapply(eq,function(e) paste('scale(',e,')',sep=''))
                  }
  ivs<-paste(eq,x,sep="*",collapse="+")
  ivs<-paste(ivs,.formatDist(l,n),sep="+")  
  paste(all.vars(as.formula(l$equation))[1],ivs,sep="<-")
}

