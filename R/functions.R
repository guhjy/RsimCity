simc.options<-function(...) {
  arg<-list(...)
  if (length(arg)==0) .getEnv()
  else  if (is.null(names(arg))) .getEnv(arg)
  else .setEnv(names(arg),arg)
  
}


.<-function(x) x


addmissing<-function(v,p) {
                  v[rbinom(length(v),1,as.numeric(p))==1]=NA
                  v
}





etheroapply<-function(alist,manyfunctions) {
  .somefunc<-function(a,b) {
    b(a)  
  }
  mapply(alist,manyfunctions,FUN=.somefunc)
}

.split.in.list<-function(thelist,ns) {
first<-1
last<-0

res<-lapply(ns,function(e) {
  last<<-last+e
  r<-thelist[first:last]    
  first<<-first+e
  r
})
remove(last)
remove(first)
res
}

.catn<-function(w)   {
  cat(as.character(w),sep="\n")
  cat('\n')
}
.cat<-function(w) {
  cat(as.character(w))
  cat('\n')
}

print.simCity.experiment<-function(obj) {
  .cat("#### model #######")
  .cat("Exogenous variables")
  .cat(obj$model$exogenous)
  .cat("Endogenous variables")
  .cat(obj$model$endogenous)
  .cat('')
  .cat('### Experiment ####')
  .cat("Equations")  
  .catn(equations(obj$model))
   .cat(paste('rep=',paste(obj$rep),collapse=','))
   .cat(paste('N=',paste(obj$N),collapse=','))
   if (!is.null(obj$fun.oneblock))    .cat(paste('block processing function defined  (use funct(exper) to check it out)'))
   if (!is.null(obj$fun.aggregate))   .cat(paste('aggregate function defined  (use funct(exper) to check it out)'))
   if (!is.null(obj$coefs)) {
     head(coefs(obj))
     .cat('use coefs(experiment) for all combinations')
   }  else .cat('Coefficients=NULL')                                                                               
}

dim.simCity.model<-function(obj) {
  obj$dim
}


.equation.terms.names<-function(obj) {
  n<-names(dim(obj$model))
  r<-lapply(dim(obj$model),function(e) 1:e)
#  r<-paste(n,unlist(r))
  print(r)
}

.nospace<-function(string) gsub(" ","", as.character(string) , fixed=TRUE)

coefs.unique<-function(obj) {
  coefs<-do.call('rbind',lapply(obj$coefs,function(e) unlist(e)))
  cnames<-lapply(1:length(dim(obj$model)), function(e) paste(names(dim(obj$model)[e]),1:dim(obj$model)[e],sep=''))
  cnames<-do.call('c',cnames)
  colnames(coefs)<-cnames  
  cv<-apply(coefs,2,unique)
  if (class(cv)=='matrix') cv<-lapply(1:dim(cv)[2],function(e) unlist(cv[,e]))
  cv
}

print.simCity.coefs<-function(obj) {
  coefs<-do.call('rbind',lapply(obj$coefs,function(e) unlist(e)))
  cnames<-lapply(1:length(dim(obj$model)), function(e) paste(names(dim(obj$model)[e]),1:dim(obj$model)[e],sep=''))
  cnames<-do.call('c',cnames)
  colnames(coefs)<-cnames  
  .cat('Coefficients values')
  cv<-apply(coefs,2,unique)
  if (class(cv)=='matrix') cv<-lapply(1:dim(cv)[2],function(e) unlist(cv[,e]))
  lapply(1:length(cv),function(e) .cat(paste(cnames[e],':',cv[e],collapse=',')))
  .cat('Number of combinations')
  .cat(dim(coefs)[1])
  .cat('Coefficients combinations')
  print(coefs)
  cat('')
}

head.simCity.coefs<-function(obj,n=6) {
  coefs<-do.call('rbind',lapply(obj$coefs,function(e) unlist(e)))
  cnames<-lapply(1:length(dim(obj$model)), function(e) paste(names(dim(obj$model)[e]),1:dim(obj$model)[e],sep=''))
  cnames<-do.call('c',cnames)
  colnames(coefs)<-cnames    
  .cat('Coefficients values')
  cv<-apply(coefs,2,unique)
  if (class(cv)=='matrix') cv<-lapply(1:dim(cv)[2],function(e) unlist(cv[,e]))
  lapply(1:length(cv),function(e) .cat(paste(cnames[e],':',cv[e],collapse=',')))
  .cat('Number of combinations')
  .cat(dim(coefs)[1])
  .cat('First coefficients combinations')
  print(coefs[1:n,])
  cat('')
}



.return.data<-function(dataframe,arguments) {
  onames<-names(dataframe)
  cc<-paste(arguments$coefs,collapse=',')
  newdata<-cbind(dataframe,arguments$N,arguments$rep,cc)
  names(newdata)<-c(onames,'N','rep','coefs')
  newdata
}