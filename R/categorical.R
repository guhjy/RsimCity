categorical.ortho<-function(n,centers=NULL,values=NULL,...) {
  d<-expand.grid(centers)
  colnames(d)<-names(centers)
  d<-categorical(n,d,values)
  d
}

categorical.formula<-function(centers=NULL,values=NULL) {
  if(class(centers)=='list')  d<-expand.grid(centers)
  else d<-centers
  d<-as.data.frame(apply(d,2,factor))
  f<-paste('~',paste(colnames(centers),collapse='*'),collapse='')  
  dd<-with(d ,{ as.data.frame(model.matrix(as.formula(f),contrasts = values))  })
  names(dd)<-gsub(":","",names(dd),fixed=T)
  paste('1+',paste(names(dd)[-1],collapse='+'),collapse='')
}

categorical<-function(n,levels,contrast=NULL) {
  
  d<-levels
  options(warn=-1)
  d <- as.data.frame(d[matrix(1:dim(d)[1],nrow=n,ncol=1),])
  options(warn=0)
  d<-as.data.frame(apply(d,2,factor))
  if (dim(d)[1]<n) warning('Sample size is too small for the requested  design')
  colnames(d)<-colnames(levels)
  rownames(d)<-1:length(d[,1])  
  if (length(colnames(levels))<1) stop("exmeans should have non-empty colnames")
  f<-paste('~',paste(colnames(levels),collapse='*'),collapse='')
  dd<-with(d ,{ as.data.frame(model.matrix(as.formula(f),contrasts = contrast))  })
  names(dd)<-gsub(":","",names(dd),fixed=T)
  cbind(dd,d)[-1]  
}