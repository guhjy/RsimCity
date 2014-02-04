
simc.model<-function(file="",exdist=NULL,exmeans=NULL,excov=NULL) {
  model <- scan(file=file, what="", sep="\n",
                strip.white=TRUE, comment.char="#", fill=TRUE) 
 
  lines<-lapply(model, .splitmyline)
  names(lines)<-letters[1:length(lines)]
  equations<-.getoptions(lines)
  endo<-lapply(equations, .getendogenous)
  
  endo<-unlist(endo)
  
  exo<-sapply(equations, .getesogenous)
  exo<-unique(unlist(exo))
  
  w<-(exo %in% endo)==FALSE
  exo<-exo[w]
  exo<-exo[exo!=""]

  mdim<-.getModelDimension(equations)
  
  if (!is.null(excov)) {
    if(any(colnames(excov) %in% endo)) stop('Model malformed: an endogenous variable cannot be in the exougenous covariance matrix')
  } 

  structure(list(endogenous=endo,
                 exogenous=exo,
                 equations=equations,
                 exdist=exdist,
                 exmeans=exmeans,
                 excov=excov,
                 dim=mdim),class='simCity.model')
}


