\name{exinfo}
\title{Set N's and repetitions for a simCity simulation}
\alias{exper.n}
\alias{exper.rep}

\concept{ simCity model }
\description{
  Set N's and repetition for a simCity simulation for a  \code{\link[simCity]{simc.model}}  
}
\usage{
exper.n(model)<-nvalue
exper.rep(model)<-rvalue
}
\arguments{
  \item{model}{ a \code{\link[simCity]{simc.model}} }
  \item{nvalue}{ the vector of N sample sizes to use in the simulation  }
  \item{rvalue}{ the number of repetition of one model (same coefficients) to use in defining a  simulation block  }

}

\details{

}
\value{
  Attributes $N and $rep are updated in \code{\link[simCity]{simc.model}}  
}
\seealso{
  \code{\link[simCity]{model}},  \code{\link[simCity]{run.block}}
}
\examples{

### generate 100 cases for a model with two correlated exogenous variables with different variances and  an endogenous variable with binomial residuals.
s.model<-simc.model()
y~x+z,rbinom 100 .5
#
covs<-matrix(c(2,.3,.3,4),ncol=2)
colnames(covs)<-rownames(covs)<-c("x","z")
covs
excov(s.model)<-covs
make.data(s.model,10,list(1,1))

#### custom  function to produce e 2x2 categorical exogenous variables ##########

mydist<-function(N,means=NULL,excov=NULL) {
  m<-matrix(rbinom(N*dim(excov)[1],1,.5),ncol=dim(excov)[1])
  a<-as.data.frame(m)
  names(a)<-colnames(excov)
  a
}
mat<-matrix(0,ncol=2,nrow=2)
exdist(s.model)<-mydist
excov(s.model)<-mat
s.model
make.data(s.model,10,list(1,1))
e<-experiment(s.model)
exper.n(e)<c(10,20,40)
exper.rep(e)<50
print(e)



}
\keyword{models}
