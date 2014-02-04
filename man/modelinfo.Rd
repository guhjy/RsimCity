\name{modelinfo}
\title{set exogenous variable properties in a simCity model}
\alias{excov}
\alias{exdist}

\concept{ simCity model }
\description{
  Set exogenous variables distribution function and covariance matix for a  \code{\link[simCity]{simc.model}}  
}
\usage{
exdist(model)<- valuefunc
exdist(model)
excov(model)<- valuemat
excov(model)
}
\arguments{
  \item{model}{ a \code{\link[simCity]{simc.model}} }
  \item{valuefunc}{ a function for exogenous variables multivariate distribution }
  \item{valuemat}{ a matrix with variance and covariances (or equivalent matrix) for the exogenous variables distribution }

}

\details{
\code{exdist(model)} sets the  multivariate distribution of the exogenous variables.If NULL \code{\link{rmnorm}}  is used. It can be any function which accept N as first argument, mean and excov as second and third argument. \cr\cr

\code{excov(model)} sets the  covariance matrix to be passed to multivariate distribution. If NULL an identity matrix is used.

}
\value{
  Attributes $exdist and $excov are updated in \code{\link[simCity]{simc.model}}  
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
