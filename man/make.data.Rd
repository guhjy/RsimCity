\name{make.data}
\title{Generate a data.frame based on a simCity model}
\alias{make.data}
\alias{simCity.make.data}

\concept{ simCity model }
\description{
  Based on a  \code{\link[simCity]{simc.model}} and a set of coefficients generate a data.frame with the variables defined.  
}
\usage{
make.data(model,N,coefs=1) 

}
\arguments{
  \item{model}{ a \code{\link[simCity]{simc.model}} }
  \item{N}{ number of cases to generate }
  \item{coefs}{coefficients to be applied to the structural equations. See Details}

}

\details{

Data are genarated applying the coefficients in \code{coefs} to the structural equations in \code{model}. Coefficients should be passed as a list of numeric vectors, with one list for each equation in the \code{model}. Each vector must contain one coefficient for each term in the equation. Coefficients are multiplied by the corresponding term in sequence. For instance, for an equation like \code{y~x+z}, coefficients should be \code{c(2,3)}, and the resulting equation will be \code{y~2*x+3*z}.  If \code{model} contains a constant term, a coefficient for it must be provide (use \code{1} for not changing it). If the vector coefficients is shorter than the vector of terms in the equation, coefficients are reused. If \code{NULL}, all coefficients are set to \code{1}.
}
\value{
  a \code{\link{data.frame}} containing all the variables defined in \code{model}.
}
\seealso{
  \code{\link[simCity]{model}},  \code{\link[simCity]{run.block}}
}
\examples{
### generate 100 cases for a simple model with two variables  normally distributed, x standardized.
s.model<-simc.model()
x~1,rnorm 0 1
y~x
#
## use 2 for all equations and terms
s.coefs<-2
equations(s.model,s.coefs)
s.data<-make.data(s.model,100,s.coefs)
head(s.data)

### generate 100 cases for a simple model with two variables  normally distributed, both standardized.
s.model<-simc.model()
x~1,rnorm 0 1
y~1+x,apply=scale()
#
s.coefs<-list(10,4,.5)
s.data<-make.data(s.model,100,s.coefs)
head(s.data)

### generate 100 cases for a model with two uncorrelated and standardized exogenous variables  a endogenous variable with binomial residuals.
s.model<-simc.model()
y~x+z,rbinom 100 .5
#
print(s.model)
s.coefs<-20
s.data<-make.data(s.model,100,s.coefs)
head(s.data)

### generate 100 cases for a model with two correlated exogenous variables with different variances and  a endogenous variable with binomial residuals.
s.model<-simc.model()
y~x+z,rbinom 100 .5
#
covs<-matrix(c(2,.3,.4,3),ncol=2)
colnames(covs)<-rownames(covs)<-c("x","z")
covs
s.coefs<-list(2,.5)
s.data<-make.data(s.model,100,s.coefs,excov=covs)
head(s.data)


### generate 100 cases for a mediational model with non-standardized residual and intercepts
s.model<-simc.model()
m~1+x, rnorm 5 10
y~1+m+x, rnorm 2 10
#
s.coefs<-list(3,.4,10,2,.5)
equations(s.model,s.coefs)
s.data<-make.data(s.model,100,s.coefs)
head(s.data)

### generate 100 cases for  a structural factorial model with two correlated latent factors and 3 observed variables for each factor with etherogenous distribution of residuals.

covs<-matrix(c(1,.2,.2,.3),ncol=2)
colnames(covs)<-rownames(covs)<-c("csi","eta")
covs
s.model<-simc.model(excov=covs)
x1~csi, rnorm 0 1
x2~csi, binom 10 .5
x2~csi, uniform 0 1
x3~eta, rnorm 0 1
x4~eta, binom 10 .5
x5~eta, uniform 0 1  
#
s.coefs<-list(.8,.7,.4,-.5,.9,.3)
s.data<-make.data(s.model,100,s.coefs,excov=covs)
head(s.data)
}
\keyword{models}
