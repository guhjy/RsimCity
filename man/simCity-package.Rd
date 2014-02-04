\name{simCity-package}
\alias{simCity-package}
\alias{simCity}
\docType{package}
\title{
Simulation suite for linear models
}
\description{
\code{simCity} provides a suite to build and run simulations on linear and additive models. It provides a flexible way to define the data generating model, and useful commands to set up a simulation experiment. Models can be specified with an intuitive syntax inspired by \code{\link{sem}} package. It basically allows building datasets accodingly to a set of linear models with different distributions of residuals, and provides mechanisms to apply custom functions to process the generated data.   
See examples in  \code{\link[simCity]{simc.model}} and  \code{\link[simCity]{experiment}} and a very simple example of simulation below.
}
\details{

The building blocks of one simulation experiment are: a command to declare the data generating model (\code{\link[simCity]{simc.model}}), a command to set up the experiment (\code{\link[simCity]{experiment}}) to be build on this model, and a command (\code{\link[simCity]{run}}) to run the simulation drawing several samples and computing statistics on it. Many different functions are provided to fine-tune the model and the experiment.


\tabular{ll}{
Package: \tab simCity\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-12-11\cr
License: \tab GPL\cr
}
}
\author{
Marcello Gallucci
}
\references{
}
\keyword{ simulations, linear models }
\examples{
### an example with a normally distributed x and two y one with normal residuals and one with residual distributed as a chisq with 2 df.
model<-simc.model()
y1~x,rdist=rnorm 0 1
y2~x, rdist=rchisq 2
#
e<-experiment(model,N=10)
coefs(e)<-list(c(1,1),c(1.1,1.1),c(1.2,1.2),c(1.3,1.3))
run(e)


}
