\name{coefs}
\title{Set up a simCity experiment coefficients}
\alias{simCity.coefs}
\alias{coefs}

\concept{ simCity model }
\description{
Set up coefficients for a simCity simulation experiment. Coefficients are set up for multiple runs of the same model, either trasversing the list of all coefficients vectors provided or creating a factorial combination of all possible coefficients values. Denote wth K the coefficients of a model and with R the number of experimental blocks one wish to run. Coefficients my be passed as a list of R vectors of lenght K, each vector featuring the K coefficients for a block. Alternative, they can be passed a list of K vectors of variable lenght each featuring all possible values the correspondig coefficient may assume. Furtheremore, one can pass the coefficients as a matrix of dimension R,K, with each row featuring the coefficients for a block.   
}
\usage{

coefs(exper,coefsobj,mode="by.equation",...) 

}
\arguments{
  \item{exper}{ an object of class \code{\link[simCity]{simCity.experiment}} }
  \item{coefsobj}{ a list of coefficients}
  \item{mode}{ control the way coefficients list is passed. If \code{mode="by.equation"} the list is assumed to be a list of R vectors of lenght K, each vector providing the coefficients as accepted by \code{\link[simCity]{make.data}} or  \code{\link[simCity]{run.block}} . The number of vectors define how many levels the experiment has (non considering variations in N of the \code{\link[simCity]{experiment}}). If \code{mode="by.term.match"} the list should have K vectors, each containing the entire set of coefficients for a single term (see Details and Examples). The ectors are matched in length (reusing values for shorter vectors). The number of replications R will be equal to the lenght of the longest vector. If \code{mode="by.term.factorial"} the  list should have K vectors, each containing the entire set of coefficients for a single term. A factorial design is built combining all possiable values of the vectors. The number of replications are equal to the number of combinations of all values of the vectors. Finally, coefficients may be passed as a R by K matrix, with the \code{mode} defining whetehr the coefficients are matched or focatorially expanded.}

\item{\dots}{not implemented}
}
\details{
The sequence of coefficients are passed in as a list of vectors. There are different ways to do that.  \code{mode="by.equation"} requires coefficients to be passed as a list of vector in which each list can be applied to the model as it is. Thus, each vector must contains K coefficients where K is then number of terms in the model. For instance, given a model defining two equations, \code{y~x+z} and \code{w~z}, passing a list \code{list(c(1,2,3),c(1.1,2.1,3.1),c(1.2,2.2,3.2)} would run three blocks with data generating equations 1. \code{y~1*x+2*z} and \code{w~3*z}, 2. \code{y~1.1*x+2.1*z} and \code{w~3.1*z}, 3. \code{y~1.2*x+2.2*z} and \code{w~3.2*z}. \cr \cr
\code{mode="by.term.match"} and \code{mode="by.term.factorial"} allow to pass the entire set of coefficients for a single term as a sequence. Each sequence is associated with the corresponding term in the same order as terms appears in the model. Thus, for the model  \code{y~x+z} and \code{w~z} one can pass \code{list(c(1,1.1,1.2), c(2,2.1,2.2)), c(3,3.1,3.2))}. The latter is handy for passing coefficients build as \code{\link{seq}}, as one can pass  \code{list(seq(1,10,.2), seq(2,20,.2), seq(3,30,.2))} . If \code{mode="by.term.factorial"} a full factorial design is build up combining all values of coefficients, if \code{mode="by.term.match"} coefficients are matched.
}
\value{
  the attribute $coefs of the  object of class \code{\link[simCity]{simCity.experiment}} is modified.
}
\seealso{
  \code{\link[simCity]{simc.model}},  \code{\link[simCity]{make.data}}, \code{\link[simCity]{run.block}},\code{\link[simCity]{run}},\code{\link[simCity]{experiment}}
}
\examples{


### set up a simple experiment on correlations. 

getstats<-function(model,modelcoefs,simdata) {
  n.case<-dim(simdata)[1]
  rstat1=
  rstat2=cor.test(simdata$x,simdata$y)
  v<-c("ncase"=n.case,"coefs"=unlist(modelcoefs),
       "rzy"=cor(simdata$z,simdata$y),"rxy"=cor(simdata$z,simdata$y))
  as.data.frame(v)
}

myaggregate<-function(results) {
  aggregate(results,list(coefs),mean)
}

s.model<-simc.model()
x~1+z,rnorm 0 1
y~x+z
#
myexp<-experiment(s.model,N=50,rep=10,getstats,myaggregate)
### by.equation mode, varing the second  coefficient fro 1 to 4 and the third from 2 to 3.
by.equation.c<-list(
  list(2,1,2,4),list(2,2,2,4),list(2,3,2,4),list(2,4,2,4),list(2,5,2,4),
  list(2,1,3,4),list(2,2,3,4),list(2,3,3,4),list(2,4,3,4),list(2,5,3,4))
coefs(myexp)<-by.equation.c
coefs(myexp)
# run(myexp)

### by.term.match mode, same coefficients.
by.term.c<-list(1,1:4,2:3,4)
by.term.c
coefs(myexp,mode='by.term.match')<-by.term.c
coefs(myexp)
# run(myexp)
### by.term.factorial mode, same coefficients.
coefs(myexp,mode='by.term.factorial')<-by.term.c
coefs(myexp)
# run(myexp)
### passing a matrix.

mcoefs<-matrix(1:5,ncol=4,nrow=5)
coefs(myexp,mode='by.term.match')<-mcoefs
coefs(myexp)
coefs(myexp,mode='by.term.factorial')<-mcoefs
head(coefs(myexp))
head(mcoefs)



}
\keyword{models}
