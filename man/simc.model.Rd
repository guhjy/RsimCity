\name{simc.model}
\title{Define a set of linear models}
\alias{simCity.model}
\concept{ simCity model }
\description{
  Specify a data generating model for simCity simulations
}
\usage{
  simc.model(file="",exdist=NULL,excov=NULL)
  \cr
  simc.model(file="",exdist=NULL,excov=NULL)
   x~1;edist=rnorm(0,1)
   y~x;edist=rbinom(10,.5); apply=fun()
   #
}
\arguments{
  \item{file}{The (quoted) file from which to read the date generating model specification, including the path to the file if it is not in the current directory. If "" (the default), then the specification is read from the standard input stream, and is terminated by a blank line or \code{#}.}
    \item{exdist}{multivariate distribution of the exogenous variables. If  NULL  \code{\link{rmnorm}} is used. It can be any function which accept N as first argument, mean and excov as second and third argument}
  \item{excov}{covariance matrix for the exogenous variables. It must have names corresponding to the exogenous variable names (see details). If there are exogenous variables and no \code{excov} is provided, an identity matrix is the default }
      \item{fun}{any function which accepts a variable as argument (see details)}

}

\details{

The data generating model can be specified in a text file or in standard input stream should specify an equation per line. Each line features (if not default applies), separated by semicolumns, an \code{equation}, \code{edist=} a function to generate random residuals, \code{apply=} a list of functions to be applied to the generated variable, \code{options=} a list of options to fine-tune the variable. 
An example of line can be:  \cr \code{y~x+z;edit=rnorm(0,1);apply=scale(),exp();options=beta} \cr
  \itemize{ 
    \item{\code{equations}:}{ a formula declaring the linear model. Formula should be specified in a way that can be coerce to \code{\link{as.formula}}. For instance
    \code{y~x+z}. No coefficients should be specified, as they will be specified later during the data generating process (so they can be varied for each run). Constant terms (intercepts) can be specified adding \code{1} as in  \code{y~1+x}. Coefficients (possibly \code{1}) should be provided in the experiment if an intercept is declared. Any artimetics should be passed using \code{\link{I}} function or \code{.()} notation. For instance:  \code{y~x+I(x^2)+z+I(x*z)} or  \code{y~x+.(x^2)+z+.(x*z)} (these two are equivalent). At the moment (version < \code{1.0}), the "\code{:}" notation for interactions is not allowed (use \code{I(x*x)} instead). See examples for further details }


    \item{\code{edist}:} {a function name to be used to generate the residuals of the linear model. The function should accept as its first argument the sample size (N) and can have any argument as one need. For instance, to generate and endogenous variable accordingly to the model \code{y~x} with standardized residuals one would write \code{edist=rnorm(0,1) } (cf. \code{\link{rnorm}}). Binomial residuals can be specified with \code{edist=rbinom(size,prob)} (cf. \code{\link{rbinom}}). If the parameters of the distribution should be changed during the simulation experiment, reference functions can be used (see examples). Any function can be used as long as it accept N as first argument. If no distribution is specified, \code{edist=rnorm(0,1) } is the default. Note that the distribution function is evaluated in the environment of the data.frame being produced, thus it may have variables as arguments (cf \code{\link[simcity]{categorial}} and "Examples") } 
    
    \item{\code{apply}:} {a list of function names separated by comma to be applied to the generated endogenous variable. The function should be writen as \code{func(arg1,arg2)} but it must accept the endogenous variable as its first argument. For instance, to standardize the endogenous variable after it is created, one can use \code{\link{scale}} writing \code{apply=scale()}. To center the variable to its mean one can write \code{apply=scale(scale=FALSE)} which implicitly assumes that the function usage is \code{scale(x,scale=FALSE)}. Note that the  function in \code{apply} is evaluated in the environment of the data.frame being produced, thus it may have variables as arguments (cf \code{\link[simcity]{categorial}} and "Examples")}  

    \item{\code{options}:} {a comma separeted list of options to fine-tune the data generating process for the corresponding endogenous variable. Available options are:

  \itemize{ 
    \item{\code{beta}:}{coefficients that will be passed to the simulation should be intended as betas (standardized coefficients). The endogenous variable is constructed using calculated coefficients such that the simple \code{beta} linking the endogenous and the exogenous variable correspond to the passed coefficient. Note that the resulting betas would correspond to the coefficients passed to the simulation only if the exogenous variables are uncorrelated.}
    \item{\code{trans}:}{the line is intended as a mere \code{trans}formation of the exogenous variable. In practice, when \code{options=trans} is specified, not random residuals are added to the endogenous variable. This options is usefull when complex transformations are needed which are not suitable for using \code{apply}. Choosing to specify a new line for a transformation (with \code{options=trans}) or using the argument  \code{apply} is a matter of taste and convenience, particularly as regard how the coefficients apply to the formulas in generating the data.  For instance in this model \code{y1} and \code{y2} are structurally identical:\cr\cr
    \code{
  simc.model(file="") \cr
   y~exp(x); options=trans \cr
   y~x;apply=exp() \cr
   # 
   \cr
   }
however, when the simulation is run and the coefficients are passed to the model, for model in line one the coefficient \code{c} result in \code{y~c*exp(x)}, whereas in line two it results in  \code{y~exp(c*x)}
}
    
    }  


}    

}
}
\value{
  An object of class \code{\link[simCity]{simCity.model}} which contains (if not default applies):

  \itemize{ 
    \item{\code{equations}:}{ an object containing, for each equation specified in the model:
  \itemize{ 
  \item{\code{equation}:} {the equation specified for one linear model}    
  \item{\code{edist}:} {the function producing the residual distribution of the corresponding linear model. }
  \item{\code{options}:} {a  list of options passed to the model }
  \item{\code{apply}:} {a list of functions to apply on the generated endogenous variable }  
    }
    }
    \item{\code{endogenous}:} {a list of endogenous variables name (as.character)}
    \item{\code{exogenous}:} {a list of exogenous variables name (as.character)}
    }
 The object can be passed to to generate one sample, to  to set up a complete experiment  
}

\seealso{
  \code{\link[stats]{lm}}
}
\examples{
### define a simple model with two variables  normally distributed, x standardized.
  simc.model()
   x~1;edist=rnorm(0,1)
   y~x
   #

### define a simple model with two variables  normally distributed, both standardized.
  model<-simc.model()
   x~1;edist=rnorm(0,1)
   y~x;apply=scale()
   #
  print(model)
### define a model with two uncorrelated and standardized exogenous variables  a endogenous variable with binomial residuals.
  model<-simc.model()
   y~x+z;edist=rbinom(100,.5)
   #
### define a model with two correlated exogenous variables with different variances and  a endogenous variable with binomial residuals.
  covs<-matrix(c(2,.3,.3,4),ncol=2)
  colnames(covs)<-rownames(covs)<-c("x","z")
  model<-simc.model(excov=covs)
   y~x+z;edist=rbinom(100, .5)
   #

### define a model with two standardized correlated exogenous variables endogenous variable with binomial residuals and exponential relations with the exogenous variable.
  covs<-matrix(c(1,.3,.3,1),ncol=2)
  colnames(covs)<-rownames(covs)<-c("x","z")
  model<-simc.model(excov=covs)
  y~x+z;rbinom(100,.5);apply=exp()
   #

### define a model with two exogenous variables and an interaction on the endogenous. Coefficients will be passed as standardized coefficients
  model<-simc.model()
  y~x+z+I(x*z);rbinom(100,.5);options=beta
   #

### define a series of polinomial models
  model<-simc.model()
  y~x+I(x^2)+I(x^3)
  z~x+I(exp(log(x)*1/2))+I(exp(log(x)*1/3))+I(exp(log(x)*1/4))
  #

### define a mediational model with non standardized residual
  model<-simc.model()
  m~x; edist=rnorm(5,10)
  y~m+x, edist=rnorm(2, 10)
  #

### define a structural factorial model with two correlated latent factors and 3 observed variables for each factor with etherogenous distribution of residuals.
  covs<-matrix(c(1,.2,.2,1),ncol=2)
  colnames(covs)<-rownames(covs)<-c("xi","eta")
  model<-simc.model(excov=covs)
  x1~xi 
  x2~xi; binom(10,.5)
  x2~xi; uniform(0,1)
  x3~eta; rnorm(0,1)
  x4~eta; binom(10,.5)
  x5~eta; uniform(0,1 )
  #

### use of reference functions: define a model with a latent variable, a continuous measure and two dichotomized versions of it with different cut-offs  
  mysplit<-function(x,p) {
                    x<-x>quantile(x,p)
                    as.numeric(x)
}
  
  model<-simc.model()
  x~csi; rnorm 0 1
  d1~x; apply=mysplit(.5)
  d2~x, apply=mysplit(.7)
  #

### use of reference functions: define a model with a latent variable, two continuous measures  with different percentace of missing values
  mymissing<-function(x,p) {
                    x[p<runif(length(x),0,1)]<-NA
                    as.numeric(x)
}  
  model<-simc.model()
  x~csi, rnorm 0 1
  d1~x, apply=mymissing(.05)
  d2~x, apply=mymissing(.15)
  #


}
\keyword{models}
