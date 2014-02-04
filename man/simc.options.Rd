\name{simc.options}
\title{Set or Get options for simCity simulations}
\alias{simc.options}

\concept{ simCity model }
\description{
  Set or Get options for simCity simulations based on   \code{\link[simCity]{simc.model}}  and \code{\link[simCity]{run}}
}
\usage{
simc.options(option=value)
simc.options(option)
simc.options()
}
\arguments{
  \item{option}{ the option to set }
  \item{value}{ the value to set }

}

\details{
if a value is provided, it sets the option, othewise retrives the options or (if no argument is passed), all options are retrieved.  
}
\value{
 The value of the options, or all options set.
}
\seealso{
  \code{\link[simCity]{model}},  \code{\link[simCity]{run}}
}
\examples{

simc.options(verbose=T)
simc.options(verbose)
simc.options()
simc.options(verbose=F)


}
\keyword{models}
