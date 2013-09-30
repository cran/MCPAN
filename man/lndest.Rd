\name{lndest}
\alias{lndest}
\alias{lnrest}

\title{
Point and varioance estimates for expected and log(expected values) of lognormal samples
}
\description{
Compute the ML estimates of the expected values and the log of the expected values given several samples of lognormal data, separated by a factor variable. For internal use in \code{lndci} and \code{lnrci}.
}
\usage{
lndest(x, f)
lnrest(x, f)
}

\arguments{
  \item{x}{
a numeric vector 
}
  \item{f}{
a factor variable (with at least two levels)
}
}

\value{
A list with elements
  \item{estimate }{ a numeric vector, the point estimates for each factor level}
  \item{varest }{ a numeric vector, the variance estimates (evaluated at the point estimates) for each factor level}
  \item{n }{ a numeric vector, the sample size of each factor level}
}
\references{
Chen, Y-H, Zhou, X-H (2006). Interval estimates for the ratio and the difference of two log-normal means. Statistics in Medicine 25, 4099-4113.
}

\keyword{ internal }
