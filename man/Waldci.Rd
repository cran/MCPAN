\name{Waldci}
\alias{Waldci}
\title{ Simultaneous Wald confidence intervals }
\description{
  General function for simultaneous CIs in a one-way layout using multivariate normal distribution.

}
\usage{
Waldci(cmat, estp, varp, varcor, alternative = "two.sided", conf.level = 0.95, dist="MVN")
}

\arguments{
  \item{cmat}{ Contrast matrix of dimension MxI, with M = the number of contrasts, I= the number of samples }
  \item{estp}{ numeric vector of point estimates of length I, with I = the number of samples}
  \item{varp}{ numeric vector of variance estimates of length I, to be used for interval construction }
  \item{varcor}{ numeric vector of variance estimates of length I }
  \item{alternative}{ character string }
  \item{conf.level}{  single numeric vector }
  \item{dist}{a character string, \code{"MVN"} invokes multiplicity adjustment via the multivariate normal distribution,
\code{"N"} invokes use of quantiles of the univariate normal distribution}
}
\details{
Mainly for internal use.

}
\value{
A list containing:
  \item{conf.int }{a matrix with 2 columns: lower and upper confidence bounds, and M rows}
  \item{alternative }{character string, as input}
  \item{conf.level}{single numeric value, as input}
  \item{quantile}{the quantile used to construct the CIs}

}
\author{ Frank Schaarschmidt}

\seealso{For user level implementations see: 
\code{\link{binomRDci}}, 
\code{\link{binomORci}},
\code{\link{poly3ci}}
}

\keyword{ htest }