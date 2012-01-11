\name{corrMatgen}
\alias{corrMatgen}

\title{ A function to calculate the correlation matrix }
\description{
Correlation matrix for teststatistics and confidence intervals assuming multivariate standard normal distribution
}
\usage{
corrMatgen(CM, varp)
}
\arguments{
  \item{CM}{ a matrix of contrast coefficients, dimension MxI, where M=number of contrasts, and I=number of groups in a oneway layout }
  \item{varp}{ a numeric vector of groupwise variance estimates (length = I) }
}

\value{
A matrix of dimension MxM. 

}

\references{
For correlation of contrasts of binomial proportion, see:
 Bretz F, Hothorn L.:
 Detecting dose-response using contrasts: asymptotic power and sample size determination for binomial data.
 Statistics in Medicine 2002; 21: 3325-3335. }

\keyword{ htest }