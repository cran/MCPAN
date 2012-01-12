\name{SCSrank}
\alias{SCSrank}
\title{Compute a rectangular simultaneous confidence set from a sample of a joint empirical distribution.}
\description{
Given a large sample of N values from an M dimensional joint empirical distribution, the rank based method of Besag et al. (1995) is used to compute a rectangular M-dimensional 'confidence' set
that includes N*conf.level values of the sample.
}
\usage{
SCSrank(x, conf.level = 0.95, alternative = "two.sided", ...)
}

\arguments{
  \item{x}{
an N x M matrix containg N sampled values of the M dimensional distribution of interest
}
  \item{conf.level}{
the simultaneous confidence level, a single numeric value between 0 and 1, defaults to 0.95 for simultaneous 95 percent sets
}
  \item{alternative}{
a single character string related to hypotheses testing, \code{"two.sided"} invokes two-sided confidence sets, \code{"less"} invokes sets with upper limits only and \code{"greater"} invokes sets with lower limits only, 
}
  \item{\dots}{ currently ignored}
}


\value{
an Mx2 (\code{alternative="two.sided"}) matrix containing the lower and upper confidence limist for the M dimensions,
in case of \code{alternative="less"}, \code{alternative="greater"} the lower and upper bounds are replaced by \code{-Inf} and \code{Inf}, respectively.
}
\references{
Besag J, Green P, Higdon D, Mengersen K (1995). Bayesian Computation and Stochastic Systems. Statistical Science 10, 3-66.
Mandel M, Betensky RA. Simultaneous confidence intervals based on the percentile bootstrap approach. Computational Statistics and Data Analysis 2008; 52(4): 2158-2165.
}
\author{
Frank Schaarschmidt
}

\examples{

x <- cbind(rnorm(1000,1,2), rnorm(1000,0,2), rnorm(1000,0,0.5), rnorm(1000,2,1))
dim(x)
cm <- rbind(c(-1,1,0,0), c(-1,0,1,0), c(-1, 0,0,1))
xd <- t(apply(x, 1, function(x){crossprod(t(cm), matrix(x))}))
pairs(xd)

SCSrank(xd, conf.level=0.9)

}

\keyword{ htest }
