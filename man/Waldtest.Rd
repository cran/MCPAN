\name{Waldtest}
\alias{Waldtest}

\title{ Simultaneous Wald tests}
\description{
  General function for adjusted p-values for an UIT in a one-way layout using multivariate normal distribution.
}
\usage{
Waldtest(estp, varp, cmat, alternative = "greater", dist="MVN")
}
\arguments{ 
  \item{estp}{ numeric vector of point estimates of length I, with I = the number of samples}
  \item{varp}{ numeric vector of variance estimates of length I, to be used for interval construction }
  \item{cmat}{ Contrast matrix of dimension MxI, with M = the number of contrasts, I= the number of samples }
  \item{alternative}{ character string }
 \item{dist}{a character string, where \code{"MVN"} invokes the computation of p-values using the multivariate normal distribution,
 and \code{"N"} invokes use p-value computation using the univariate normal distribution}
}

\value{
A list containing:
\item{teststat}{a numeric vector of teststatistics of length M}
\item{pval}{a single numeric p-value, the p-value of the maximum test (minimum p-value)} 
\item{p.val.adj}{ a vector of length M, the adjusted p-values of the single contrasts}
\item{alternative}{a single character vector, as the input}
\item{dist}{a character string specifying which distribution}
}


\author{ Frank Schaarschmidt }

\seealso{
For user level implementations see:

\code{\link{binomRDtest}},
\code{\link{poly3test}}

}

\keyword{ htest }