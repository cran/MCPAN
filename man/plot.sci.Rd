\name{plot.sci}
\alias{plot.sci}
\alias{plot.binomRDci}
\alias{plot.binomORci}
\alias{plot.binomRRci}
\alias{plot.Shannonci}
\alias{plot.Simpsonci}

\alias{plot.poly3ci}

\title{ Plot confidence intervals }
\description{
Function for convenient plotting of confidence intervals.
}
\usage{
\method{plot}{sci}(x, ...)
\method{plot}{binomRDci}(x, ...)
\method{plot}{binomORci}(x, ...)
\method{plot}{binomRRci}(x, ...)
\method{plot}{poly3ci}(x, ...)
\method{plot}{Shannonci}(x, ...)
\method{plot}{Simpsonci}(x, ...)

}

\arguments{
  \item{x}{ an object of class \code{"binomRDci"},\code{"binomORci"},\code{"binomRRci"},\code{"poly3ci"},\code{"sci"} }
  \item{\dots}{ further arguments as described in \code{\link{plotCI}} }
} 
\details{
Extracts some values and calls \code{\link{plotCI}}.

}
\value{
A plot.
}

\examples{

}

\keyword{ hplot }
\concept{confidence interval}
