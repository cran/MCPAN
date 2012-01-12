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
  \item{x}{ an object of class \kbd{"binomRDci"}, \kbd{"binomORci"}, \kbd{"binomRRci"}, \kbd{"poly3ci"}, \kbd{"sci"} }
  \item{\dots}{ further arguments as described in \link{plotCI} }
} 
\details{
Extracts some values and calls \link{plotCI}.

}
\value{
A plot.
}

\keyword{ hplot }
\concept{confidence interval}
