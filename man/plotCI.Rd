\name{plotCI}

\alias{plotCI}
\alias{plotCI.default}
\alias{plotCI.sci.ratio}
\alias{plotCI.confint.glht}

\title{ Plot confidence intervals }
\description{
 A function for convenient plotting of confidence intervals.
}
\usage{
\method{plotCI}{default}(x, ...)
\method{plotCI}{sci.ratio}(x, ...)
\method{plotCI}{confint.glht}(x, ...)
}

\arguments{
  \item{x}{ An object of class "sci.ratio" or "conf.int.glht" or a list with elements \code{estimate}, containing a numeric vector,\code{conf.int}, containing a matrix with two columns, giving the lower and upper bounds, and a string \code{alternative}, one of "two.sided", "less", "greater"  }
  \item{\dots}{ additional arguments to be passed to \code{\link{plotCII}} }
}
\details{
 Test version.
}
\value{
  A plot.
}


\seealso{ The function used internally is \code{\link{plotCII}}}
\examples{

}

\keyword{ hplot }
\concept{ confidence interval }
