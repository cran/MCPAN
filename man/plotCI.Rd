\name{plotCI}

\alias{plotCI}
\alias{plotCI.default}
\alias{plotCI.sci}
\alias{plotCI.sci.ratio}
\alias{plotCI.confint.glht}

\title{ Plot confidence intervals }
\description{
 A function for convenient plotting of confidence intervals.
}
\usage{
\method{plotCI}{default}(x, ...)
\method{plotCI}{sci}(x, ...)
\method{plotCI}{sci.ratio}(x, ...)
\method{plotCI}{confint.glht}(x, ...)
}

\arguments{
  \item{x}{ An object of class "sci", "sci.ratio" or "conf.int.glht" or a list with elements \kbd{estimate}, containing a numeric vector,\kbd{conf.int}, containing a matrix with two columns, giving the lower and upper bounds, and a string \kbd{alternative}, one of "two.sided", "less", "greater"  }
  \item{\dots}{ additional arguments to be passed to \link{plotCII} }
}
\details{
 Plots the estimates, upper and lower limits using \kbd{points} and \kbd{segments}. The names of \kbd{estimate} are passed as labels of the confidence intervals.
 If infinite bounds occur, the plot region is limited by the most extreme non infinite bound or estimate.
}
\value{
  A plot.
}


\seealso{ Internally, the function \link{plotCII} is used.}
\examples{

x=c(8,9,9,18,39,44)
n=c(2000,2000,2000,2000,2000,2000)

x<-binomORci(x=x, n=n, names=c("0","120","240","480","600","720"))

plotCI(x, lines=1)


}

\keyword{ hplot }
\concept{ confidence interval }
