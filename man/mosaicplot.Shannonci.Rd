\name{mosaicplot.Shannonci}
\alias{mosaicplot.Shannonci}
\alias{mosaicplot.Simpsonci}

\title{ Mosaicplot for Shannonci and Simpsonci}
\description{
Create a mosaicplot from objects of class Shannonci or Simpsonci
}
\usage{
\method{mosaicplot}{Shannonci}(x, decreasing = NULL, ...)
\method{mosaicplot}{Simpsonci}(x, decreasing = NULL, ...)
}

\arguments{
  \item{x}{ an object of class \kbd{"Simpsonci"} or \kbd{"Shannonci"} as can be obtained from calling \link{Simpsonci} or \link{Shannonci} }
  \item{decreasing}{ a single logical value, indicating whether the species should be plotted in the current order (if \kbd{decreasing=NULL}),
 in decreasing order (if \kbd{decreasing=TRUE}), or in increasing order (if \kbd{decreasing=FALSE}) }
  \item{\dots}{ further arguments to be passed to \kbd{mosaicplot}, see \kbd{?mosaicplot} and \kbd{?par} for details }
}
\details{
 This function uses the counts in \kbd{[["sample.estimate"]][["table"]]} to produce a mosaicplot.
}
\value{
A plot.
}

\examples{

data(HCD)

HCDFam <- HCD[,-1]

SCI<-Simpsonci(X=HCDFam, f=HCD[,1])

mosaicplot(SCI, decreasing=TRUE, col=rainbow(n=8))

}

\keyword{ hplot }
\concept{ diversity }
