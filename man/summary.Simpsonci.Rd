\name{summary.Simpsonci}
\alias{summary.Simpsonci}

\title{ Summary function for Simpsonci }
\description{
Produces a detailed print out of the results of function Simpsonci.
}
\usage{
\method{summary}{Simpsonci}(object, ...)
}

\arguments{
  \item{object}{ an object of class \code{"Simpsonci"} as obtained by calling \code{\link{Simpsonci}} }
  \item{\dots}{ further arguments to be passed to print and round: currently only \code{digits}}
}

\value{

A print out, comprising a table of the (possibly aggregated) data used for estimation,
the sample estimates for the Simpsons index, and its variance estimates,
the used contrast matrix, and the confidence intervals.

}

\examples{

data(HCD)

HCDcounts<-HCD[,-1]
HCDf<-HCD[,1]

SimpsonS<-Simpsonci(X=HCDcounts, f=HCDf, type = "Sequen",
 alternative = "greater", conf.level = 0.95, dist = "MVN")

summary(SimpsonS)

}


\keyword{ print }