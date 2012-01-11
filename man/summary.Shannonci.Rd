\name{summary.Shannonci}
\alias{summary.Shannonci}

\title{ Summary for Shannonci}
\description{
  Produces a detailed print out of the results of the function Shannonci.
}
\usage{
\method{summary}{Shannonci}(object, ...)
}

\arguments{
  \item{object}{ An object of class \code{"Shannonci"}, see \code{\link{Shannonci}} }
  \item{\dots}{ further arguments to be passed to print, currently only \code{digits} }
}

\value{

A print out, comprising a table of the (possibly aggregated) data used for estimation,
the sample estimates for the Shannon index with bias corrected and raw values, its variance estimates,
the used contrast matrix, and the confidence intervals.

}

\examples{


data(HCD)

HCDcounts<-HCD[,-1]
HCDf<-HCD[,1]

# Comparison to the confidence bounds shown in
# Fritsch and Hsu (1999), Table 5, "Standard normal".

cmat<-rbind(
"HM-HU"=c(0,1,-1),
"HL-HM"=c(1,-1,0),
"HL-HU"=c(1,0,-1)
)

ShannonS<-Shannonci(X=HCDcounts, f=HCDf, type = "Sequen",
 alternative = "greater", conf.level = 0.9, dist = "N")

summary(ShannonS)

}


\keyword{ print }

