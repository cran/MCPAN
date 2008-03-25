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


\keyword{ print }

