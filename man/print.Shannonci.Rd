\name{print.Shannonci}
\alias{print.Shannonci}

\title{ Print function for Shannonci}
\description{
 Print out the confidence intervals calculated by Shannonci.
}
\usage{
\method{print}{Shannonci}(x, ...)
}

\arguments{
  \item{x}{ An object of class \kbd{"Shannonci"}, as produced by calling \code{\link{Shannonci}} }
  \item{\dots}{ currently only \kbd{digits} is passed to \kbd{print} }
}

\value{
A print out.
}
\seealso{ For a more detailed print out, use \code{\link{summary.Shannonci}} }

\keyword{ print }
