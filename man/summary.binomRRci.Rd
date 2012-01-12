\name{summary.binomRRci}
\alias{summary.binomRRci}
\title{ Detailed print out for binomRRci }
\description{
 Produces a more detailed print out of objects of class "binomRRci", including summary statistics,
 the used contrast matrix and the confidence intervals.
}
\usage{
\method{summary}{binomRRci}(object, ...)
}
\arguments{
  \item{object}{ an object of class "binomRRci" as created by function \code{\link{binomRRci}} }
  \item{\dots}{ further arguments to be passed to summary, currently only \code{digits} for rounding is supported }
}

\value{
 A print out.
}

\examples{



data(liarozole)

head(liarozole)

LiDu<-binomRRci(Improved ~ Treatment, data=liarozole,
 type="Dunnett", alternative="greater")

LiDu

summary(LiDu)



}

\keyword{ print }
