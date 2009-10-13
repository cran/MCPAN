\name{summary.binomRDci}
\alias{summary.binomRDci}
\title{ Detailed print out for binomRDci }
\description{
 Produces a more detailed print out of objects of class "binomRDci", including summary statistics,
 the used contrast matrix and the confidence intervals.
}
\usage{
\method{summary}{binomRDci}(object, ...)
}
\arguments{
  \item{object}{ an object of class "binomRDci" as created by function \link{binomRDci} }
  \item{\dots}{ further arguments to be passed to summary, currently only \kbd{digits} for rounding is supported }
}

\value{
 A print out.
}

\examples{



data(liarozole)

head(liarozole)

LiWi<-binomRDci(Improved ~ Treatment, data=liarozole,
 type="Williams")

LiWi

summary(LiWi)



}

\keyword{ print }
