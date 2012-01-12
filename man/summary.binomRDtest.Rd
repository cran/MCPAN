\name{summary.binomRDtest}
\alias{summary.binomRDtest}

\title{ Detailed print out for binomRDtest }
\description{
 Produces a more detailed print out of objects of class "binomRDtest",
 including summary statistics,
 the used contrast matrix and the p-values.
}
\usage{
\method{summary}{binomRDtest}(object, ...)
}
\arguments{
  \item{object}{ an object of class "binomRDtest" as created by function \link{binomRDtest}  }
  \item{\dots}{ further arguments to be passed to summary, currently only \kbd{digits} for rounding is supported }
}
\value{
 A print out.
}

\examples{


ntrials <- c(40,20,20,20)
xsuccesses <- c(1,2,2,4)
names(xsuccesses) <- LETTERS[1:4]
test<-binomRDtest(x=xsuccesses, n=ntrials, method="ADD1",
 type="Changepoint", alternative="greater")

test

summary(test)

}

\keyword{ print }

