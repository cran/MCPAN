\name{summary.binomORci}
\alias{summary.binomORci}

\title{ Detailed print out for binomORci }
\description{
Produces a more detailed print out of objects of class "binomORci", including summary statistics,
 the used contrast matrix and the confidence intervals.
}
\usage{
\method{summary}{binomORci}(object, ...)
}
\arguments{
  \item{object}{ an object of class "binomORci" as created by function \code{\link{binomORci}} }
  \item{\dots}{ ... }
}

\value{
A print out.
}


\examples{

x<-c(1,3,6,7,5)
n<-c(30,30,30,30,30)
names<-LETTERS[1:5]

ORD<-binomORci(x=x, n=n, names=names,
 type="Dunnett", alternative="greater")
summary(ORD)

ORW<-binomORci(x=x, n=n, names=names,
 type="Williams", alternative="greater")
summary(ORW)


}
\keyword{ print }

