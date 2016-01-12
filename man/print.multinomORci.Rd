\name{print.multinomORci}
\alias{print.multinomORci}
\alias{as.data.frame.multinomORci}
\title{
Print out put of multinomORci
}
\description{
For output of function multinomORci: print ot confidence intervals or coerce out put to a data.frame.
}
\usage{
\method{print}{multinomORci}(x, exp = TRUE, ...)
\method{as.data.frame}{multinomORci}(x, row.names = NULL, optional = FALSE, exp = TRUE, ...)
}

\arguments{
  \item{x}{an object of class \code{multinomORci}}
  \item{exp}{logical; if \code{exp=TRUE} interval limits are exp-transformed to yield limits on the sclae of odds-ratios; if \code{exp=FALSE} interval limits are printed/Returned on the logit scale}
  \item{row.names}{see \code{as.data.frame}}
  \item{optional}{see \code{as.data.frame}}
  \item{\dots}{arguments to be passed to \code{print}, or \code{as.data.frame}}
}

\examples{

# fakle data: 3 categories (A,B,C) in 4 tretament groups c(co,d1,d2,d3)
dm <- rbind(
"co" = c(15,5,5),
"d1" = c(10,10,5),
"d2" = c(5,10,10),
"d3" = c(5,5, 15))
colnames(dm)<- c("A","B","C")

dm

#  define and name odds between categories
cmodds <- rbind(
"B/A"=c(-1,1,0),
"C/A"=c(-1,0,1))

#  define and name comparsions between groups
cmtrt <- rbind(
"d1/co"=c(-1,1,0,0),
"d2/co"=c(-1,0,1,0),
"d3/co"=c(-1,0,1,0))

TEST <- multinomORci(Ymat=dm, cmcat=cmodds, cmgroup=cmtrt, cimethod="DP", BSIM=1000, prior=1)
TEST
print(TEST, exp=FALSE)
as.data.frame(TEST)

}

\keyword{ misc }
