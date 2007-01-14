\name{plot.sci}
\alias{plot.sci}
\alias{plot.binomRDci}
\alias{plot.poly3ci}
\alias{plot.binomORci}

\title{ Plot simultaneous confidence intervals }
\description{
 Creates a high level plot of confidence intervals from objects returned
 by functions binomRDci, binomORci, poly3ci.
 Plot function for objects of class "sci".
}
\usage{
\method{plot}{sci}(x, line = 0, linelty = 2, linelwd = 1, linecol = "black", CIvert = FALSE, CIlty = 1, CIlwd = 1, HL=TRUE, ...)
\method{plot}{binomRDci}(x,...)
\method{plot}{poly3ci}(x,...)
\method{plot}{binomORci}(x, ...)
}
\arguments{
  \item{x}{ an object of class "sci", "binomRDci", "poly3ci", as can be obtained by calling \code{\link{binomRDci}},\code{\link{poly3ci}}            }
  \item{line}{ as single numeric value or vector, position of line(s) to be added orthogonal to the plotted CI }
  \item{linelty}{ line type(s) for \code{line} as defined in \code{\link{par}} }
  \item{linelwd}{ line width(s) for \code{line} as defined in \code{\link{par}}  }
  \item{linecol}{ colour(s) for \code{line} as defined in \code{\link{par}}  }
  \item{CIvert}{ logical value, specifying whether confidence intervals shall be represented by vertical (if TRUE) or horizontal (if FALSE) lines }
  \item{CIlty}{ single value, line type for confidence intervals as defined in \code{\link{par}}  }
  \item{CIlwd}{ single value, line type for confidence intervals as defined in \code{\link{par}}  }
  \item{HL}{ a logical value; if \code{HL=TRUE} (default), plot margins are chosen appropriately, but several plots can not be combined using \code{layout} or \code{par(mfrow)},
  if \code{HL=FALSE}, several plots can be combined using \code{par(mfrow)} or \code{layout}, but plot margins have to be chosen appropriately by the user}
  \item{\dots}{ parameters to be passed to \code{plot.default}, as documented in
 \code{\link{plot.default}}, \code{\link{plot}}, or \code{\link{par}}  }
 }

\details{
Currently, the \code{plot} parameters \code{ylim, xlim, las, axes, type, pch}
and the \code{axis} parameters \code{labels, las} are chosen internally by the function
and can not be set by the user. 
Currently, combining several plots via \code{par("mfrow")} or \code{\link{layout}} in one device is not possible.

  }
\value{
A plot. 
 }


\examples{

# An example with three dose groups
# compared to Placebo, binomial response:

resp<-c(2,8,6,13)
trials<-c(34,33,36,34)
names(resp)<-c("Placebo", "50", "75", "150")

ci<-binomRDci(x=resp, n=trials,
 type="Dunnett", method="ADD1")
plot(ci, line=c(0,0.1,0.2),linelty=c(1,2,3))

#

data(liarozole)
tab<-table(liarozole)
tab

ci<-binomORci(tab,type="Dunnett", base=4, alternative="greater", success="y")
ci
summary(ci)

layout(matrix(c(1,2),ncol=1))
mosaicplot(t(tab))
par(mar=c(5,8,3,1))
plot(ci, line=1, linelty=3, HL=FALSE)

}
\keyword{ hplot }
\concept{confidence}
\concept{interval}
