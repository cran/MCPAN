\name{plotCII}
\alias{plotCII}

\title{ Plot confidence intervals }
\description{
 A function for convenient plotting of confidence intervals.
}
\usage{
plotCII(estimate, lower = NULL, upper = NULL,
 alternative = c("two.sided", "less", "greater"),
 lines = NULL, lineslty = 2, lineslwd = 1, 
 linescol = "black", CIvert = FALSE, CIlty = 1,
 CIlwd = 1, CIcex = 1, CIcol = "black",
 HL = TRUE, ...)
}

\arguments{
  \item{estimate}{ a numeric vector }
  \item{lower}{ an optional numeric vector, of the same length as estimate }
  \item{upper}{ an optional numeric vector, of the same length as estimate }
  \item{alternative}{ a single character string, one of \code{"two.sided"}, \code{"less"}, \code{"greater"} }
  \item{lines}{ a numeric (vector) giving the position(s) of line(s) to draw into the plots orthogonal to the confidence intervals }
  \item{lineslty}{ possible a vector of line type of the \code{lines}, see the options for \code{lty} in \code{\link{par}} }
  \item{lineslwd}{ possible a vector of line width of the \code{lines}, see the options for \code{lwd} in \code{\link{par}} }
  \item{linescol}{ possible a vector of line colors of the \code{lines} }
  \item{CIvert}{ logical indicating, whether confidence intervals shall be drawn horizontal (default), or vertical (if set to TRUE) }
  \item{CIlty}{ single value, to specify the line type used for the CI, see options for \code{lty} in \code{?par} }
  \item{CIlwd}{ single value, to specify the width type used for the CI, see options for \code{lty} in \code{?par}  }
  \item{CIcex}{ single value, to specify the extension of sympols in the plot, see options for \code{cex} in \code{?par}  }
  \item{CIcol}{ single value, to specify the color used for the CI  }
  \item{HL}{ a logical, if TRUE (default), plot margins of the are adjusted depending on the length of the names by appropriate calls to \code{par}; this might be incompatible with combining the plot with others in the same device. If set to FALSE, 
 its up to the user to choose appropriate plot margins by calling to \code{par}.   }
  \item{\dots}{  further arguments to be passed to \code{plot}  }
}
\details{
 Test version.
}
\value{
A plot.
}

\seealso{  \code{\link{plotCI}}, for more convenient methods }
\examples{

}

\keyword{ hplot }
\concept{ confidence interval }
