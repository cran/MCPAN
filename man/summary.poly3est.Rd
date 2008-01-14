\name{summary.poly3est}
\alias{summary.poly3est}

\title{ Detailed print out for poly3est }
\description{
Summary statistics for long-term carcinogenicity data, including poly-3-estimates.
For internal use.
}
\usage{
\method{summary}{poly3est}(object, ...)
}

\arguments{
  \item{object}{ An object of class "poly3est", as can be obtained by \code{\link{poly3est}} }
  \item{\dots}{ further argument for the print out, as e.g. \code{digits} for rounding }
}
\details{For internal use.}

\value{
A print out.
}

\author{ Frank Schaarschmidt }

\examples{

data(methyl)
head(methyl)

estk3<-poly3estf(time=methyl$death, status=methyl$tumour, f=methyl$group)
summary(estk3)

estk5<-poly3estf(time=methyl$death, status=methyl$tumour, f=methyl$group, k=5)
summary(estk5)

}

\keyword{ print }
