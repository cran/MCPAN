\name{poly3table}
\alias{poly3table}
\title{ Summarize long term carcinogenicity data }
\description{
   Function to summarize data of long term carcinogenicity trials in a text format.
 Data are assumed to consist of (1) a dichotomous variable, 
 defining whether the tumour of interest was present in an individual animal at time of death,
 and (2) a numeric variable containing the time of death of an individual animal,
 and (3) a grouping factor.
}
\usage{
poly3table(time, status, f, tumour = NULL, symbol = "*")
}
\arguments{
  \item{time}{ a numeric vector, containing the time of death of an individual }
  \item{status}{ a logical (or dichotomous categorical) vector }
  \item{f}{ a factor, specifying treatment groups}
  \item{tumour}{ the value which \code{status} obtains if a tumour is present in an individual at time of death  }
  \item{symbol}{ symbol to indicate presence of tumour in the text representation }
}

\value{ A named list, containing a character string for each group
}

\author{ Frank Schaarschmidt }
\examples{
data(methyl)
methyl
poly3table(time=methyl$death, status=methyl$tumour,
 f=methyl$group, tumour = 1, symbol = "*")

}

\keyword{ print }
\concept{poly-k}
