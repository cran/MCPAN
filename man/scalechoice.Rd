\name{scalechoice}
\alias{scalechoice}

\title{ Only for internal use.}
\description{
How to define scale parameters of the Weibull distribution in the poly-k-setting  }
\usage{
scalechoice(shape = 3, pm = 0.2, t = 1)
}

\arguments{
  \item{shape}{ shape parameter (usually shape=3) }
  \item{pm}{ a numeric value (0<p<1), probability that an individual dies before time t }
  \item{t}{ a numeric value, time t }
}
\value{
  a single numeric value, giving the scale parameter needed to fulfill pm(t) for the current definition of the Weibull disstribution.}


\note{
Scale parameter is defined differently than that in Peddada (2005)
 }
\seealso{ \kbd{?pweibull} }

\keyword{ internal }