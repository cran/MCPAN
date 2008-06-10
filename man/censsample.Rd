\name{censsample}
\alias{censsample}

\title{ Random data for Poly-k }
\description{
Random numbers from two independent Weibull distributions for Mortality and tumour induction.
}
\usage{
censsample(n, scale.m, shape.m,
 scale.t, shape.t = 3, tmax)
}
\arguments{
  \item{n}{ a single numeric value, the number of individuals }
  \item{scale.m}{  a single numeric value, scale parameter of the Weibull distribution for mortality }
  \item{shape.m}{ a single numeric value, shape parameter of the Weibull distribution for mortality }
  \item{scale.t}{ a single numeric value, scale parameter of the Weibull distribution for tumour induction }
  \item{shape.t}{ a single numeric value, shape parameter of the Weibull distribution for tumour induction }
  \item{tmax}{ a single numeric value, maximum time in the trial }
}

\value{ A data.frame with columns
   \item{time }{a numeric vector of length \code{n}, the time of death of an individual}
   \item{status }{a logical vector of length \code{n}, the tumour status at time of death (TRUE: tumour present, FALSE: no tumour present)}
   \item{T.t}{time of tumour induction (unobservable)}
   \item{T.m}{time of death}
   \item{tmax}{maximum time of death}
}

\keyword{datagen}
