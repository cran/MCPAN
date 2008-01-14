\name{censsamplef}
\alias{censsamplef}
\title{ Random data for Poly-k  }
\description{
Random data for Poly-k for a one-way layout, with I groups.
}
\usage{
censsamplef(n, scale.m, shape.m, scale.t, shape.t = 3, tmax)
}

\arguments{
   \item{n}{ a numeric vector, the numbers of individuals of length I }
  \item{scale.m}{  a numeric vector, scale parameters of the Weibull distribution for mortality }
  \item{shape.m}{ a numeric vector, shape parameters of the Weibull distribution for mortality }
  \item{scale.t}{ a numeric vector, scale parameters of the Weibull distribution for tumour induction }
  \item{shape.t}{ a numeric vector, shape parameters of the Weibull distribution for tumour induction }
  \item{tmax}{ a single numeric value, maximum time in the trial }
}
\details{


  }
\value{A data.frame with columns
   \item{time}{a numeric vector of length \code{n}, the time of death of an individual}
   \item{status}{a logical vector of length \code{n}, the tumour status at time of death (TRUE: tumour present, FALSE: no tumour present)}
   \item{T.t}{time of tumour induction (unobservable)}
   \item{T.m}{time of death}
   \item{tmax}{maximum time of death}
   \item{f}{a factor of containing an appropriate grouping variable}
}

\keyword{datagen}
