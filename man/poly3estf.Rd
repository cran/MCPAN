\name{poly3estf}
\alias{poly3estf}
\title{ Only for internal use. }
\description{
 Poly-3- adjusted point and variance estimates for long term carcinogenicity data if data are given as a numeric time vector,
a logical status vector and a factor containing a grouping variable }
\usage{
poly3estf(time, status, tmax=NULL, f, method = "BP", k=NULL)
}

\arguments{
  \item{time}{  a numeric vector of times of death of the individuals}
  \item{status}{  a logical (or numeric, consisting of 0,1 only) vector giving the tumour status at time of death of each individual,
 where TRUE (1) = tumour present, FALSE (0) = no tumour present  }
  \item{tmax}{ a single numeric value, the time of sacrifice in the trial, or the last last time of death, defaults to the maximal value observed in \code{time} }
  \item{f}{  a factor of the same length as \code{time} \code{status}, giving the levels of a grouping variable in a one-way layout }
  \item{method}{ a single charcter string, specifying the method for adjustment,
with options: "BP" (Bailer Portier: assuming poly-3-adjusted rates are binomial variables),
 "BW" (Bieler, Williams: delta method as in Bieler-Williams (1993))
"ADD1" (as Bailer Portier, including an add1-adjustment on the raw tumour rates)
"ADD2" (as Bailer Portier, including an add2-adjustment on the raw tumour rates following Agresti Caffo (2000) for binomials)
 }

\item{k}{a single numeric value, the exponent to calculate survival adjusted proportions according to Bailer and Portier (1988), defaults to 3}
}
\details{
 For internal use.
}
\value{
   A list containing:
  \item{Y }{a numeric vector, groupwise number of tumours}
  \item{n}{ a numeric vector, groupwise number of individuals}
  \item{estimate}{ a numeric vector, groupwise poly-3-adjusted rates according to Bailer, Portier (1988)}
  \item{weight}{a numeric vector of poly-3-adjusted weights }
  \item{estp}{a numeric vector, groupwise poly-3-adjusted rate (according to method)}
  \item{nadj}{adjusted n (sum of weights)}
  \item{varp}{a numeric vector, groupwise variance estimate (according to method)}
  \item{varcor}{a numeric vector, groupwise variance estimate, if necessary corrected such that estimates of 0 can not occur}
  \item{names}{a character vector, the levels of the grouping variable \code{f} }
  \item{k}{a single numeric value, as input}

}
\references{ See \code{\link{poly3est}}}

\note{ See \code{\link{poly3est}}}

\examples{

data(bronch)

poly3estf(status=bronch$Y, time=bronch$time, f=bronch$group, k=3)

poly3estf(status=bronch$Y, time=bronch$time, f=bronch$group, k=5)

}
\keyword{ htest }