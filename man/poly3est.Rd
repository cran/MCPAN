\name{poly3est}
\alias{poly3est}

\title{ Only for internal use. }
\description{
 Poly-3- adjusted point and variance estimates for long term carcinogenicity data 
 }
\usage{
poly3est(time, status, tmax, method = "BP", k=NULL)
}
\arguments{
  \item{time}{  a numeric vector of times of death of the individuals}
  \item{status}{  a logical (or numeric, consisting of 0,1 only) vector giving the tumour status at time of death of each individual,
 where TRUE (1) = tumour present, FALSE (0) = no tumour present  }

  \item{tmax}{ a single numeric vector, the final time of the trial }
  \item{method}{ a single charcter string, specifying the method for adjustment,
with options: "BP" (Bailer Portier: assuming poly-3-adjusted rates are binomial variables),
 "BW" (Bieler, Williams: delta method as in Bieler-Williams (1993))
"ADD1" (as Bailer Portier, including an add1-adjustment on the raw tumour rates)
"ADD2" (as Bailer Portier, including an add2-adjustment on the raw tumour rates following Agresti Caffo (2000) for binomials)
 }
\item{k}{a single numeric value, the exponent to calculate survival adjusted proportions according to Bailer and Portier (1988), defaults to 3}
}
\details{
 Only for internal use of \link{poly3test} and \link{poly3ci}.
}
\value{
 A list containing:
  \item{Y }{number of tumours}
  \item{n}{number of individuals}
  \item{estimate}{poly-3-adjusted rates according to Bailer, Portier (1988)}
  \item{weight}{a vector of poly-3-adjusted weights, of length n }
  \item{estp}{poly-3-adjusted rate (according to method)}
  \item{nadj}{adjusted n (sum of weights)}
  \item{varp}{ variance estimate (according to method)}
  \item{varcor}{ variance estimate, if necessary corrected such that estimates of 0 can not occur}
}
\references{
 Bailer, J.A. and Portier, C.J. (1988):
 Effects of treatment-induced mortality and tumor-induced mortality on tests for carcinogenicity in small samples.
 Biometrics 44, 417-431.}

\note{ Please note, that appropriateness of the point estimates seriously depends on whether the assumptions in Bailer and Portier are met or not. }

\keyword{misc}
