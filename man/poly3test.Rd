\name{poly3test}
\alias{poly3test}

\title{ Approximate simultaneous test for poly-3-adjusted tumour rates }
\description{
P-value of maximum test and adjusted p-values for M contrasts of I groups in a one-way layout.
Based on approximation of the true distribution of the M test statistics by an M-variate normal distribution. 
}
\usage{
poly3test(time, status, f, type = "Dunnett",
 cmat = NULL, method = "BP", alternative = "two.sided",
dist="MVN", k=3, \dots)
}
\arguments{
  \item{time}{  a numeric vector of times of death of the individuals}
  \item{status}{  a logical (or numeric, consisting of 0,1 only) vector giving the tumour status at time of death of each individual,
 where TRUE (1) = tumour present, FALSE (0) = no tumour present  }
  \item{f}{  a factor of the same length as \kbd{time}, \kbd{status}, giving the levels of a grouping variable in a one-way layout }
  \item{type}{ a character string specifying the contrast type } 
  \item{cmat}{an optional user defined contrast matrix of dimension MxI}
  \item{method}{ a single charcter string, specifying the method for adjustment,
with options: "BP" (Bailer Portier: assuming poly-3-adjusted rates are binomial variables),
 "BW" (Bieler, Williams: delta method as in Bieler-Williams (1993))
"ADD1" (as Bailer Portier, including an add1-adjustment on the raw tumour rates)
"ADD2" (as Bailer Portier, including an add2-adjustment on the raw tumour rates following Agresti Caffo (2000) for binomials)
 }
  \item{alternative}{ a character string specifying the direction of the alternative hypothesis}
 \item{dist}{a character string, where \kbd{"MVN"} invokes the computation of p-values using the multivariate normal distribution,
 and \kbd{"N"} invokes use p-value computation using the univariate normal distribution}

\item{k}{a single numeric value, the exponent to calculate survival adjusted proportions according to Bailer and Portier (1988), defaults to 3}
\item{\dots}{further arguments to be passed; currently only \kbd{base}, to choose the control group with \kbd{type="Dunnett"}}

}
\details{
 Testversion.
}
\value{ An object of class "poly3test", a list containing:

\item{teststat}{a numeric vector of teststatistics of length M}
\item{pval}{a single numeric p-value, the p-value of the maximum test (minimum p-value)} 
\item{p.val.adj}{ a vector of length M, the adjusted p-values of the single contrasts}
\item{alternative}{a single character vector, as the input}
\item{dist}{a character string specifying which distribution}
\item{time}{ as input}
\item{status}{ as input}
\item{f}{ as input}
\item{method}{as input}
\item{cmat}{ used contrast matrix}
\item{sample.est}{a list containing sample estimates}
}

\references{
 Assumptions corresponding to the poly-k-adjustment:

 Bailer, J.A. and Portier, C.J. (1988):
 Effects of treatment-induced mortality and tumor-induced mortality on tests for carcinogenicity in small samples.
 Biometrics 44, 417-431.

 Peddada, S.D., Dinse, G.E., and Haseman, J.K. (2005):
 A survival-adjusted quantal response test for comparing tumor incidence rates.
 Applied Statistics 54, 51-61.

 Statistical procedures and characterization of coverage probabilities are described in:
 Sill, M. (2007):
 Approximate simultaneous confidence intervals for multiple comparisons of binomial proportions.
 Master thesis, Institute of Biostatistics, Leibniz University Hannover.


 }

\note{ Please note that all methods here described are only approximative, and might violate the nominal level in certain situations.
Please note further that appropriateness of the point estimates, and consequently of tests and confidence intervals is based on the assumptions in Bailer and Portier (1988),
which might be a matter of controversies.  }

\examples{

# poly-3-adjusted tumour rates with a potential
# down-turn effect for the highest dose group "4":

data(methyl)

# many-to-one:
methylD<-poly3test(time=methyl$death, status=methyl$tumour,
 f=methyl$group, type = "Dunnett", method = "ADD1")

methylD

# Williams-Contrast:
methylW<-poly3test(time=methyl$death, status=methyl$tumour,
 f=methyl$group, type = "Williams", method = "ADD1", alternative="greater" )

methylW

# Changepoint-Contrast:
methylCh<-poly3test(time=methyl$death, status=methyl$tumour,
 f=methyl$group, type = "Change", method = "ADD1", alternative="greater" )

methylCh

}
\keyword{ htest }
\concept{poly-k}
