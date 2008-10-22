\name{poly3ci}
\alias{poly3ci}
\title{ Simultaneous confidence intervals for contrasts of poly-3-adjusted tumour rates}
\description{
Function to calculate simultaneous confidence intervals for several contrasts
 of poly-3-adjusted tumour rates in a oneway layout.
Assuming a data situation as in Peddada(2005) or Bailer and Portier (1988).
Simultaneous asymptotic CI for contrasts of tumour rates, assuming that
 standard normal approximation holds.
}
\usage{
poly3ci(time, status, f, type = "Dunnett",
 cmat = NULL, method = "BP", alternative = "two.sided",
 conf.level = 0.95, dist="MVN", k=3, \dots)
}
\arguments{
  \item{time}{ a numeric vector of times of death of the individuals}
  \item{status}{ a logical (or numeric, consisting of 0,1 only) vector giving the tumour status at time of death of each individual,
 where TRUE (1) = tumour present, FALSE (0) = no tumour present  }
  \item{f}{ a factor, giving the classification variable }
  \item{type}{ a character string, giving the name of a contrast method, as defined in \code{contrMat(multcomp)} }
  \item{cmat}{ a optional contrast matrix }
   \item{method}{ a single charcter string, specifying the method for adjustment,
with options: "BP" (Bailer Portier: assuming poly-3-adjusted rates are binomial variables),
 "BW" (Bieler, Williams: delta method as in Bieler and Williams (1993))
"ADD1" (as Bailer Portier, including an add1-adjustment on the raw tumour rates)
"ADD2" (as Bailer Portier, including an add2-adjustment on the raw tumour rates following Agresti and Caffo (2000) for binomials)
 }
  \item{alternative}{ a single character string }
  \item{conf.level}{ a single numeric value, simultaneous confidence level }
 \item{dist}{a character string, \code{"MVN"} invokes multiplicity adjustment via the multivariate normal distribution,
\code{"N"} invokes use of quantiles of the univariate normal distribution}
\item{k}{the exponent to calculate survival adjusted proportions, default is k=3}
\item{\dots}{further arguments to be passed; currently only base, to be passed to \code{contrMat} to choose the control group with \code{type="Dunnett"}}

}

\value{
A object of class "poly3ci", a list containing:
  \item{conf.int }{a matrix with 2 columns: lower and upper confidence bounds, and M rows}
  \item{alternative }{character string, as input}
  \item{conf.level}{single numeric value, as input}
  \item{quantile}{the quantile used to construct the CIs}
  \item{estimate}{ a numeric vector with the point estimates of the contrasts}
  \item{time}{ as input}
  \item{status}{as input}
  \item{f}{as input}
  \item{method}{as input}
  \item{cmat}{as input, with colnames= factor levels of f}
  \item{sample.est}{a list containing sample estimates}
}
\references{ 

The implemented methodology is described in:

Schaarschmidt, F., Sill, M., and Hothorn, L.A. (2008):
 Approximate Simultaneous confidence intervals for multiple contrasts of binomial proportions.
 Biometrical Journal 50, 782-792.


Background references are:

Assumption for poly-3-adjustment:
 Bailer, J.A. and Portier, C.J. (1988):
 Effects of treatment-induced mortality and tumor-induced mortality on tests for carcinogenicity in small samples.
 Biometrics 44, 417-431.

 Peddada, S.D., Dinse, G.E., and Haseman, J.K. (2005):
 A survival-adjusted quantal response test for comparing tumor incidence rates.
 Applied Statistics 54, 51-61.

 Bieler, G.S. and Williams, R.L. (1993):
 Ratio estimates, the Delta Method, and quantal response tests for increased carcinogenicity.
 Biometrics 49, 793-801.

 Statistical procedures and characterization of the coverage probabilities are described in:
 Sill, M. (2007):
 Approximate simultaneous confidence intervals for multiple comparisons of binomial proportions.
 Master thesis, Institute of Biostatistics, Leibniz University Hannover.

 }
\author{ Frank Schaarschmidt }
\note{ Please note that all methods here described are only approximative, and might violate the nominal level in certain situations.
Please note further that appropriateness of the point estimates, and consequently of tests and confidence intervals is based on the assumptions in Bailer and Portier (1988),
which might be a matter of controversies. }

\examples{

#############################################################

### Methyleugenol example in Schaarschmidt et al. (2008) ####

#############################################################

# load the data:

data(methyl)

# The results in Table 5 (Schaarschmidt et al. 2008) can be
# reproduced by calling:


methylW<-poly3ci(time=methyl$death, status=methyl$tumour,
 f=methyl$group, type = "Williams", method = "ADD1", alternative="greater" )

methylW


methylWT<-poly3test(time=methyl$death, status=methyl$tumour,
 f=methyl$group, type = "Williams", method = "ADD1", alternative="greater" )

methylWT


plot(methylW, main="Simultaneous CI for Poly-3-adjusted tumour rates")

# The results in Table 6 can be reproduced by calling:

methylD<-poly3ci(time=methyl$death, status=methyl$tumour,
 f=methyl$group, type = "Dunnett", method = "ADD1", alternative="greater" )

methylD

methylDT<-poly3test(time=methyl$death, status=methyl$tumour,
 f=methyl$group, type = "Dunnett", method = "ADD1", alternative="greater" )

methylDT


plot(methylD, main="Simultaneous CI for Poly-3-adjusted tumour rates")


############################################################


# unadjusted CI

methylD1<-poly3ci(time=methyl$death, status=methyl$tumour,
 f=methyl$group, type = "Dunnett", method = "ADD1", dist="N" )

methylD1

plot(methylD1, main="Local CI for Poly-3-adjusted tumour rates")

}
\keyword{ htest }
\concept{poly-k}

