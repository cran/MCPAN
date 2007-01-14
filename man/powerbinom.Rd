\name{powerbinom}
\alias{powerbinom}
\title{ Approximate power for multiple contrast tests of binomial proportions }
\description{
  Approximative power calculation for multiple contrast tests of binomial proportions, based on probabilities of the multivariate standard normal distribution.
}
\usage{
powerbinom(p, n, alpha = 0.05, type = "Williams", cmat = NULL, method = "Wald", alternative = "less")
}
\arguments{
  \item{p}{ a numeric vector, the proportions assumed under the alternative }
  \item{n}{ an integer vector, the number of observations, must be of same length as \code{p} }
  \item{alpha}{ a single numeric value, the alpha-level of the test  }
  \item{type}{ if \code{cmat} is not specified, the type of multiple contrast test to be applied on the proportions \code{p}; will be ignored if \code{cmat} is specified}
  \item{cmat}{ a numeric contrast matrix, number of columns must be the same as length of \code{n}, and \code{p}; if secified, \code{type} will be ignored }
  \item{method}{ a character string, the method for variance estimation in test / confidence interval construction: one of "Wald", "ADD1", "ADD2"; see \code{\link{binomRDtest}} }
  \item{alternative}{ a character string, specifying the alternative, options are "two.sided", "less", "greater" }
}
\details{
 Calls \code{\link{powermcp}}. 

Power for an Union-Intersection-Test is calculated, i.e. the power that H0 is rejected for at least one of the contrasts specified by \code{cmat} or \code{type}.

For the Wald-method, see the methods in Bretz and Hothorn (2002). The implementation in this function
differs slightly from the methods described in Bretz and Hothorn. Here, we use an unpooled variance estimator,
while Bretz and Hothorn us a pooled variance estimator.

Note, that currently only power calculation for Union-Intersection-Tests of 
type H0: ALL Lm<=0 vs.  HA: ANY Lm>0 (alternative="greater"), H0: ALL Lm>=0 vs.  HA: ANY Lm<0 (alternative="less"), or H0: ALL Lm=0 vs.  HA: ANY Lm<or>0 (alternative="two.sided") are implemented.
Power for tests against delta>0 or delta<0 is not implemented.

Calcualtion is based on probabilities of the multivariate standard normal distribution, thus will be only appropriate if normal approximation for the test statistics holds.
Therefore, results might be misleading for small sample sizes, such as n < 40, and a warning is posted. In this case, simulation is recommended.  

}
\value{
A single numeric value, the power.
}
\references{ 

 Bretz F, Hothorn L.:
 Detecting dose-response using contrasts: asymptotic power and sample size determination for binomial data.
 Statistics in Medicine 2002; 21: 3325-3335.

 }

\seealso{ \code{\link{powermcp}}}

\examples{

# Assume, one wants to perform a test for increasing trend
#  using Williams type of contrasts among I=5 groups
#  (e.g. 4 doses and one control).
#  Proportions are assumed to have values
#  pi=(0.1,0.12,0.14,0.14,0.2) under the alternative.



powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(20,20,20,20,20), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(30,30,30,30,30), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(60,60,60,60,60), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(80,80,80,80,80), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(100,100,100,100,100), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(150,150,150,150,150), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(190,140,140,140,140), type = "Williams",
  method = "ADD1", alternative = "greater")

}
\keyword{ htest }
\concept{ power }
\concept{ binomial }
\concept{ multiple contrast test}