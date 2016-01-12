\name{powermcpt}
\alias{powermcpt}

\title{
Testversion. Power calculation for multiple contrast tests (1-way ANOVA model)
}

\description{
Testversion. Calculate the power of a multiple contrast tests of k means in a model with homogeneous Gaussian errors, using the function pmvt(mvtnorm) to calculate multivariate t probabilities. Different options of power defineition are
"global": the overall rejection probability (the probability that at the elementary null is rejected for at least one contrast, irrespective of being under the elementary null or alternative), "anypair": the probability to reject any of the elementary null hypotheses for those contrasts that are under the elmentary alternatives, "allpair": and the probability that all elementary nulls are rejected which are indeed under the elementary nulls. See Sections 'Details' and 'Warnings'!
}

\usage{
powermcpt(mu, n, sd, cmat = NULL, rhs=0, type = "Dunnett",
 alternative = c("two.sided", "less", "greater"), alpha = 0.05,
 ptype = c("global", "anypair", "allpair"), crit = NULL, ...)
}

\arguments{
  \item{mu}{a numeric vector of expected values in the k treatment groups}
  \item{n}{a numeric vector of sample sizes in the k treatment groups}
  \item{sd}{a single numeric value, specifying the expected standard deviation of the residual error}
  \item{cmat}{optional specification of a contrast matrix; if specified, it should have as many columns as there are groups in arguments \code{mu} and \code{n} and it should have at least 2 rows, if specified, argument \code{type} is ignored, if not specified, the contrast is determined by argument \code{type}}
  \item{rhs}{numeric vector, specifying the right hand side of the hyptheses to test, defaults to 0, other specifications lead to tests of non-inferiority and superiority.}
  \item{type}{ a single character string, naming one of the contrast types available in \code{contrMat(multcomp)}; argument is ignored if \code{cmat} is specified}
  \item{alternative}{ a single character string, specifying the direction of the alternative hypothesis, one of \code{"two.sided","less","greater"}. Note that this argument governs how the multivariate t probabilities are evaluated as well as the computation of the critical value if none is specified (i.e. default \code{crit=NULL})}
  \item{alpha}{ a single numeric value, familywise type I error to be controlled, is ignored if argument \code{crit} is specified}
  \item{ptype}{ a single character string, naming the type of rejection probability to be computed; options are \code{"global"} for the global rejection probability,  \code{"anypair"} for the rejection probability considering only those contrasts under the alternative, \code{"global"} for the probability that all elementary alternatives are rejected. }
  \item{crit}{ a single numeric value to serve as equicoordinate critical point in the multiple test; if it is not specified, it is computed as a quantile of the multivariate t distribution based on the specifications in arguments \code{n}, \code{cmat} (or \code{type}); note that for alternatives \code{'two.sided'} and \code{'greater'}, \code{crit} should be a single positive value, while for alternative \code{'less'}, \code{crit} should be a single negative value. }
  \item{\dots}{ further arguments, which are passed to the functions \code{qmvt} and \code{pmvt}, mainly to control the computation errors, see help \code{GenzBretz(mvtnorm)} for details}
}
\details{
In a homoscedastic Gaussian model with k possibly different means compared by (user-defined) multiple contrast tests, different types of rejection probabilities in the multiple testing problem can be computed.
 Based on a central multivariate t distribution with df=sum(n)-k appropriate equicoordinate critical points for the test are computed, different critical points can be specified in \code{crit}
 Computing probabilities of non-central multivariate t distributions \code{pmvt(mvtnorm)} one can calculate:

The global rejection probability (\code{power="global"}), i.e. the probability that at least one of the elementary null hypotheses is rejected, irrespective, whether this (or any contrast!) is under the corresponding elementary alternative). As a consequence this probability involves elementary type-II-errors for those contrasts which are under their elementary null hypothesis.

The probability to reject at least one of those elementary null hypotheses which are indeed under their corresponding elementary alternatives (\code{power="anypair"}). Technically, this is achieved by omitting those contrasts under the elementary null and compute the rejection probability for a given criticla value from a multivariate t distribution with reduced dimension. Note that for \code{'two-sided'} alternatives, type III-errors (rejection of the two-sided null in favor of the wrong direction) are included in the power.

The probability to reject all elementary null hypotheses which are indeed under their corresponding elementary alternatives (\code{power="allpair"}). Also here, for 'two-sided' alternatives type III-error contribute to the computed 'allpair power'. Note further that two-sided allpair power is simulated based on multivariate t random numbers.


}
\value{
A list consisting of the following items:
\item{power}{a numeric value the computed power, with estimated computational error as an attribute}
\item{mu}{the input vector of expected values of group means} 
\item{n}{the input vector of group sample sizes}
\item{conexp}{a data frame containing the contrast matrix, the expected values of the contrasts given mu (expContrast), the right hand sides of the hypotheses (rhs, as input), the expected values of the test statistics corresponding to the contrasts and rhs, and a column of logical values indicating whether the corresponding contrasts was under the alternative (under HA)}
\item{crit}{a single numeric value, the critical value used for power computation}
\item{alternative}{a single character string, as input}
\item{ptype}{a single character string, as input}
\item{alpha}{a single numeric value, as input}
}
\references{
\emph{Genz A, Bretz F (1999):} Numerical computation of multivariate t-probabilities with application to power calculation of multiple contrasts. Journal of Statistical Computation and Simulation, 63, 4, 361-378.
\emph{Bretz F, Hothorn LA (2002):} Detecting dose-response using contrasts: asymptotic power and sample size determination for binomial data. Statistics in Medicine, 21, 22, 3325-3335.
\emph{Bretz F, Hayter AJ and Genz A (2001):} Critical point and power calculations for the studentized range test for generally correlated means. Journal of Statistical Computation and Simulation, 71, 2, 85-97.
\emph{Dilba G, Bretz F, Hothorn LA, Guiard V (2006):} Power and sample size computations in simultaneous tests for non-inferiority based on relative margins. Statistics in Medicien 25, 1131-1147.
}
\author{
Frank Schaarschmidt
}

\section{Warning}{This is a test version, which has roughly (but not for an extensive number of settings) been checked by simulation. Any reports of errors/odd behaviour/amendments are welcome.}


\examples{
powermcpt(mu=c(3,3,5,7), n=c(10,10,10,10), sd=2, type = "Dunnett",
 alternative ="greater", ptype = "global")
powermcpt(mu=c(3,3,5,7), n=c(10,10,10,10), sd=2, type = "Williams",
 alternative ="greater", ptype = "global")

powermcpt(mu=c(3,3,5,7), n=c(10,10,10,10), sd=2, type = "Dunnett",
 alternative ="greater", ptype = "anypair")
powermcpt(mu=c(3,3,5,7), n=c(10,10,10,10), sd=2, type = "Williams",
 alternative ="greater", ptype = "anypair")

powermcpt(mu=c(3,4,5,7), n=c(10,10,10,10), sd=2, type = "Dunnett",
 alternative ="greater", ptype = "allpair")
powermcpt(mu=c(3,2,1,-1), n=c(10,10,10,10), sd=2, type = "Dunnett",
 alternative ="greater", ptype = "allpair")
}
\keyword{htest}
\concept{power}