\name{powerbinom}
\alias{powerbinom}
\alias{powerbinomOR}
\title{
 Approximate power for multiple contrast tests of binomial proportions.
}
\description{
  Approximative power calculation for multiple contrast tests of binomial proportions (based on risk differences, or odds ratios), based on probabilities of the multivariate standard normal distribution.
}
\usage{
powerbinom(p, n, alpha = 0.05, type = "Dunnett", cmat = NULL,
 rhs = 0, alternative = c("two.sided", "less", "greater"),
 ptype = c("global", "anypair", "allpair"), method = "Wald", crit = NULL, ...)

powerbinomOR(p, n, alpha = 0.05, type = "Dunnett", cmat = NULL, 
    rhs = 1, alternative = c("two.sided", "less", "greater"), 
    ptype = c("global", "anypair", "allpair"), 
    crit = NULL, ...) 
}

\arguments{
  \item{p}{ a numeric vector of assumed true proportions under the alternative hypothesis where \code{length(p)} is the number of (treatment) groups in the multiple comparison problem
}
  \item{n}{
 a vector (of integer values), the number of observations per (treatment) group, must have the same length as \code{p} }

  \item{alpha}{
a single numeric value, the alpha-level of the test, default is 0.05
}
  \item{type}{
a single charcater string naming the type of multiple contrast test to be applied on the proportions \kbd{p}; will be ignored if argument \code{cmat} is specified; for possible choices, see \code{\link[multcomp]{contrMat}}
}
  \item{cmat}{
optional, a matrix fo contrast coefficients (with number of columns = number of treatmenmt groups and number of rows = number of comparisons); if \code{cmat} is not specified, the type of multiple contrast test to be applied on the proportions \code{p} is chosen according to argument \code{type}
}
  \item{rhs}{
 a (vector of) numeric value(s), specifying the right hand sides of the alternative hypotheses, default is 0 for the risk difference (\kbd{powerbinom}), and 1 for the odds ratio (\kbd{powerbinomOR}), if values greater or less than the default are specified, power for shifted tests (non-inferiority, superiority) is computed; Note that the latter primarily makes sense for one-sided hypotheses. A warning is given when \code{alternative ="two.sided"} with \code{rhs!=0} or \code{rhs!=1} for difference or oddsratios, respectively.
}
  \item{alternative}{
 a character string, specifying the direction of the alternative hypotheses, options are "two.sided", "less", "greater"; \code{alternative} primarily controls for which directional decisions power is calculated, in case that argument \code{crit} is not specified, \code{alternative} also controls the choice of the critical value.
}
  \item{ptype}{ a single character string, naming the type of rejection probability to be computed; options are \code{"global"} for the overall rejection probability (irrespective which contrasts are under the null or the alternative), \code{"anypair"} for the rejection probability considering only those contrasts under the alternative, \code{"global"} for the probability that all elementary alternatives are rejected. 
}
  \item{method}{
a character string, the method for variance estimation in test / confidence interval construction: one of "Wald", "ADD1", "ADD2".
}
  \item{crit}{
a single numeric value to serve as equicoordinate critical point in the multiple test; if it is not specified, it is computed as a quantile of the multivariate normal distribution based on the specifications in arguments \code{n}, \code{cmat} (or \code{type}); note that for alternatives \code{'two.sided'} and \code{'greater'}, \code{crit} should be a single positive value, while for alternative \code{'less'}, \code{crit} should be a single negative value. 
}
  \item{\dots}{
further arguments, which are passed to the functions \code{qmvnorm} and \code{pmvnorm}, mainly to control the computation errors, see \code{GenzBretz} in package \code{mvtnorm} for details
}

}
\details{
For (standard type and user-defined) multiple contrast tests of proportions in 2xk contigency tables assuming the binomial distribution and k treatment groups, different types of rejection probabilities in the multiple testing problem can be computed.
 Based on a central multivariate normal distribution, equicoordinate critical points for the test are computed, different critical points for the test can be specified in \code{crit}.

Via computing probabilities of non-central multivariate normal distributions \code{pmvt(mvtnorm)} one can calculate:

The global rejection probability (\code{power="global"}), i.e. the probability that at least one of the elementary null hypotheses is rejected, irrespective, whether this (or any contrast!) is under the corresponding elementary alternative). As a consequence this probability involves elementary type-II-errors for those contrasts which are under their elementary null hypothesis.

The probability to reject at least one of those elementary null hypotheses which are indeed under their corresponding elementary alternatives (\code{ptype="anypair"}). Technically, this is achieved by omitting those contrasts under the elementary null and, for a given critical value, compute the rejection probability from a multivariate normal distribution with reduced dimension. Note that for \code{'two-sided'} alternatives, type III-errors (rejection of the two-sided null in favor of the wrong direction) are included in the power.

The probability to reject all elementary null hypotheses which are indeed under their corresponding elementary alternatives (\code{power="allpair"}). Also here, for 'two-sided' alternatives, the type III-error contributes to the computed 'allpair power'. Note further that two-sided allpair power is simulated based on multivariate normal random numbers.

Whether a given hypothesis is under the alternative hypothesis is currently checked via \code{abs(L-rhs) > 10*.Machine$double.eps}, where L is the constrasts true value depending on \code{p} and the contrast matrix and rhs the right hand side of the null hypothesis. For alternatives \code{"less"} or \code{"greater"}, the corresponding checks are: \code{(L-rhs) < -10*.Machine$double.eps} and \code{ (L-rhs) > 10*.Machine$double.eps}, respectively.

For the case of differences or proportion (powerbinom) the underlying methods are closely related to those described in Bretz and Hothorn (2002). The methods here differ from that of Bretz and Hothorn (2002) by using a variance estimator in the teststatistic based of unpooled sample rpoportions, not a pooled variances estimator under H0. A description is also given in Schaarschmidt, Biesheuvel and Hothorn, 2009. The method implemented for the odds ratio corresponds to the asymptotic intervals given in Holford et al. 1989, the power computation is a straightforward generalization of the methods above, assuming asymptotic normality at the scale of log odds.

}
\value{A list consisting of the following items:
\item{power}{a numeric value the computed power, with estimated computational error as an attribute}
\item{p}{the input vector of expected groupwise proportions} 
\item{n}{the input vector of group sample sizes}
\item{conexp}{a data frame containing the contrast matrix, the expected values of the contrasts given p (expContrast), the right hand sides of the hypotheses (rhs, as input), the expected values of the test statistics corresponding to the contrasts and rhs, and a column of logical values indicating whether the corresponding contrasts was under the alternative (under HA)}
\item{crit}{a single numeric value, the critical value used for power computation}
\item{alternative}{a single character string, as input}
\item{ptype}{a single character string, as input}
\item{alpha}{a single numeric value, as input}
}
\references{
\emph{Genz A, Bretz F (1999):} Numerical computation of multivariate t-probabilities with application to power calculation of multiple contrasts. Journal of Statistical Computation and Simulation, 63, 4, 361-378.

\emph{Bretz F, Hothorn LA (2002):} Detecting dose-response using contrasts: asymptotic power and sample size determination for binomial data. Statistics in Medicine, 21, 22, 3325-3335.

\emph{Holford, TR, Walter, SD and Dunnett, CW (1989):} Simultaneous interval estimates of the odds ratio in studies with two or more comparisons. Journal of Clinical Epidemiology 42, 427-434.

\emph{Schaarschmidt F, Biesheuvel E, Hothorn LA (2009):} Asymptotic simultaneous confidence intervals for many-to-one comparisons of binray proportions in randomized clinical trials. Journal of Biopharmaceutical Statistics 19, 292-310.

}

\author{Frank Schaarschmidt
}
\section{Warning }{Note that the the tests for which power is computed as well as the power computation itself relies on simple approximations of binomial distribution by normal distributions. It is known that the test procedures do not control FWER for small \code{n} and extreme proportions \code{p}. Hence, computed power may substantially deviate from the true power of the methods a) due to the fact that the tests have sizes deviating from the nominal level, b) due to insufficient approximation in the power calculation. Simulations show that absolute deviations of approximate power from simulated power are large if low values of n*p or n*(1-p) are involved and if power is low (i.e. power<0.3). }


\examples{

# Assume, one wants to perform a test for increasing trend
#  using Williams type of contrasts among I=5 groups
#  (e.g. 4 doses and one control).
#  Proportions are assumed to have values
#  pi=(0.1,0.12,0.14,0.14,0.2) under the alternative.

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(100,100,100,100,100), type = "Williams",
 alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(150,150,150,150,150), type = "Williams",
 alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(190,140,140,140,140), type = "Williams",
 alternative = "greater")


# probability to show for at least one group (2,3,4)
# a significant reduction versus control (1)

powerbinom(p=c(0.3, 0.15, 0.15, 0.15),
 n=c(140,140,140,140), type = "Dunnett",
 alternative = "less")

# probability to show for at least one group (2,3,4)
# a significant reduction versus control (1) of more 
# than 0.05 percent

powerbinom(p=c(0.3, 0.15, 0.15, 0.15),
 n=c(140,140,140,140), type = "Dunnett",
 alternative = "less", rhs=-0.05)

# probability to show for all groups (2,3,4)
# a significant reduction versus control (1) of more 
# than 0.05 percent

powerbinom(p=c(0.3, 0.15, 0.15, 0.15),
 n=c(140,140,140,140), type = "Dunnett",
 alternative = "less", rhs=-0.05, ptype="allpair")


# probability to show for at least one group (2,3,4)
# a significant reduction versus control (1)

powerbinom(p=c(0.3, 0.15, 0.15, 0.15),
 n=c(140,140,140,140), type = "Dunnett",
 alternative = "less")


powerbinomOR(p=c(0.3, 0.15, 0.15, 0.15),
 n=c(140,140,140,140), type = "Dunnett",
 alternative = "less")
}

\keyword{htest}
\concept{power}
