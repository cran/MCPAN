\name{powermcpn}
\alias{powermcpn}

\title{
Approximative power calculation for multiple contrast tests 
}
\description{
Approximative power calculation for multiple contrast tests which are based on normal approximation.
}
\usage{
powermcpn(ExpTeststat, corrH1, crit, alternative = c("two.sided", "less", "greater"),
 alpha = 0.05, ptype = c("global", "anypair", "allpair"), ...)
}

\arguments{
  \item{ExpTeststat}{
numeric vector: the expectation of the test statistics under the alternative
}
  \item{corrH1}{
a numeric matrix, the correlation matrix of the teststatistics under the alternative, must have same number of columns and rows as length of \code{ExpTeststat} 
}
  \item{crit}{
a single numeric value, the critical value of the test; if not specified a critical value is computed as an equicoordinate (1-\code{alpha}) quantile of a central multivariate normal distribution with correlation \code{corrH1}
}
  \item{alternative}{
 a character string, specifying the direction of the alternative hypotheses, options are "two.sided", "less", "greater"; \code{alternative} primarily controls for which directional decisions power is calculated, in case that argument \code{crit} is not specified, \code{alternative} also controls the choice of the critical value.
}
  \item{alpha}{
 a single numeric value: the FWER for the multiple test, defaults to 0.05
}
  \item{ptype}{ a single character string, naming the type of rejection probability to be computed; options are \code{"global"} for the overall rejection probability (irrespective which contrasts are under the null or the alternative), \code{"anypair"} for the rejection probability considering only those contrasts under the alternative, \code{"global"} for the probability that all elementary alternatives are rejected. 
}
  \item{\dots}{
further arguments, which are passed to the functions \code{qmvnorm} and \code{pmvnorm}, mainly to control the computation errors, see help \code{GenzBretz(mvtnorm)} for details
}

}
\details{
For (standard type and user-defined) multiple contrast tests based on approximation with the multivariate normal distribution, three types of rejection probabilities in the multiple testing problem can be computed.

The global rejection probability (\code{power="global"}), i.e. the probability that at least one of the elementary null hypotheses is rejected, irrespective, whether this (or any contrast!) is under the corresponding elementary alternative). As a consequence this probability involves elementary type-II-errors for those contrasts which are under their elementary null hypothesis.

The probability to reject at least one of those elementary null hypotheses which are indeed under their corresponding elementary alternatives (\code{power="anypair"}). Technically, this is achieved by omitting those contrasts under the elementary null and compute the rejection probability for a given criticla value from a multivariate normal distribution with reduced dimension. Note that for \code{'two-sided'} alternatives, type III-errors (rejection of the two-sided null in favor of the wrong direction) are included in the power.

The probability to reject all elementary null hypotheses which are indeed under their corresponding elementary alternatives (\code{power="allpair"}). Also here, for 'two-sided' alternatives type III-error contribute to the computed 'allpair power'. Note further that two-sided allpair power is simulated based on multivariate normal random numbers.

}
\value{
A list consisting of the following items:
\item{power}{a numeric value the computed power, with estimated computational error as an attribute}
\item{conexp}{ the expected values of the test statistics corresponding to the contrasts and rhs, and a column with [0,1] values, indicating whether the corresponding contrasts was under the alternative (1) or under the null (0)}
\item{crit}{a single numeric value, the critical value used for power computation}
\item{alternative}{a single character string, as input}
\item{ptype}{a single character string, as input}
\item{alpha}{a single numeric value, as input}
}

\author{
Frank Schaarschmidt
}



\keyword{htest}
\keyword{power}
