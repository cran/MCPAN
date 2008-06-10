\name{powermcp}
\alias{powermcp}

\title{ Approximative power calculation for multiple contrast tests }
\description{
Approximative power calculation for multiple contrast tests, based on normal approximation.

}
\usage{
powermcp(TExpH1, alpha = 0.05, corrmatH1, alternative = "two.sided")
}
\arguments{
  \item{TExpH1}{numeric vector: the expectation of the test statistics under the alternative }
  \item{alpha}{ a single numeric value: alpha-level, defaults to 0.05 }
  \item{corrmatH1}{ a numeric matrix, the correlation matrix of the teststatistics under the alternative, must have same dimensions as length of \code{TExpH1}  }
  \item{alternative}{  a single character string, specifying the alternative, one of "two.sided", "less", or "greater"  }
}
\details{  

Any-pair-power is calcualted, the power of a Union-Intersection-Test.

The probability, that any teststatistic exceeds the critical value is calculated from a central multivariate standard normal distribution.
The appropriateness of the result strongly depends on the assumptions, that teststatistics are truely following a standard normal distributions,
which might not be the case for small sample sizes.

Note that differing definitions of power are possible, but not implemented here.

}
\value{
A single numeric value, the approximative power.
}


\keyword{ htest }
\concept{ power }
\concept{ muliple contrast tests}
