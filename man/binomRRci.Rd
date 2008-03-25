\name{binomRRci}
\alias{binomRRci}
\alias{binomRRci.default}
\alias{binomRRci.table}
\alias{binomRRci.matrix}
\alias{binomRRci.formula}

\title{ Simultaneous confidence intervals for ratios of independent binomial proportions }
\description{
 Simultaneous asymptotic CI for contrasts of binomial proportions,
 assuming that standard normal approximation holds on the log scale.
 Confidence intervals for ratios of (weighted geometric means) of proportions are calculated
 based on differences of log-proportions, and normal approximation on the log-scale.
}
\usage{

binomRRci(x,...)

\method{binomRRci}{default}(x, n, names=NULL,
 type="Dunnett", cmat=NULL,
 alternative="two.sided", conf.level=0.95,
 dist="MVN", ...)

\method{binomRRci}{formula}(formula, data,
 type="Dunnett", cmat=NULL,
 alternative="two.sided", conf.level=0.95,
 dist="MVN",...)

\method{binomRRci}{table}(x, type="Dunnett",
 cmat=NULL, alternative="two.sided",
 conf.level=0.95, dist="MVN",...)

\method{binomRRci}{matrix}(x, type="Dunnett",
 cmat=NULL, alternative="two.sided",
 conf.level=0.95, dist="MVN",...)

}
\arguments{
  \item{x}{ a numeric vector, giving the number of successes in I independent samples,
  or an object of class \code{"table"}, representing the 2xk-table,
  or an object of class \code{"matrix"}, representing the 2xk-table
}
  \item{n}{ a numeric vector, giving the number of trials (i.e. the sample size) in each of the I groups
 (only required if \code{x} is a numeric vector, ignored otherwise) }
  \item{names}{an optional character string, giving the names of the groups/ sample in \code{x}, \code{n};
 if not specified the possible names of x are taken as group names (ignored if \code{x} is a table or matrix)}
   \item{formula}{a two-sided formula of the style 'response ~ treatment', where 'response' should be a categorical variable with two levels,
  while treatment should be a factor specifying the treatment levels}
  \item{data}{a data.frame, containing the variables specified in formula}
  \item{type}{ a character string, giving the name of a contrast method,
 as defined in \code{contrMat(multcomp)}; ignored if \code{cmat} is sepcified }
  \item{cmat}{ a optional contrast matrix }
  \item{alternative}{ a single character string, one of "two.sided", "less", "greater" }
  \item{conf.level}{ a single numeric value, simultaneous confidence level }
  \item{dist}{a character string, \code{"MVN"} invokes multiplicity adjustment via the multivariate normal distribution,
 \code{"N"} invokes use of quantiles of the univariate normal distribution}
  \item{\dots}{ arguments to be passed to \code{\link{binomest}}, currently only \code{success} labelling the event which should be considered as success}
}
\details{
 The interval for the ratio of two independent proportions, described in section "Crude Methods using first-order variance estimation" in Gart and Nam (1988) are extended
 to multiple contrasts. Confidence intervals are constructed based on contrasts for differences of lp=log (x+0.5)/(n+0.5), using quantiles of the multivariate normal or normal approximation.
 Applying the exponential functions to the bounds results in intervals for the risk ratio.
 In case that 0 occur in both, the numerator and denominator of the ratio, the interval is expanded to [0,Inf], in case that only 0s numerator go to the numerator, the lower bound is expanded to 0,
 in case that only 0s go to the denominator, the upper bound is expanded to Inf.
 

 See the examples for different usages.
}
\value{
A object of class "binomRDci", a list containing:
  \item{conf.int}{a matrix with 2 columns: lower and upper confidence bounds, and M rows}
  \item{alternative }{character string, as input}
  \item{conf.level}{single numeric value, as input}
  \item{quantile}{the quantile used to construct the confidence intervals}
  \item{estimate}{a matrix with 1 column: containing the estimates of the contrasts}
  \item{x}{the observed number of successes in the treatment groups}
  \item{n}{the number of trials in the treatment groups}
  \item{p}{the estimated proportions in the treatment groups}
  \item{success}{a character string labelling the event considered as success}
  \item{names}{the group names}
  \item{method}{a character string, specifying the method of interval construction}
  \item{cmat}{the contrast matrix used}
}
\references{ 

Gart, JJ and Nam,J-m (1988): Approximate interval estimation of the ratio of binomial parameters: a review and corrections for skewness. Biometrics 44, 323-338.

 }

\note{ Note, that all implemented methods are approximate only. The coverage probability of the 
intervals might seriously deviate from the nominal level for small sample sizes and extreme success probabilities.
}
\seealso{ \code{\link{summary.binomRDci}} for the risk difference, \code{\link{summary.binomORci}} for the odds ratio, \code{\link{plot.sci}} for plotting}
\examples{

# In simple cases, counts of successes
# and number of trials can be just typed:

ntrials <- c(40,20,20,20)
xsuccesses <- c(1,2,2,4)
names(xsuccesses) <- LETTERS[1:4]
ex1D<-binomRRci(x=xsuccesses, n=ntrials,
 type="Dunnett")
ex1D

ex1W<-binomRRci(x=xsuccesses, n=ntrials,
 type="Williams", alternative="greater")
ex1W

# results can be plotted:
plot(ex1D, main="Comparisons to control group A")

# summary gives a more detailed print out:
summary(ex1W)

# if data are represented as dichotomous variable
# in a data.frame one can make use of table:


data(liarozole)

head(liarozole)

# here, it might be important to define which level of the
# variable 'Improved' is to be considered as success

binomRRci(Improved ~ Treatment, data=liarozole,
 type="Dunnett", success="y", base=4, alternative="greater")

# If data are available as a named kx2-contigency table:

tab<-table(liarozole)
tab

binomRRci(tab, type="Dunnett", success="y", base=4, alternative="greater")


# Performance for extreme cases:


binomRRci(x=c(0,0,20,5),n=c(20,20,20,20),names=c("A","B","C","D"),
 type="Dunnett", alternative="greater")


}
\keyword{ htest }
\concept{binomial}
\concept{proportion}
\concept{risk ratio}
\concept{simultaneous}
