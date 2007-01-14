\name{binomORci}
\alias{binomORci}
\alias{binomORci.default}
\alias{binomORci.formula}
\alias{binomORci.table}
\alias{binomORci.matrix}


\title{ Simultaneous confidence intervals for odds ratios}
\description{
  Approximate simultaneous confidence intervals for (weighted geometric means of) odds ratios
are constructed. Estimates are derived from fitting a glm on the logit-link,
 approximate intervals are constructed on the log-link,
 and transformed to origninal scale.}

\usage{
binomORci(x, ...)

\method{binomORci}{default}(x, n, names = NULL,
 type = "Dunnett", cmat = NULL, alternative = "two.sided",
 conf.level = 0.95, dist="MVN", ...)

\method{binomORci}{formula}(formula, data,
 type = "Dunnett", cmat = NULL, alternative = "two.sided",
 conf.level = 0.95, dist="MVN", ...)

\method{binomORci}{table}(x,
 type = "Dunnett", cmat = NULL, alternative = "two.sided",
 conf.level = 0.95, dist="MVN", ...)

\method{binomORci}{matrix}(x,
 type = "Dunnett", cmat = NULL, alternative = "two.sided",
 conf.level = 0.95, dist="MVN", ...)
}


\arguments{
  \item{x}{ a numeric vector, giving the number of successes in I independent samples,
  or an object of class \code{"table"}, representing the 2xk-table,
  or an object of class \code{"matrix"}, representing the 2xk-table}

  \item{n}{  numeric vector, giving the number of trials (i.e. the sample size) in each of the I groups
 (only required if \code{x} is a numeric vector, ignored otherwise)  }

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
This function calls \code{\link{glm}} and fits a one-way-model with family binomial on the logit-link.
Then, the point estimates and variances estimates from the fit are taken to construct simultaneous confidence
intervals for differences (of weighted arithmetic means) of log-odds.
Applying the exponential function to these intervals on the logit scale yields
intervals for ratios (of weighted geometric) of odds. For simple groupwise comparisons,
one yields intervals for oddsratios.
For the case of Dunnett-type contrasts, the calculated simultaneous confidence intervals
are those described in Holford et al. (1989), replacing the estimates in the adjusted
 Woolf-interval by those from the glmfit.
For the case of general contrasts, the methods are not described explicitly so far.

}
\value{
A object of class "binomORci", a list containing:
  \item{conf.int}{a matrix with 2 columns: lower and upper confidence bounds, and M rows}
  \item{alternative }{character string, as input}
  \item{conf.level}{single numeric value, as input}
  \item{estimate}{a matrix with 1 column: containing the estimates of the contrasts}
  \item{x}{the observed number of successes}
  \item{n}{the number of trials}
  \item{p}{the estimated proportions}
  \item{success}{a character string labelling the event considered as success}
  \item{names}{the group names}
  \item{method}{a character string, specifying the method of interval construction}
  \item{cmat}{the contrast matrix used}
}
\references{
Holford, TR, Walter, SD and Dunnett, CW (1989). Simultaneous interval estimates of the odds ratio in
studies with two or more comparisons. Journal of Clinical Epidemiology 42, 427-434.

}

\author{ Daniel Gerhard, Frank Schaarschmidt }

\note{ 
}
\seealso{ Intervals for the risk difference \code{\link{binomRDci}},
 summary for odds ratio confidence intervals \code{\link{summary.binomORci}}
 plot for confidence intervals \code{\link{plot.sci}}
 }
\examples{
data(liarozole)

table(liarozole)

ORlia<-binomORci(Improved ~ Treatment,
 data=liarozole, success="y")
ORlia
summary(ORlia)
plot(ORlia)

# if data are available as table:

tab<-table(liarozole)
ORlia2<-binomORci(tab, success="y")
ORlia2



}
\keyword{ htest }
\concept{ confidence interval }
\concept{ odds ratio }
