\name{binomRDtest}
\alias{binomRDtest}
\alias{binomRDtest.default}
\alias{binomRDtest.table}
\alias{binomRDtest.matrix}
\alias{binomRDtest.formula}

\title{ Simultaneous test for contrasts of independent binomial proportions (in a oneway layout) }
\description{
P-value of maximum test and adjusted p-values for M contrasts of I groups
 in a one-way layout. Tests are performed for contrasts of proportions,
 which can be interpreted as differences of (weighted averages of) proportions.
}
\usage{
binomRDtest(x, ...)

\method{binomRDtest}{default}(x, n, names=NULL,
 type="Dunnett", cmat=NULL, method="Wald",
 alternative="two.sided", dist="MVN", ...)

\method{binomRDtest}{formula}(formula, data,
 type="Dunnett", cmat=NULL, method="Wald",
 alternative="two.sided", dist="MVN", ...)

\method{binomRDtest}{table}(x, type="Dunnett",
 cmat=NULL, method="Wald", alternative="two.sided",
 dist="MVN", ...)

\method{binomRDtest}{matrix}(x, type="Dunnett",
 cmat=NULL, method="Wald", alternative="two.sided",
 dist="MVN", ...)
}
\arguments{
  \item{x}{ a numeric vector, giving the number of successes in I independent samples,
  or an object of class \kbd{"table"}, representing the 2xk-table,
  or an object of class \kbd{"matrix"}, representing the 2xk-table
 }
  \item{n}{ a numerioc vector, giving the number of trials (i.e. the sample size) in each of the I groups }
  \item{names}{an optional character vector, giving the names of the groups in \kbd{x}, \kbd{n}; if not specified, possibly availbale names of x are taken as group names}
  \item{formula}{a two-sided formula of the style 'response ~ treatment', where 'response' should be a categorical variable with two levels,
  while treatment should be a factor specifying the treatment levels}
  \item{data}{a data.frame, containing the variables specified in formula}
  \item{type}{ a character string specifying the contrast type } 
  \item{cmat}{an optional user defined contrast matrix of dimension MxI}
  \item{method}{ a single charcter string, specifying the method for adjustment,
with options: "Wald" (Maximum likelihood estimators),
"ADD1" (add1-adjustment on the raw proportion estimates)
"ADD2" (add2-adjustment on proportion estimates following Agresti Caffo (2000))
 }
  \item{alternative}{ a character string specifying the direction of the alternative hypothesis}
 \item{dist}{a character string, where \kbd{"MVN"} invokes the computation of p-values using the multivariate normal distribution,
 and \kbd{"N"} invokes use p-value computation using the univariate normal distribution}
\item{\dots}{ arguments to be passed to \link{binomest}, currently only \kbd{success} labelling the event which should be considered as success}

}
\details{ 
For usage, see the examples.
}
\value{ An object of class "binomRDtest", a list containing:
\item{teststat}{a numeric vector of teststatistics of length M}
\item{pval}{a single numeric p-value, the p-value of the maximum test (minimum p-value)} 
\item{p.val.adj}{ a vector of length M, the adjusted p-values of the single contrasts}
\item{dist}{character string indicating whether the multivariate normal or normal distribution was used for computation of p-values}
\item{alternative}{a single character vector, as the input}
\item{x}{the observed number of successes in the treatment groups}
\item{n}{the number of trials in the treatment groups}
\item{p}{the estimated proportions in the treatment groups}
\item{success}{a character string labelling the event considered as success}
\item{method}{as input, a character string}
\item{cmat}{ used contrast matrix}
}

\references{
 Statistical procedures and characterization of coverage probabilities are described in:
 Sill, M. (2007):
 Approximate simultaneous confidence intervals for multiple comparisons of binomial proportions.
 Master thesis, Institute of Biostatistics, Leibniz University Hannover.
 }

\note{
Note, that all implemented methods are approximate only. The size of the 
test might seriously deviate from the nominal level for small sample sizes and extreme success probabilities.
See the simulation results in Sill (2007) for details.  }
\seealso{ \link{summary.binomRDtest} }
\examples{

ntrials <- c(40,20,20,20)
xsuccesses <- c(1,2,2,4)
names(xsuccesses) <- LETTERS[1:4]
binomRDtest(x=xsuccesses, n=ntrials, method="ADD1",
 type="Dunnett")

binomRDtest(x=xsuccesses, n=ntrials, method="ADD1",
 type="Williams", alternative="greater")

binomRDtest(x=xsuccesses, n=ntrials, method="ADD2",
 type="Williams", alternative="greater")
}
\keyword{ htest }
\concept{binomial}
\concept{proportion}
\concept{risk difference}
