\name{lnrci}
\alias{lnrci}
\alias{lndci}

\title{
Simultaneous confidence intervals for expected values assuming lognormal distribution.
}
\description{
Simultaneous confidence intervals for multiple contrasts of expected values of several (K) groups in a one-way layout, assuming lognormal distribution of the response. Multiple ratios of (weighted geometric means of) expected values can be estimated as well as multiple differences of (weighted arithmetic means of) the expected values in the K groups can be estimated. For both, ratios and differences, a method based on asymptotic normality (not recommended) and a method based on generalized pivotal quantities are available.

}
\usage{
lnrci(x, f, type = "Dunnett", cmat = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, method = c("GPQ", "AN"), ...)
lndci(x, f, type = "Dunnett", cmat = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, method = c("GPQ", "AN"), ...)
}

\arguments{
  \item{x}{
a numeric vector, the response variable, assumed tpo be lognormally dsitributed, should contain only positive values
}
  \item{f}{
a factor variable, of the same length as \code{x}, groupingt the observations in \code{x}
}
  \item{type}{
a single character string, naming a contrast type, see \link[multcomp]{contrMat} for the options; this argument is ignored if a contrast matrix is specified in \code{cmat}
}
  \item{cmat}{
a matrix with numeric entries, containing contrast coefficients; if there are K levels in \code{f}, the matrix should have k columns
}
  \item{alternative}{
a single character string, with options \code{"two.sided"} for two-sided confidence intervals, \code{"less"} for upper bounds only and \code{"greater"} for lower bounds only
}
  \item{conf.level}{
a single numeric value between 0 and 1
}
  \item{method}{
a single character string, naming the method by which to compute the confidence intervals; options are \code{"GPQ"} for a method based on generalized pivotal quantities and \code{"AN"} for the asymptotic normal method (not recommended).
}
  \item{\dots}{
 further arguments to be passed to the internal methods, in particular: Argument \code{B} specifies the number of samples to be drawn if method \code{"GPQ"} (defaults to 10000), it is ignored if \code{method="AN"}. Second, \code{dist} must be a single character string as in \code{\link{Waldci}}, invoking the use of multivariate normal quantiles \code{dist="MVN"} or standard normal quantiles \code{dist="N"} when \code{method="AN"}; it is ignored if \code{method="GPQ"}.
}
}
\details{
This is a testversion. In a setting with K treatment groups, assuming a completely randomized design and lognormal distribution of the response variable, multiple (M) ratios or differences among expected values of the K treatment groups may be of interest. The ratios or differences of interest can be specified via a character string (in argument \code{type}) or via an M x K matrix in argument \code{cmat}. Intervals for ratios can be computed (via linear contrasts on the log scale) in function \code{lnrci}, differences can be computed in fucntion \code{lndci}.
By default, the confidence intervals are adjusted for multiple comparisons. The asymptotic normal methods, rely on maximum likelihood estimates for the expected values and the corresponding variance estimates (as given in Chen and Zhou, 2006) and adjut for multiplicity via critical value from the multivariate normal distribution (correlation structure with plug-in of variance estimates).
The generalized pivotal quantity methods rely on simulating the distribution of the parameters (number of samples B=10000 by default, Krishnamoorthy and Mathew, 2003; Chen and Zhou, 2006) and compute simultaneous intervals from this sample using the function \code{SCSrank}.

}
\value{
A list with elements

\item{estimate}{a column vector, containing the point estimates of the contrasts}
\item{conf.int}{a Mx2 matrix of confidence bounds, if M comparisons among the K samples are invoked}
\item{alternative}{a character string, as input}
\item{conf.level}{a numeric value, as input}
}
\references{
\emph{Chen, Y-H, Zhou, X-H (2006).} Interval estimates for the ratio and the difference of two log-normal means.
Statistics in Medicine 25, 4099-4113.

\emph{Krishnamoorthy K, Mathew T (2003).} Inferences on the means of lognormal distributions using generalized p-values and generalized confidence intervals. Journal of Statistical Planning and Inference 115,
103-121.
}

\author{Frank Schaarschmidt}

\examples{

x <- rlnorm(n=100, meanlog=rep(c(0,0.1,1,1), each=25), sdlog=0.5)
f <- as.factor(rep(LETTERS[1:4], each=25))
boxplot(x~f)

lndci(x=x, f=f, type="Dunnett", method="GPQ", B=40000)
lndci(x=x, f=f, type="Dunnett", method="AN")

lnrci(x=x, f=f, type="Tukey", method="GPQ", B=40000)
lnrci(x=x, f=f, type="Tukey", method="AN")
}

\keyword{ htest }
