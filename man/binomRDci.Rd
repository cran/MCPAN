\name{binomRDci}
\alias{binomRDci}
\alias{binomRDci.default}
\alias{binomRDci.table}
\alias{binomRDci.matrix}
\alias{binomRDci.formula}

\title{ Simultaneous confidence intervals for contrasts of independent binomial proportions (in a oneway layout) }
\description{
 Simultaneous asymptotic CI for contrasts of binomial proportions,
 assuming that standard normal approximation holds.
 The contrasts can be interpreted as differences of (weighted averages)
 of proportions (risk ratios).
}
\usage{

binomRDci(x,...)

\method{binomRDci}{default}(x, n, names=NULL,
 type="Dunnett", cmat=NULL, method="Wald",
 alternative="two.sided", conf.level=0.95,
 dist="MVN", ...)

\method{binomRDci}{formula}(formula, data,
 type="Dunnett", cmat=NULL, method="Wald",
 alternative="two.sided", conf.level=0.95,
 dist="MVN",...)

\method{binomRDci}{table}(x, type="Dunnett",
 cmat=NULL, method="Wald", alternative="two.sided",
 conf.level=0.95, dist="MVN",...)

\method{binomRDci}{matrix}(x, type="Dunnett",
 cmat=NULL, method="Wald", alternative="two.sided",
 conf.level=0.95, dist="MVN",...)

}
\arguments{
  \item{x}{ a numeric vector, giving the number of successes in I independent samples,
  or an object of class \kbd{"table"}, representing the 2xk-table,
  or an object of class \kbd{"matrix"}, representing the 2xk-table
}
  \item{n}{ a numeric vector, giving the number of trials (i.e. the sample size) in each of the I groups
 (only required if \kbd{x} is a numeric vector, ignored otherwise) }
  \item{names}{an optional character string, giving the names of the groups/ sample in \kbd{x}, \kbd{n};
 if not specified the possible names of x are taken as group names (ignored if \kbd{x} is a table or matrix)}
   \item{formula}{a two-sided formula of the style 'response ~ treatment', where 'response' should be a categorical variable with two levels,
  while treatment should be a factor specifying the treatment levels}
  \item{data}{a data.frame, containing the variables specified in formula}
  \item{type}{ a character string, giving the name of a contrast method,
 as defined in \kbd{contrMat(multcomp)}; ignored if \kbd{cmat} is specified }
  \item{cmat}{ a optional contrast matrix }
  \item{method}{ a single character string, specifying the method for confidence interval construction; options are: \kbd{"Wald"}, \kbd{"ADD1"}, or \kbd{"ADD2"} }
  \item{alternative}{ a single character string, one of "two.sided", "less", "greater" }
  \item{conf.level}{ a single numeric value, simultaneous confidence level }
  \item{dist}{a character string, \kbd{"MVN"} invokes multiplicity adjustment via the multivariate normal distribution,
 \kbd{"N"} invokes use of quantiles of the univariate normal distribution}
  \item{\dots}{ arguments to be passed to \link{binomest}, currently only \kbd{success} labelling the event which should be considered as success}
}
\details{
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

 Schaarschmidt, F., Sill, M. and Hothorn, L.A. (2008):
 Approximate simultaneous confidence intervals for multiple contrasts of binomial proportions.
 Biometrical Journal 50, 782-792.

Background references:
 
 The ideas underlying the "ADD1" and "ADD2" adjustment are described in:
 
Agresti, A. and Caffo, B.(2000):
 Simple and effective confidence intervals for proportions and differences of proportions result from adding two successes and two failures.
 American Statistician 54, p. 280-288.

And have been generalized for a single contrast of several proportions in:

Price, R.M. and Bonett, D.G. (2004):
 An improved confidence interval for a linear function of binomial proportions.
 Computational Statistics and Data Analysis 45, 449-456.

More detailed simulation results are available in:

 Sill, M. (2007):
 Approximate simultaneous confidence intervals for multiple comparisons of binomial proportions.
 Master thesis, Institute of Biostatistics, Leibniz University Hannover.

 }

\note{ Note, that all implemented methods are approximate only. The coverage probability of the 
intervals might seriously deviate from the nominal level for small sample sizes and extreme success probabilities.
See the simulation results in Sill (2007) for details. }
\seealso{ \link{summary.binomRDci}, \link{plot.sci} }
\examples{

###############################################################

### Example 1 Tables 1,7,8 in Schaarschmidt et al. (2008):  ###

###############################################################

# Number of patients under observation: 
n <- c(29, 24, 25, 24, 46)

# Number of patients with complete response:
cr <- c(7, 11, 10, 12, 21)

# (Optional) names for the treatments
dn <- c("0.3_1.0", "3", "10", "30", "90")

# Assume we aim to infer an increasing trend with increasing dosage,
# Using the changepoint contrasts (Table 7, Schaarschmidt et al., 2008)

# The results in Table 8 can be reproduced by calling:

binomRDci(n=n, x=cr, names=dn, alternative="greater",
 method="ADD2", type="Changepoint")

binomRDci(n=n, x=cr, names=dn, alternative="greater",
 method="ADD1", type="Changepoint")

binomRDci(n=n, x=cr, names=dn, alternative="greater", 
 method="Wald", type="Changepoint")

##############################################################

### Example 2, Tables 2,9,10 in Schaarschmidt et al. 2008  ###

##############################################################

# Data (Table 2)

# animals under risk
n<-c(30,30,30,30)

# animals showing cancer
cancer<-c(20,14,27,19)

# short names for the treatments
trtn<-c("HFaFi","LFaFi","HFaNFi","LFaNFi")


# User-defined contrast matrix (Table 9),
# columns of the contrast matrix 

cmat<-rbind(
"Fiber - No Fiber"=c( 0.5, 0.5,-0.5,-0.5),
"Low Fat - High Fat"=c(-0.5, 0.5,-0.5, 0.5),
"Interaction Fat:Fiber"=c(   1,  -1,   -1,  1))

cmat

# The results in Table 10 can be reproduced by calling:

# simultaneous CI using the add-2 adjustment

sci<-binomRDci(x=cancer, n=n, names=trtn, method="ADD2",
 cmat=cmat, dist="MVN")

sci

# marginal CI using the basic Wald formula

ci<-binomRDci(x=cancer, n=n, names=trtn, method="Wald",
 cmat=cmat, dist="N")

ci


# check, whether the intended contrasts have been defined:

summary(sci)

# plot the result:

plot(sci, lines=0, lineslty=3)


##########################################


# In simple cases, counts of successes
# and number of trials can be just typed:

ntrials <- c(40,20,20,20)
xsuccesses <- c(1,2,2,4)
names(xsuccesses) <- LETTERS[1:4]
ex1D<-binomRDci(x=xsuccesses, n=ntrials, method="ADD1",
 type="Dunnett")
ex1D

ex1W<-binomRDci(x=xsuccesses, n=ntrials, method="ADD1",
 type="Williams", alternative="greater")
ex1W

# results can be plotted:
plot(ex1D, main="Comparisons to control group A", lines=0, linescol="red", lineslwd=2)

# summary gives a more detailed print out:
summary(ex1W)

# if data are represented as dichotomous variable
# in a data.frame one can make use of table:


#################################

data(liarozole)

head(liarozole)

binomRDci(Improved ~ Treatment, data=liarozole,
 type="Tukey")

# here, it might be important to define which level of the
# variable 'Improved' is to be considered as success

binomRDci(Improved ~ Treatment, data=liarozole,
 type="Dunnett", success="y", base=4)

# If data are available as a named kx2-contigency table:

tab<-table(liarozole)
tab

# Comparison to the control group "Placebo",
# which is the fourth group in alpha-numeric order:

CIs<-binomRDci(tab, type="Dunnett", success="y", base=4)

plot(CIs, lines=0)

}
\keyword{ htest }
\concept{binomial}
\concept{proportion}
\concept{risk difference}
\concept{simultaneous}
