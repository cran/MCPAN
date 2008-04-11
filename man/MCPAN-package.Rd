\name{MCPAN-package}
\alias{MCPAN-package}
\alias{MCPAN}
\docType{package}
\title{
Multiple comparison procedures based on normal approximation.
}
\description{
Multiple contrast tests and simultaneous confidence intervals using normal approximation, if individuals
are randomly assigned to treatments in a oneway layout. For dichotomous variables,
approximate confidence intervals for the risk difference, risk ratio and odds ratio are available.
If the variable of interest is the rate of tumours in long-term rodent carcinogenicity trials (without cause of death information), 
confidence intervals for poly-k-adjusted tumour rates are available.
For abundance data of multiple species, approximate simultaneous confidence intervals for differences of Simpson and Shannon-indices are implemented.

Please note, that the methods implemented in this package are NOT validated yet and NOT published so far. 
}
\details{
\tabular{ll}{
Package: \tab MCPAN\cr
Type: \tab Package\cr
Version: \tab 1.1-4\cr
Date: \tab 2008-04-11\cr
License: \tab GPL \cr
}

}

\author{
Frank Schaarschmidt, Daniel Gerhard, Martin Sill
Maintainer: Frank Schaarschmidt <schaarschmidt@biostat.uni-hannover.de>
}

\references{

The Add-2 Confidence interval for the difference of two binomial proportions:

Agresti, A. and Caffo, B.(2000):
 Simple and effective confidence intervals for proportions and differences of proportions result from adding two successes and two failures.
 American Statistician 54, p. 280-288.

and its generalization to a single contrast of several proportions in:

Price, R.M. and Bonett, D.G. (2004):
 An improved confidence interval for a linear function of binomial proportions.
 Computational Statistics and Data Analysis 45, 449-456.

Simultaneous confidence intervals for Dunnett-type comparisons to control using odds ratios:

Holford, TR, Walter, SD and Dunnett, CW (1989): Simultaneous interval estimates of the odds ratio in
studies with two or more comparisons. Journal of Clinical Epidemiology 42, 427-434.


For long-term rodent carcinogenicity data: The assumptions of poly-3-adjustment are outlined in:

 Bailer, J.A. and Portier, C.J. (1988):
 Effects of treatment-induced mortality and tumor-induced mortality on tests for carcinogenicity in small samples.
 Biometrics 44, 417-431.

 Peddada, S.D., Dinse, G.E., and Haseman, J.K. (2005):
 A survival-adjusted quantal response test for comparing tumor incidence rates.
 Applied Statistics 54, 51-61.

For correlation of multiple contrasts of binomial proportions, see:
 Bretz F, Hothorn L.:
 Detecting dose-response using contrasts: asymptotic power and sample size determination for binomial data.
 Statistics in Medicine 2002; 21: 3325-3335.

Simulation results (coverage probability of simultaneous confidence intervals)
 for the binomial proportions and poly-3-adjusted tumour rates can be found in:

 Sill, M. (2007):
 Approximate simultaneous confidence intervals for multiple comparisons of binomial proportions.
 Master thesis, Institute of Biostatistics, Leibniz University Hannover.

}

\keyword{ package }

\seealso{

}

\examples{


# # # 1)
# Adjusted p-values and simultaneous confidence intervals 
# for 2xk tables of binomial data: 
# binomtest, binomci

# Difference of proportions

binomRDtest(x=c(2,6,4,13), n=c(34,33,36,34),
 names=c("Placebo", "50", "75", "150"),
 type="Dunnett", method="ADD1")

binomRDci(x=c(2,6,4,13), n=c(34,33,36,34),
 names=c("Placebo", "50", "75", "150"),
 type="Dunnett", method="ADD1")

# Odds ratios:

binomORci(x=c(2,6,4,13), n=c(34,33,36,34),
 names=c("Placebo", "50", "75", "150"),
 type="Dunnett")

# For more details on evaluation,
# see:
# ?liarozole

 data(liarozole) 



# # # 2)
# Adjusted p-values and simultaneous confidence intervals 
# for poly-3-adjusted tumour rates: 
# poly3test, poly3ci

data(methyl)
methyl

# poly-3-adjusted sample estimates:

poly3estf(time=methyl$death,
 status=methyl$tumour,
 f=methyl$group)

# Simultaneous Add-1-confidence intervals
# for difference to the control group:

poly3ci(time=methyl$death, status=methyl$tumour,
 f=methyl$group, method="ADD1",
 type="Dunnett", alternative="greater")

# Test for trend, based on Changepoint contrasts:

poly3test(time=methyl$death, status=methyl$tumour,
 f=methyl$group, method="ADD1",
 type="Changepoint", alternative="greater")


# # # 3) Plot of confidence intervals
# created by binomci and poly3ci:

MethylCI <- poly3ci(time=methyl$death, status=methyl$tumour,
 f=methyl$group, method="ADD1",
 type="Dunnett", alternative="greater")

plot(MethylCI)

}
