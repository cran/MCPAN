\name{MCPAN-package}
\alias{MCPAN-package}
\alias{MCPAN}
\docType{package}
\title{
Multiple comparison procedures based on normal approximation.
}
\description{
Multiple contrast tests and simultaneous confidence intervals using normal approximation, if individuals are randomly assigned to treatments in a oneway layout. For some cases improvements compared to crude normal approximation are implemented.
}

\details{
\tabular{ll}{
Package: \tab MCPAN\cr
Type: \tab Package\cr
Version: \tab 1.1-15\cr
Date: \tab 2013-09-30\cr
License: \tab GPL \cr
}

For dichotomous variables, approximate confidence intervals for the risk difference (Schaarschmidt et al. 2008) and odds ratio (Holford et al. 1989) are available. The implementation of multiple contrast methods for the risk ratio and the odds ratio may be seen as a generalization of methods in Holford et al. (1989), and the crude normal approximation as described in Gart and Nam (1988) as special cases of the framework described in Hothorn et al. (2008).
If the variable of interest is the rate of tumours in long-term rodent carcinogenicity trials (without cause of death information), 
confidence intervals for poly-k-adjusted tumour rates (Bailer and Portier, 1988) are available, as described in Schaarschmidt et al. (2008).
For abundance data of multiple species, asymptotic simultaneous confidence intervals for differences of Simpson and Shannon-indices are implemented, assuming multinomial count data (Rogers and Hsu, 2001, Fritsch and Hsu, 1999, Scherer et al., 2013).
For expected values of lognormal samples, asymptotic Wald-type intervals and a sampling based improvement are available (Schaarschmidt, 2013).

}

\author{
Frank Schaarschmidt, Daniel Gerhard, Martin Sill
Maintainer: Frank Schaarschmidt <schaarschmidt@biostat.uni-hannover.de>
}

\references{

\emph{Schaarschmidt, F., Sill, M., and Hothorn, L.A. (2008):} Approximate Simultaneous Confidence Intervals for Multiple Contrasts of Binomial Proportions. Biometrical Journal 50, 782-792.

\emph{Schaarschmidt, F., Sill, M., and Hothorn, L.A. (2008):} Poly-k-trend tests for survival adjusted analysis of tumor rates formulated as approximate multiple contrast test. Journal of Biopharmaceutical Statistics 18, 934-948.

\emph{Holford, T.R., Walter, S.D. and Dunnett, C.W. (1989):} Simultaneous interval estimates of the odds ratio in studies with two or more comparisons. Journal of Clinical Epidemiology 42, 427-434.

\emph{Gart, J.J. and Nam, J. (1988)}: Approximate interval estimation of the ratio of binomial parameters: A review and corrections for skewness. Biometrics 44, 323-338.

\emph{Schaarschmidt, F. (2013).} Simultaneous confidence intervals for multiple comparisons among expected values of log-normal variables. Computational Statistics and Data Analysis 58, 265-275.

\emph{Hothorn, T., Bretz. F, and Westfall, P. (2008):} Simultaneous inference in general parametric models. Biometrical Journal 50(3), 346-363. 

\emph{Bailer, J.A. and Portier, C.J. (1988):} Effects of treatment-induced mortality and tumor-induced mortality on tests for carcinogenicity in small samples. Biometrics 44, 417-431.

\emph{Bretz, F., Hothorn, L. (2002):} Detecting dose-response using contrasts: asymptotic power and sample size determination for binomial data. Statistics in Medicine 21: 3325-3335.

\emph{Rogers, J.A. and Hsu, J.C. (2001):} Multiple Comparisons of Biodiversity. Biometrical Journal 43, 617-625.

\emph{Fritsch, K.S., and Hsu, J.C. (1999):} Multiple Comparison of Entropies with Application to Dinosaur Biodiversity. Biometrics 55, 1300-1305.

\emph{Scherer, R., Schaarschmidt, F., Prescher, S., and Priesnitz, K.U. (2013):} Simultaneous confidence intervals for comparing biodiversity indices estimated from overdispersed count data. Biometrical Journal 55, 246-263.
}

\keyword{ package }


\examples{


# # # 1)
# Simultaneous confidence intervals 
# for 2xk tables of binomial data: 
# binomRDtest, binomRDci

# Difference of proportions

binomRDci(x=c(2,6,4,13), n=c(34,33,36,34),
 names=c("Placebo", "50", "75", "150"),
 type="Dunnett", method="ADD1")

# Odds ratios:

binomORci(x=c(2,6,4,13), n=c(34,33,36,34),
 names=c("Placebo", "50", "75", "150"),
 type="Dunnett")


# # # 
# Simultaneous confidence intervals for comparing a treatment
# (trt) to 3 controls (Var1-Var3) in terms of differences of
# Simpson indices for a community comprising 33 species.

PSM <-
as.data.frame(structure(c(0, 0, 2, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 
2, 1, 1, 1, 0, 1, 2, 50, 25, 29, 42, 1, 1, 0, 3, 14, 6, 6, 24, 
64, 56, 121, 98, 1, 1, 1, 4, 410, 357, 586, 588, 16, 29, 21, 
38, 1, 1, 1, 1, 7, 12, 7, 11, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 4, 
1, 4, 3, 11, 4, 0, 0, 1, 0, 0, 1, 5, 0, 0, 0, 1, 1, 0, 0, 1, 
0, 30, 31, 10, 42, 0, 0, 1, 0, 7, 8, 10, 13, 111, 125, 112, 73, 
2, 1, 0, 0, 67, 64, 81, 102, 0, 0, 1, 0, 0, 0, 0, 1, 21, 20, 
14, 24, 0, 1, 0, 0), .Dim = c(4L, 33L), .Dimnames = list(c("Trt", 
"Var1", "Var2", "Var3"), c("Sp1", "Sp2", "Sp3", "Sp4", "Sp5", "Sp6", 
"Sp7", "Sp8", "Sp9", "Sp10", "Sp11", "Sp12", "Sp13", "Sp14", 
"Sp15", "Sp16", "Sp17", "Sp18", "Sp19", "Sp20", "Sp21", "Sp22", 
"Sp23", "Sp24", "Sp25", "Sp26", "Sp27", "Sp28", "Sp29", "Sp30", 
"Sp31", "Sp32", "Sp33"))))

fvar<-factor(row.names(PSM), levels=row.names(PSM))

Simpsonci(X=PSM, f=fvar)

# The complete data is available in package simboot.

# # #
# Simultaneous confidence intervals for ratios of expected values
# under lognormal assumption

x <- rlnorm(n=40, meanlog=rep(c(0,0.1,1,1), each=10), sdlog=rep(c(0.2,0.2,0.5,0.5), each=10))
f <- as.factor(rep(LETTERS[1:4], each=10))

lnrci(x=x, f=f, type="Tukey", method="GPQ", B=10000)


}
