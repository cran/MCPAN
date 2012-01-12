\name{liarozole}
\alias{liarozole}
\docType{data}
\title{ Marked improvement of psoriasis after application of liarozole}
\description{
  In a placebo controlled clinical trial, patients with psoriasis were randomly assigned
 to a placebo group and three dose groups (50 mg, 75 mg, and 150 mg). Variable of primary
 interest was the proportion of patients with marked improvement of psoriasis.
 This data.frame mimics how raw data could have been represented in a larger data frame.
}
\usage{data(liarozole)}
\format{
  A data frame with 137 observations on the following 2 variables.
  \describe{
    \item{Improved}{a factor with levels \kbd{n}, \kbd{y}, for "no" and "yes"}
    \item{Treatment}{a factor with levels \kbd{Dose150}, \kbd{Dose50}, \kbd{Dose75}, \kbd{Placebo}}
  }
}
\details{
   For illustrative purpose only. Number of successes recalculated from proportions
 presented in the publication, while the number of patients in group \kbd{Dose50} was not exactly clear.
}
\source{
  Berth-Jones J, Todd G, Hutchinson PE, Thestrup-Pedersen K, Vanhoutte FP:
  Treatment of psoriasis with oral liarozole: a dose-ranging study.
  British Journal of Dermatology 2000; 143: 1170-1176.
}

\examples{
data(liarozole)
head(liarozole)
# create a contingency table:

table(liarozole)


# the order of the groups is alpha-numeric,
# and "y" for success is of higher order than
# to change the order:

liarozole$Treatment<-factor(liarozole$Treatment,
 levels=c("Placebo", "Dose50", "Dose75", "Dose150"))

liarozole$Improved<-factor(liarozole$Improved,
 levels=c("y", "n"))


tab<-table(liarozole)
tab

# Approximate simultaneous confidence intervals
# for the differences  pDose-pPlacebo:

LCI<-binomRDci(tab, type="Dunnett", 
alternative="greater", method="ADD1")

LCI

plot(LCI, main="Proportion of patients
 with marked improvement")

# Perform a test on increasing trend 
# vs. the placebo group:

Ltest<-binomRDtest(tab, type="Williams", 
alternative="greater", method="ADD1")

summary(Ltest)

}
\keyword{datasets}
\concept{binomial}
\concept{proportion}
