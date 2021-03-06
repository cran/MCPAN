\name{methyl}
\alias{methyl}
\docType{data}
\title{ NTP bioassay data: effect of methyleugenol on skin fibroma}
\description{
 NTP bioassay of methyleugenol: 200 male rats were randomly assigned to 4 treatment groups with balanced sample size 50.
 Individuals in treatment group 0, 1, 2, and 3 received doses of 0, 37, 75, and 150 mg methyleugenol per kg body weight, respectively.
 The response variable \kbd{tumour} is the presence of skin fibroma at time of death.
 The variable \kbd{death} gives individual time of death, with a final sacrifice of surviving animals at 730 days after begin of the assay.
}
\usage{data(methyl)}
\format{
  A data frame with 200 observations on the following 3 variables.
  \describe{
    \item{group}{a factor with levels \kbd{0}, \kbd{1}, \kbd{2}, \kbd{3}, specifying dose groups 0, 37, 75, and 150 mg/kg, respectively}
    \item{tumour}{a numeric vector, specifying whether a tumour was present at time of death}
    \item{death}{a numeric vector, specifying the time of death}
  }
}
\source{
  National toxicology program (2000).
}
\references{
  SD Peddada, GE Dinse, JK Haseman (2005): A survival-adjusted quantal response test for comparing tumour incidence rates. Applied Statistics 54, 51-61. 
}
\examples{
data(methyl)
# raw tumour proportions:
table(methyl[c("group", "tumour")])

# time of death:
boxplot(death~group, data=methyl, horizontal=TRUE)

}
\keyword{datasets}
\concept{poly-k}
