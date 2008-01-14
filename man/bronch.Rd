\name{bronch}
\alias{bronch}
\docType{data}
\title{Rodent bronchial carcinoma data}
\description{
 In a long term rodent carcinogenicity study on female B6C3F1 mice, 
 the effect of vinylcyclohexene diepoxide on the incidence of murine
 alveolar/bronchiolar tumors was assessed. The mice were exposed to
 0 mg/ml, (group 0), 25 mg/ml (group 1), 50 mg/ml (group 2),
 and 100 (group 3) mg/ml, with 50 animals per group.
}
\usage{data(bronch)}
\format{
  A data frame with 200 observations on the following 3 variables.
  \describe{
    \item{\code{group}}{a factor with levels \code{0} \code{1} \code{2} \code{3}, labelling the control and the three dose groups}
    \item{\code{Y}}{a logical vector, indicating whether a tumour was present at time of death (if TRUE), or not (if FALSE) }
    \item{\code{time}}{a numeric vector, the time of death, counted in days? from begin of the study}
  }
}
\details{
  Not yet checked for consistency with the source!
}
\source{
Piegorsch WW and Bailer AJ (1997): Statistics for environmental biology and toxicology.
Chapman and Hall, London.
Table 6.5, page 238.

}
\references{
Portier cJ and Bailer AJ (1989): Testing for increased carcinogenicity using survival-adjusted quantal response tests.
Fundamental and Applied Toxicology 12, 731.
}
\examples{
data(bronch)
# raw tumour counts:

table(bronch[c("group","Y")])

# groupwise times of death:

boxplot(time ~ group, data=bronch, horizontal=TRUE)

# Using poly3estf, we can produce the
# summary statistics as presented in 
# Table 6.6, page 239, of Piegorsch and Bailer (1997):

poly3estf(status=bronch$Y, time=bronch$time, f=bronch$group)


}
\keyword{datasets}
