\name{multinomORci}
\alias{multinomORci}

\title{
Simultaneous confidence intervals for odds ratios comparing multiple odds and multiple treatments in a contingency table
}
\description{
Testversion. Two methods are provided to compute simultaneous confidence intervals for the comparison of several types of odds between several multinomial samples. Asymptotic Wald-type intervals (incl. replacing zero by some small number) as well as a method that computes simultaneous percentile intervals based on samples from the joint Dirichlet-posterior distribution with vague prior. A separate multinomial distribution is assumed for each row of the contingency table.
}
\usage{
multinomORci(Ymat, cmcat=NULL, cmgroup=NULL, cimethod = "DP", 
alternative = "two.sided", conf.level = 0.95, 
bycomp = FALSE, bychr = " btw ", ...)
}

\arguments{
  \item{Ymat}{ a \code{matrix} or \code{table}: input data as a 2-dimensional contingency table, containing the counts of the categories in the different treatment groups, with rows of the table representing the treatment groups and columns of the table representing the categories. Also \code{data.frame}s can be provided given that they contain counts only. Names of treatments/groups may be attached as \code{row.names}, names of categories may be attached as \code{col.names}.}

  \item{cmcat}{ a \code{matrix} or a \code{character} string: the contrast matrix that specifies which odds to compute between the categories. Column number of \code{cmcat} must be equal to the column number of \code{Ymat}; or, a \code{character} string (\code{"Dunnett", "Tukey"}) invoking baseline odds or all pairwise odds between categories; if \code{NULL} baseline odds are computed}

  \item{cmgroup}{a \code{matrix} or a \code{character} string: the contrast matrix that specifies, which treatment groups should be compared. Column number of \code{cmgroup} must be equal to the row number of \code{Ymat}; or, a \code{character} string (\code{"Dunnett", "Tukey"}) invoking comparisons to the first group or all pairwise comparisons between groups; if \code{NULL} comparisons to control are computed}

  \item{cimethod}{\code{character} string specifyoing the method for computation: \code{"DP"} invokes use of the sampling method based on the Drirchlet posterior with vague prior; \code{"Wald"} invokes use of an asmptotic method;
}
  \item{alternative}{single \code{character} string: \code{"two.sided"}: two-sicded intervals; \code{"less"} intervals with upper limits only; \code{"greater"} intervals with lower limits only.
}
  \item{conf.level}{single number: the simultaneous confidcence level}

  \item{bycomp}{logical, if \code{bycomp=FALSE} parameters are ordered by odds first; if \code{bycomp=TRUE} parameters are ordered by between-group-comparison first}
  \item{bychr}{character string separating the name of the odds from the name of the between group comparison in the output
}
  \item{\dots}{ further arguments to be passed to the internal functions: if \code{cimethod = "DP"} argument BSIM (defaults to \code{BSIM=10000}) 
defines the number of samples drwan from the Dirichlet posterior, and \code{prior} allows to define a single number, vector or matrix of alpha-parameters for the Dirichlet prior. If not specified, Dirichlet(1,1,...,1) is used as a prior, independent for each treatemnet group.
if \code{cimethod = "Wald"}, argument \code{addx} sets the quantity to be added to each cell/row/total of the contingency table.
}
}

\details{
Testversion.
}

\value{
A list with items
\item{SCI }{ a \code{data.frame} with names of comprisons as well as the lower and upper confidence limits}
\item{details}{a list with computational details depending on the \code{cimethod}:  for \code{"DP"}, the prior parameters, the complete sample from posterior, etc.) , for \code{"Wald"}-type (observations added to the table, estimated correlation matrix and covariance matrix, mulivariate normal quantiles) }

}

\author{
Frank Schaarschmidt
}

\seealso{
\code{\link{as.data.frame.multinomORci}}, \code{\link{print.multinomORci}} 
}
 

\examples{

# Randomized clinical trial 2 treatment groups (injection of saline or sterile water)
# to cure chronic pain after whiplash injuries. Response are 3 (ordered) categories,
# 'no change', 'improved', 'much improved'. Source: Hand, Daly, Lunn, McConway,
# Ostrowski (1994): A handbook of small data sets. Chapman & Hall, Example 124, page 993


dwi <- data.frame("no.change"=c(1,14), "improved"=c(9,3), "much.improved"=c(10,3))
rownames(dwi) <- c("sterile3", "saline3")

dwi

DP1dwi <- multinomORci(Ymat=dwi, cmcat="Dunnett", cmgroup="Tukey", cimethod="DP", BSIM=5000)
DP1dwi 

# at logit-scale (i.e., not backtransformation)
print(DP1dwi , exp=FALSE)

\dontrun{
# Compute asymptotic Wald-type intervals
Waldwbc <- multinomORci(Ymat=dwi, cmcat="Dunnett", cmgroup="Tukey", cimethod="Wald")
Waldwbc
print(Waldwbc, exp=FALSE)
}

}

\keyword{ htest }
\concept{ Confidence intervals }
