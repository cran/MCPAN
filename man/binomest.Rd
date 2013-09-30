\name{binomest}
\alias{binomest}
\alias{binomest.default}
\alias{binomest.formula}
\alias{binomest.table}

\title{ Only for internal use.  }
\description{
  Groupwise point and variance estimates for binomials,
  if data are given as numeric vectors of successes x and trials n,
  with I the number of levels in the one-way layout}
\usage{
binomest(x, ...)

\method{binomest}{default}(x, n, names=NULL,
 method="Wald", success=NULL, ...)

\method{binomest}{formula}(formula, data,
 method="Wald", success=NULL, ...)

\method{binomest}{table}(x, method="Wald",
 success=NULL, ...)

}
\arguments{
  \item{x}{either a numeric vector of the number of successes, then \kbd{n} must be specified,
 or an object of class "table" as can be obtained by using \link{table} on a data.frame with to categorical variables }
  \item{n}{a numeric vector of the number of trials, only needed, if \kbd{x} is a numeric vector  }
  \item{names}{ optional character vector of group names (labels for later output), must have the same length as \kbd{x} and \kbd{n} }
  \item{formula}{a two-sided formula of the style 'response ~ treatment', where 'response' should be a categorical variable with two levels, while treatment should be a factor specifying the treatment levels}
  \item{data}{a data.frame, containing the variables specified in formula}
  \item{method}{ a character string, specifying the method for variance estimator, with options "Wald","ADD1","ADD2" }
  \item{success}{an optional character string, to indicate the level considered as success if data are given as table, or data.frame, or, serving as label for the event considered as success in later functions}
  \item{\dots}{ currently not used}
}
\details{
  Only for internal use.
}
\value{

  \item{Y}{a numeric vector of the groupwise number of successes}
  \item{n}{a numeric vector of the groupwise number of trials (observations)}
  \item{estimate}{ a numeric vector of groupwise point estimates for the probability of success, Y/n}
  \item{estp}{a numeric vector of groupwise interval midpoints}
  \item{varp}{a numeric vector of groupwise variance estimators, to be used for CI construction}
  \item{varcor}{ a numeric vector of groupwise variance estimators, to be used for correlation structures and test
 these values are adjusted to be larger than zero also in cases where Y=0 or Y=n}
  \item{names}{ a character vector of the level names of \kbd{f}}
  \item{success}{ a character string naming the category which has been defined as success }
}

\examples{

# if data are available as counts:

nsuccess<-c(1,2,6,8)
ntrials<-c(20,20,20,20)
binomest(x=nsuccess, n=ntrials)

binomest(x=nsuccess, n=ntrials,
 names=c("Control", "A", "B", "C"))

# if data are available as data.frame
# with categorical response variable
# and factor as grouping variable 

data(liarozole)
binomest(Improved ~ Treatment, data=liarozole)
binomest(Improved ~ Treatment, data=liarozole, success="y")

# if data are available as table
# and factor as grouping variable 

data(liarozole)
tab<-table(liarozole)

binomest(tab)
binomest(tab, success="y")


}


\keyword{ internal }