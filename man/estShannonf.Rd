\name{estShannonf}
\alias{estShannonf}

\title{ Estimate the Shannon-Wiener index }
\description{
  Calculate estimates of the Shannon-Wiener index after pooling over several samples, grouped by a factor variable.
}
\usage{
estShannonf(X, f)
}

\arguments{
  \item{X}{ an n times p matrix with integer entries, where n is the number of samples and p is the number of species}
  \item{f}{ a factor variable of length n, grouping the observations in \code{X} }
}
\details{
 The function splits \code{X} according to the levels of the grouping variable \code{f}, builds the sum over each column and calculates the Shannon index ove the resulting counts.
 
}
\value{
A list, containing the elements:
 \item{estimae }{ a named numeric vector, the groupwise Shannon indices with bias correction according to Fritsch and Hsu (1999)}
 \item{estraw }{ a named numeric vector, the groupwise Shannon indices, without bias correction}
 \item{varest }{ a named numeric vector, the groupwise variance estimates of the Shannon indices}
 \item{table }{a matrix, giving the summarized counts of the groups in the rows}
}
\references{ Fritsch, KS, and Hsu, JC (1999): Multiple Comparison
of Entropies with Application to Dinosaur Biodiversity. Biometrics 55, 1300-1305. }

\examples{

data(HCD)
HCD

# Groupwise point estimates:

est<-estShannonf(X=HCD[,-1], f=HCD[,1])

est



}

\keyword{ htest }
\concept{biodiversity }
