\name{estShannon}
\alias{estShannon}

\title{ Estimate the Shannon-Wiener Index }
\description{
 Calculates estimates of the Shannon-Wiener from count data.
}
\usage{
estShannon(x, Nspec = NULL)
}

\arguments{
  \item{x}{ a integer(numeric) vector of species counts }
  \item{Nspec}{ a single integer value, fixing the number of species to a certain value \code{Nspec} }
}

\value{
A list, containing the elements:
  \item{estimate }{ a single numeric value, the estimate with bias correction according to Fritsch and Hsu (1999)}
  \item{estraw }{ a single numeric value, the raw estimate }
  \item{varest }{ a single numeric value, the variance estimate according to Fritsch and Hsu (1999) }

}
\references{ Fritsch, KS, and Hsu, JC (1999): Multiple Comparison
of Entropies with Application to Dinosaur Biodiversity. Biometrics 55, 1300-1305. }


\seealso{ \code{\link{estSimpsonf}} for estimating Shannon indices pooled over several samples, grouped by a factor  }


\keyword{ htest }
\concept{ biodiversity }
