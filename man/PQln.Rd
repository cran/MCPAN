\name{PQln}
\alias{PQln}

\title{
Generalized pivotal quantity for log expected values of several lognormal samples.
}
\description{
Computes samples of the generalized pivotal quantities for the log of expected values of lognormal samples according to Krishnamoorthy and Mathew (2003). For internal use in \code{lndci}, \code{lnrci}.
}
\usage{
PQln(nx, mlx, varlx, B = 10000)
}

\arguments{
  \item{nx}{
 a numeric (integer) vector, the sample sizes for each of K groups
}
  \item{mlx}{
 a numeric vector, the sample estimates of the log mean for each of K groups
}
  \item{varlx}{
 a numeric vector, the (unbiased) sample estimates of the log variance for each of K groups
}
  \item{B}{
a single (integer) value, the number of samples to draw, defaults to 10000
}
}
\details{
Generates B samples from independent standard normal and chi^2 distributions for each of K independent groups in a one-way layout and computes the generalized pivotal quantities for the log expected value for each of the groups, according to Krishnamoorthy and Mathew (2003).
The input vectors \code{nx}, \code{mlx}, \code{varlx} must be vectors of length K.
}
\value{
A BxK matrix
}
\references{
Krishnamoorthy K, Mathew T (2003): Inferences on the means of lognormal distributions using generalized p-values and generalized condence intervals. Journal of Statistical Planning and Inference 115,
103-121.
}
\author{
Frank Schaarschmidt
}

\keyword{ htest }
