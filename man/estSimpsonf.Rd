\name{estSimpsonf}
\alias{estSimpsonf}

\title{ Estimate the Simpson index from several samples }
\description{
 Calculate estimates of the Simpson index after pooling over several samples, grouped by a factor variable.

}
\usage{
estSimpsonf(X, f)
}
\arguments{
  \item{X}{ a data.frame of dimension n times p with integer entries, where n is the number of samples and p is the number of species }
  \item{f}{ a factor variable of length n, grouping the observations in \kbd{X} }
}
\details{
 The function splits \kbd{X} according to the levels of the grouping variable \kbd{f}, builds the sum over each column and calculates the Shannon index ove the resulting counts.
 
}
\value{
 A list containing the items:
  \item{estimate }{ the groupwise point estimates of the Simpson index}
  \item{varest }{the groupwise variance estimates of the Simpson index}
  \item{table }{a matrix of counts,containing the summed observations for each level of \kbd{f} in its rows}
}
\references{ Rogers, JA and Hsu, JC (2001): Multiple Comparisons of Biodiversity. Biometrical Journal 43, 617-625. }


\seealso{ \link{estShannonf} }
\examples{

# Here, the estimates for the Hell Creek Dinosaur 
# example are compared to the estimates in 
# Tables 2 and 3 of Rogers and Hsu (2001).

data(HCD)
HCD

# Groupwise point estimates:

est<-estSimpsonf(X=HCD[,-1], f=HCD[,1])

est

# Table 2:


cmat<-rbind(
"lower-middle"=c(1,-1,0),
"lower-upper"=c(1, 0,-1),
"middle-upper"=c(0,1,-1))


# the point estimates:

# cmat %*% est$estimate
crossprod(t(cmat), est$estimate)


# the standard errors:
# sqrt(diag(cmat %*% diag(est$varest) %*% t(cmat)))

sqrt(diag(crossprod(t(cmat), crossprod(diag(est$varest), t(cmat)) ) ))


# Table 3:

cmat<-rbind(
"middle-lower"=c(-1,1,0),
"upper-lower"=c(-1,0,1))

# cmat %*% est$estimate
crossprod(t(cmat), est$estimate)


# sqrt(diag(cmat %*% diag(est$varest) %*% t(cmat)))

sqrt(diag(crossprod(t(cmat), crossprod(diag(est$varest), t(cmat)) ) ))

# Note, that the point estimates are exactly
# the same as in Rogers and Hsu (2001),
# but the variance estimates are not, whenever
# the Upper group is involved.


}

\keyword{ htest }
\concept{ biodiversity }
