\name{Shannonci}
\alias{Shannonci}
\title{ Confidence intervals for multiple contrasts of Shannon indices }
\description{
 Calculates simultaneous and local confidence intervals for differences of Shannon indices under the assumption of multinomial count data.

}
\usage{
Shannonci(X, f, cmat = NULL, type = "Dunnett", alternative = "two.sided",
 conf.level = 0.95, dist = "MVN", \dots)
}

\arguments{
  \item{X}{ a data.frame of dimensions n times p with integer entries, where n is the number of samples and p is the number of species }
  \item{f}{ a factor variable of length n, grouping the observations in \kbd{X} }
  \item{cmat}{ an contrast matrix; the number of columns should match the number of levels in \kbd{f} }
  \item{type}{ a single character string, currently one of "Dunnett","Tukey","Sequen" }
  \item{alternative}{ a single character string, one of "two.sided","less" (upper bounds),"greater" (lower bounds)}
  \item{conf.level}{ the confidence level of the simultaneous (or local) confidence intervals }
  \item{dist}{ a single character string, defining the type of quantiles to be used for interval calculation; "MVN" invokes simultaneous intervals, "N" invokes unadjusted confidence intervals with coverage probability conf.level for each of them  }
  \item{\dots}{ further arguments to be passed; currently only \kbd{base} is used, a single integer value, specifying which group to be taken as the control in case that \kbd{type="Dunnett"}, ignored otherwise}
}
\details{
 This function implements confidence intervals described by Fritsch and Hsu (1999) for the difference of Shannon indices between several groups.
 Deviating from Fritsch and Hsu, quantiles of the multivariate normal distribution based on a plug-in-estamator for the correlation matrix.

Note, that this approach, by assuming multinomial distribution for the vectors of counts, ignores the variability of the individual samples. If such extra-multinomial variatio is present in the data, the intervals will be too narrow, coverage probability will be substantially lower than specified in 'conf.level'. Consider approaches based on bootstrap instead (e.g., package simboot).

}
\value{
A list containing the elements:

  \item{conf.int }{ a matrix, containing the lower and upper confidence limits in the columns}
  \item{quantile}{a single numeric value, the quantile used for interval calculation}
  \item{estimate }{a matrix,containing the point estimates of the contrasts in its column}
  \item{cmat}{ the contrast matrix used}
  \item{methodname}{a character string, for printing}
  \item{sample.estimate}{A list of sample estimates as returned by \link{estShannonf} }
  and some of the input arguments

}

\references{ 
\emph{Fritsch, KS, and Hsu, JC (1999):} Multiple Comparison of Entropies with Application to Dinosaur Biodiversity. Biometrics 55, 1300-1305.
\emph{Scherer, R, Schaarschmidt, F, Prescher, S, and Priesnitz, KU (2013):} Simultaneous confidence intervals for comparing biodiversity
indices estimated from overdispersed count data. Biometrical Journal 55,246-263.
}

\author{ Frank Schaarschmidt }

\seealso{ \link{Simpsonci} for simultaneous and local intervals of differences of the Simpson index }

\examples{


data(HCD)

HCDcounts<-HCD[,-1]
HCDf<-HCD[,1]

# Comparison to the confidence bounds shown in
# Fritsch and Hsu (1999), Table 5, "Standard normal".

cmat<-rbind(
"HM-HU"=c(0,1,-1),
"HL-HM"=c(1,-1,0),
"HL-HU"=c(1,0,-1)
)

Shannonci(X=HCDcounts, f=HCDf, cmat=cmat,
 alternative = "two.sided", conf.level = 0.9, dist = "N")

# Note, that the calculated confidence intervals
# differ from those published by Fritsch and Hsu (1999),
# whenever Lower is involved.



# Comparison to the lower cretaceous,
# unadjusted confidence intervals:

Shannonci(X=HCDcounts, f=HCDf, type = "Dunnett",
 alternative = "greater", conf.level = 0.9, dist = "N")

# Stepwise comparison between the strata,
# unadjusted confidence intervals:

ShannonS<-Shannonci(X=HCDcounts, f=HCDf, type = "Sequen",
 alternative = "greater", conf.level = 0.9, dist = "N")


ShannonS

summary(ShannonS)

plot(ShannonS)


# A trend test based on multiple contrasts:

cmatTREND<-rbind(
"U-LM"=c(-0.5,-0.5,1),
"MU-L"=c(-1,0.5,0.5),
"U-L"=c(-1,0,1)
)

TrendCI<-Shannonci(X=HCDcounts, f=HCDf, cmat=cmatTREND,
 alternative = "greater", conf.level = 0.95, dist = "MVN")
TrendCI

plot(TrendCI)



  }


\keyword{ htest }
