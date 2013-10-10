\name{HCD}
\alias{HCD}
\docType{data}
\title{ Hell Creek Dinosaur Data}
\description{
 Counts of dinosaur families found in three stratigraphic levels of the Cretaceous period in the Hell Creek formation in North Dakota.
 The eight families are the Ceratopsidae (Ce), Hadrosauridae (Ha), Hypsilophodontidae (Hy), Pachycephalosauridae (Pa), Tyrannosauridae (Ty), Ornithomimidae
(Or), Sauronithoididae (Sa) and Dromiaeosauridae (Dr).

}
\usage{data(HCD)}
\format{
  A data frame with 3 observations on the following 9 variables.
  \describe{
    \item{Level}{a factor with levels \kbd{Lower}, \kbd{Middle}, \kbd{Upper}, specifiyng the stratigraphic levels}
    \item{Cr}{a numeric vector, counts of the Ceratopsidae }
    \item{Ha}{a numeric vector, counts of the Hadrosauridae}
    \item{Hy}{a numeric vector, counts of the Hypsilophodontidae}
    \item{Pa}{a numeric vector, counts of the Pachycephalosauridae}
    \item{Ty}{a numeric vector, counts of the Tyrannosauridae}
    \item{Or}{a numeric vector, counts of the Ornithomimidae}
    \item{Sa}{a numeric vector, counts of the Sauronithoididae}
    \item{Dr}{a numeric vector, counts of the Dromiaeosauridae}
  }
}

\source{
 Table 1 in: Rogers, JA and Hsu, JC (2001): Multiple Comparisons of Biodiversity. Biometrical Journal 43, 617-625.
}
\references{
  Sheehan, P.M., et al. (1991): Sudden extinction of the Dinosaurs: Latest Cretaceous, Upper Great Plains, U.S.A. Science 254, 835-839.
}
\examples{

data(HCD)
str(HCD)
HCD

mat<-as.matrix(HCD[,-c(1)])

rownames(mat)<-HCD$Level

mosaicplot(mat, las=1)

estSimpsonf(X=HCD[,-c(1)], f=HCD$Level)

estShannonf(X=HCD[,-c(1)], f=HCD$Level)

}
\keyword{datasets}
