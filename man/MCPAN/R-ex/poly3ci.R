### Name: poly3ci
### Title: Simultaneous confidence intervals for contrasts of
###   poly-3-adjusted tumour rates
### Aliases: poly3ci
### Keywords: htest

### ** Examples


data(methyl)
methylD<-poly3ci(time=methyl$death, status=methyl$tumour, f=methyl$group, type = "Dunnett", method = "ADD1" )
methylD
plot(methylD, main="Simultaneous CI for Poly-3-adjusted tumour rates")




