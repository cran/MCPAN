### Name: binomORci
### Title: Simultaneous confidence intervals for odds ratios
### Aliases: binomORci binomORci.default binomORci.formula binomORci.table
###   binomORci.matrix
### Keywords: htest

### ** Examples

data(liarozole)

table(liarozole)

ORlia<-binomORci(Improved ~ Treatment, data=liarozole, success="y")
ORlia
summary(ORlia)
plot(ORlia)

# if data are available as table:

tab<-table(liarozole)
ORlia2<-binomORci(tab, success="y")
ORlia2





