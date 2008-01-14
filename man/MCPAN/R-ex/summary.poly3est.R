### Name: summary.poly3est
### Title: Detailed print out for poly3est
### Aliases: summary.poly3est
### Keywords: print

### ** Examples

data(methyl)
head(methyl)
ests<-poly3estf(time=methyl$death, status=methyl$tumour, f=methyl$group)
summary(ests)




