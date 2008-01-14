### Name: plot.sci
### Title: Plot simultaneous confidence intervals
### Aliases: plot.sci plot.binomRDci plot.poly3ci plot.binomORci
### Keywords: hplot

### ** Examples


# An example with three dose groups
# compared to Placebo, binomial response:

resp<-c(2,8,6,13)
trials<-c(34,33,36,34)
names(resp)<-c("Placebo", "50", "75", "150")

ci<-binomRDci(x=resp, n=trials,
 type="Dunnett", method="ADD1")
plot(ci, line=c(0,0.1,0.2),linelty=c(1,2,3))




