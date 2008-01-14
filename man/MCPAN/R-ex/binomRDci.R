### Name: binomRDci
### Title: Simultaneous confidence intervals for contrasts of independent
###   binomial proportions (in a oneway layout)
### Aliases: binomRDci binomRDci.default binomRDci.table binomRDci.matrix
###   binomRDci.formula
### Keywords: htest

### ** Examples


# In simple cases, counts of successes
# and number of trials can be just typed:

ntrials <- c(40,20,20,20)
xsuccesses <- c(1,2,2,4)
names(xsuccesses) <- LETTERS[1:4]
ex1D<-binomRDci(x=xsuccesses, n=ntrials, method="ADD1",
 type="Dunnett")
ex1D

ex1W<-binomRDci(x=xsuccesses, n=ntrials, method="ADD1",
 type="Williams", alternative="greater")
ex1W

# results can be plotted:
plot(ex1D, main="Comparisons to control group A")

# summary gives a more detailed print out:
summary(ex1W)

# if data are represented as dichotomous variable
# in a data.frame one can make use of table:

data(liarozole)

head(liarozole)

binomRDci(Improved ~ Treatment, data=liarozole, type="Tukey")
# here it might be important to define which level of the
# variable 'Improved' is to be considered as success
binomRDci(Improved ~ Treatment, data=liarozole, type="Tukey", success="y")

# If data are available as a named kx2-contigency table:

tab<-table(liarozole)
tab

binomRDci(tab, type="Tukey", success="y")




