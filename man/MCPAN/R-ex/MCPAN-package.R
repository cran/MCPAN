### Name: MCPAN-package
### Title: Multiple comparison procedures using normal approximation
### Aliases: MCPAN-package MCPAN
### Keywords: package

### ** Examples


# # # 1)
# Adjusted p-values and simultaneous confidence intervals 
# for 2xk tables of binomial data: 
# binomtest, binomci

# Difference of proportions

binomRDtest(x=c(2,6,4,13), n=c(34,33,36,34),
 names=c("Placebo", "50", "75", "150"),
 type="Dunnett", method="ADD1")

binomRDci(x=c(2,6,4,13), n=c(34,33,36,34),
 names=c("Placebo", "50", "75", "150"),
 type="Dunnett", method="ADD1")

# Odds ratios:

binomORci(x=c(2,6,4,13), n=c(34,33,36,34),
 names=c("Placebo", "50", "75", "150"),
 type="Dunnett")

# For more details on evaluation,
# see:
# ?liarozole

 data(liarozole) 


# # # 2)
# Adjusted p-values and simultaneous confidence intervals 
# for poly-3-adjusted tumour rates: 
# poly3test, poly3ci

data(methyl)
methyl

# poly-3-adjusted sample estimates:

poly3estf(time=methyl$death,
 status=methyl$tumour,
 f=methyl$group)

# Simultaneous Add-1-confidence intervals
# for difference to the control group:

poly3ci(time=methyl$death, status=methyl$tumour,
 f=methyl$group, method="ADD1",
 type="Dunnett", alternative="greater")

# Test for trend, based on Changepoint contrasts:

poly3test(time=methyl$death, status=methyl$tumour,
 f=methyl$group, method="ADD1",
 type="Changepoint", alternative="greater")

# # # 3) Plot of confidence intervals
# created by binomci and poly3ci:

MethylCI <- poly3ci(time=methyl$death, status=methyl$tumour,
 f=methyl$group, method="ADD1",
 type="Dunnett", alternative="greater")

plot(MethylCI)






