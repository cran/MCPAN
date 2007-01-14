### Name: powerbinom
### Title: Approximate power for multiple contrast tests of binomial
###   proportions
### Aliases: powerbinom
### Keywords: htest

### ** Examples


# Assume, one wants to perform a test for increasing trend
#  using Williams type of contrasts among I=5 groups
#  (e.g. 4 doses and one control).
#  Proportions are assumed to have values
#  pi=(0.1,0.12,0.14,0.14,0.2) under the alternative.


powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(20,20,20,20,20), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(30,30,30,30,30), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(60,60,60,60,60), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(80,80,80,80,80), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(100,100,100,100,100), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(150,150,150,150,150), type = "Williams",
  method = "ADD1", alternative = "greater")

powerbinom(p=c(0.1, 0.12, 0.14, 0.14, 0.2),
 n=c(190,140,140,140,140), type = "Williams",
  method = "ADD1", alternative = "greater")




