`powermcp` <-
function(TExpH1, alpha=0.05, corrmatH1, alternative="two.sided")
{

require(mvtnorm)

if(length(TExpH1)!=nrow(corrmatH1))
 {stop("length of TexpH1 and number of rows in corrmatH1 must be the same")}

# the dimension of the distribution, i.e. the number of comparisons:

ncons<-nrow(corrmatH1)

if(alternative=="less")
 {
  quantH0 <- qmvnorm(p=1-alpha, tail="upper.tail",
   mean = rep(0,ncons), corr = corrmatH1)[["quantile"]]

  centre <- quantH0-TExpH1

  power1 <- 1-pmvnorm(lower=as.numeric(centre), upper=rep(Inf, ncons),
   mean=rep(0, ncons), corr=corrmatH1 )

 }

if(alternative=="greater")
 {
  quantH0 <- qmvnorm(p=1-alpha, tail="lower.tail",
   mean = rep(0,ncons), corr = corrmatH1)[["quantile"]]

  centre <- quantH0-TExpH1
  power1 <- 1-pmvnorm(lower=rep(-Inf,ncons), upper=as.numeric(centre),
   mean=rep(0,ncons), corr=corrmatH1)

 }

if(alternative=="two.sided")
 {
  quantH0 <- qmvnorm(p=1-alpha, tail="both.tails",
   mean = rep(0,ncons), corr = corrmatH1)[["quantile"]]

  centrelower <- -quantH0-TExpH1
  centreupper <- quantH0-TExpH1

  power1 <- 1-pmvnorm(lower=as.numeric(centrelower),
   upper=as.numeric(centreupper),
   mean=rep(0,ncons), corr=corrmatH1)

 }

return(power=power1)


}

