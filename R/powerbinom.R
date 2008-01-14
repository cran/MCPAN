`powerbinom` <-
function(p, n, alpha=0.05, type="Williams", cmat=NULL, method="Wald", alternative="less")
{
ngroups<-length(p)

if(any(n<=40))
 {warning("For n < 40 this function might give misleading approximations")}

require(multcomp)

if(is.null(cmat))
 {
 type<-match.arg(type,
  choices=c("Tukey","Dunnett","Sequence","AVE","Changepoint","Williams", "Marcus", "McDermott"))
 cmat<-contrMat(n=n, type=type)
 }

ncons<-nrow(cmat)

if(ncol(cmat)!=ngroups | length(n)!=ngroups)
 {stop("p,n,ncol(cmat) must be of the same length")}

# expectation of the linear combinations (LExpectH1),
# expectation of the groupwise variance estimators (VarpH1)
# expectation of the variance estimators for the linear combinations (LVarpH1)
# expectations of the teststatistics (TexpH1)
# under the alternative

if(method=="Wald")
 {
  LExpectH1 <- cmat%*%p
  VarpH1 <- p*(1-p)/n
  LVarH1 <- (cmat^2) %*% (VarpH1)
  TExpH1 <-LExpectH1/sqrt(LVarH1)
 }

if(method=="ADD1")
 {
  PExpH1<-(p*n+0.5)/(n+1)
  LExpectH1 <- cmat%*%PExpH1
  VarpH1 <- PExpH1*(1-PExpH1)/(n+1)
  LVarH1 <- (cmat^2) %*% (VarpH1)
  TExpH1 <-LExpectH1/sqrt(LVarH1)
 }

if(method=="ADD2")
 {
  PExpH1<-(p*n+1)/(n+2)
  LExpectH1 <- cmat%*%PExpH1
  VarpH1 <- PExpH1*(1-PExpH1)/(n+2)
  LVarH1 <- (cmat^2) %*% (VarpH1)
  TExpH1 <-LExpectH1/sqrt(LVarH1)
 }


# calculate the correlation matrix

corrmatH1 <- corrMatgen(CM=cmat, varp=VarpH1)

power<-powermcp(TExpH1=TExpH1, alpha=alpha,
 corrmatH1=corrmatH1, alternative=alternative)

return(as.numeric(power[1]))

}

