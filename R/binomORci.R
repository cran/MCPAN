`binomORci` <-
function(x, ...){UseMethod("binomORci")}


`binomORci.default` <-
function(x, n, names=NULL, type="Dunnett",
 cmat=NULL, alternative="two.sided", conf.level=0.95, dist="MVN", ...)
{
 require(multcomp)

args<-list(...)

# check x, n

    k <- length(x)
    if(k<2)
     {stop("x, n must contain at least two elements")}

    if(any(n<2)) 
     {stop("sample size must be at least 2 in each group")}   

    if(length(n)!=k)
     {stop("x, n must be of equal length")}

    if(!is.numeric(x) | !is.numeric(n))
     {stop("x, n must be numeric vectors")}



# check the names
# if no names are given, available names of x are taken
# if names are specified, names of x/n are overwritten

    gnames<-names

    if(is.null(gnames))
     {
      if(is.null(names(x)))
       {gnames <- names(x) <- names(n) <- as.character(1:k)}
       else
        {gnames <- names(n) <- names(x)}
     }
     else
      {
       if(length(gnames)!=k)
        {stop("length of names does not fit to length of x,n")}
        else
         {names(x) <- names(n) <- as.character(gnames)}
      }

# check the contrast matrix

    if (is.null(cmat)) {
      if(type=="Dunnett") {
        if(is.null(args$base)){base<-1}
        else{base<-args$base}
        cmat <- contrMat(n=n, type=type, base=base)
       }
       else{cmat <- contrMat(n = n, type = type)}
    }
    else {
        if (!is.matrix(cmat) || ncol(cmat) != k)
         {stop("cmat must be a matrix with number of columns = number of groups")}
    }


# get the point and variance estimates

    est <- binomest.default(x=x, n=n, names=gnames, success=args$success)
    grp<-as.factor(est$names)
    logisticfit <- glm(cbind(x,n-x) ~ 0 + grp, family=binomial(link="logit"))
    eta <- coefficients(logisticfit)
    sigma <- vcov(logisticfit)

    out<-Waldci( estp=eta,
     varcor=diag(sigma),
     varp=diag(sigma),
     cmat=cmat,
     alternative = alternative,
     conf.level=conf.level,
     dist=dist)


    conf.int <- exp(out$conf.int)
    estimate <- exp(cmat %*% eta)

    if(!is.null(dimnames(cmat)[[1]]))
    {
    cnamesD<-dimnames(cmat)[[1]]
    cnamesSlist<-strsplit(cnamesD, "-")
    cnamesOR<-unlist(lapply(cnamesSlist, FUN=function(x){paste(x, collapse="/")}))
    }
    else{cnamesOR<-paste("C",1:nrow(cmat),sep="")}

    rownames(estimate)<-cnamesOR
    rownames(conf.int)<-cnamesOR    

    out$conf.int<-conf.int
    out$estimate<-estimate
    colnames(cmat) <- gnames
    out$x <- x
    out$n <- n
    out$p <- est$estimate
    out$success <- est$success
    out$names <- gnames
    out$method <- "glm"
    out$cmat <- cmat

    class(out) <- "binomORci"
    return(out)
}


`binomORci.formula` <-
function(formula, data, type="Dunnett",
 cmat=NULL, alternative="two.sided", conf.level=0.95, dist="MVN", ...)
{
args<-list(...)

est<-binomest.formula(formula=formula, data=data, success=args$success)

args$x<-est$Y
args$n<-est$n
args$names<-est$names
args$type<-type
args$cmat<-cmat
args$alternative<-alternative
args$conf.level<-conf.level
args$success<-est$success
args$dist<-dist

out<-do.call("binomORci.default", args)

return(out)

}




`binomORci.table` <-
function(x, type="Dunnett",
 cmat=NULL, alternative="two.sided", conf.level=0.95, dist="MVN", ...)
{
args<-list(...)

est<-binomest.table(x=x, success=args$success)

args$x<-est$Y
args$n<-est$n
args$names<-est$names
args$type<-type
args$cmat<-cmat
args$alternative<-alternative
args$conf.level<-conf.level
args$success<-est$success
args$dist<-dist

out<-do.call("binomORci.default", args)

return(out)

}

`binomORci.matrix` <-
function(x, type="Dunnett",
 cmat=NULL, alternative="two.sided", conf.level=0.95, dist="MVN", ...)
{
args<-list(...)

tab<-as.table(x)

est<-binomest.table(x=tab, success=args$success)

args$x<-est$Y
args$n<-est$n
args$names<-est$names
args$type<-type
args$cmat<-cmat
args$alternative<-alternative
args$conf.level<-conf.level
args$success<-est$success
args$dist<-dist

out<-do.call("binomORci.default", args)

return(out)

}

