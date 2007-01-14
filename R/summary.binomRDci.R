"summary.binomRDci"<-function(object,...)
{

args<-list(...)
if(is.null(args$digits))
 {digits<-4}
else
 {digits<-args$digits}


cat("Summary statistics: \n")

summarystat<-rbind("number of successes"=object$x,
"number of trials"=object$n,
"estimated success probability"=round(object$x/object$n, digits=digits))

colnames(summarystat)<-object$names

print(summarystat, digits=digits)


cat("\n Contrast matrix: \n")

print(object$cmat, digits=digits)

cat("\n")

print(object, digits=digits)

invisible(object)
}