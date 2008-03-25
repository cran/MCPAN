#

mosaicplot.Shannonci<-function(x, decreasing=NULL, ...)
{
tab<-x$sample.estimate$table

args<-list(...)

if(is.null(args$las))
 {args$las<-1}

if(is.null(args$main))
 {args$main<-""}


if(is.null(decreasing))
{
args$x<-tab
do.call("mosaicplot", args)
}
else{

if(!is.logical(decreasing)) 
 {stop("decreasing should be either TRUE, FALSE or NULL")}

if(decreasing)
{
cs<-apply(tab, 2, sum)
CO<-order(cs, decreasing=TRUE)
args$x<-tab[,CO]
do.call("mosaicplot", args)
}


if(!decreasing)
{
cs<-apply(tab, 2, sum)
CO<-order(cs, decreasing=FALSE)
args$x<-tab[,CO]
do.call("mosaicplot", args)
}
}

}

#

mosaicplot.Simpsonci<-function(x, decreasing=NULL, ...)
{
tab<-x$sample.estimate$table

args<-list(...)

if(is.null(args$las))
 {args$las<-1}

if(is.null(args$main))
 {args$main<-""}


if(is.null(decreasing))
{
args$x<-tab
do.call("mosaicplot", args)
}
else{

if(!is.logical(decreasing)) 
 {stop("decreasing should be either TRUE, FALSE or NULL")}

if(decreasing)
{
cs<-apply(tab, 2, sum)
CO<-order(cs, decreasing=TRUE)
args$x<-tab[,CO]
do.call("mosaicplot", args)
}


if(!decreasing)
{
cs<-apply(tab, 2, sum)
CO<-order(cs, decreasing=FALSE)
args$x<-tab[,CO]
do.call("mosaicplot", args)
}
}

}


