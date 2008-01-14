`plot.binomORci` <-
function (x, ...) 
{
args<-list(...)
 if(is.null(args$line))
  {args$line<-1}

args$x<-x

do.call("plot.sci", args)
}


"plot.binomRDci"<-function(x, ...)
{
plot.sci(x,...)
}


"plot.poly3ci"<-function(x, ...)
{
plot.sci(x,...)
}



"plot.sci" <-
function(x, line = 0, linelty=2, linelwd=1, linecol="black", CIvert=FALSE, CIlty = 1, CIlwd=1, HL=TRUE, ...)
{

if(HL){
old.par <- par(no.readonly=TRUE)
}
method <- x$method
conf.int <- x$conf.int
esti <- x$estimate
compn <- dimnames(x$cmat)[[1]]
num <- 1:length(esti)
args <- list(...)
alternative <- x$alternative
conf.level <- x$conf.level
mymai <- par("mai")
type <- attr(x$cmat, "type")

if(is.null(args$cex))
 {args$cex <- 1}

if( alternative == "two.sided" )
 {

  lower <- conf.int[,1]; upper <- conf.int[,2]
 
  lplot <- min(lower, line)
  uplot <- max(upper, line)
 }

  # extract the intervall bounds:

 if(alternative == "less")
  {
   upper <- conf.int[,2] 
   lplot <- min(line, esti) 
   uplot <- max(upper, line)

  }

 if(alternative == "greater")
  {
   lower <- conf.int[,1] 
   lplot <- min(line, lower) 
   uplot <- max(esti, line)
  }

 llplot <- lplot - 0.2*abs(lplot-uplot)
 uuplot <- uplot + 0.2*abs(lplot-uplot)
  
if(is.null(args$main))
 {args$main <- ""}
 
if (is.null(args$ylab)) {args$ylab <- ""}
if (is.null(args$xlab)) {args$xlab <- ""}
 
### produce the plot:

# vertical CI:

if(CIvert==TRUE)
{
if(HL){
 plot.new()

 # the default margin size in inches
  mymai <- par("mai")

 # adjust margin under the x axis according to length of comparison names
  xwidth<- 1.5 * max(strwidth(compn, units = "inches", cex = par("cex.axis"))) 

 if (mymai[1] < xwidth) 
        mymai[1] <- xwidth
 par(mai=mymai, new=TRUE)
}

args$x <- num
args$y <- esti
args$axes <- FALSE
args$ylim <- c(llplot, uuplot) 
args$type <- "p"
args$pch <- 16

do.call("plot", args)

axis(side = 1, at = num, labels=compn, las=2, ... )
axis(side = 2, ...)
box()

if(alternative=="two.sided")
{
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lower[i], upper[i]), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = lower[i], pch="-", cex = args$cex*1.5)
  points(x = num[i], y = upper[i], pch="-", cex = args$cex*1.5)
  }
 }


if(alternative=="less")
{
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(llplot, upper[i]), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = upper[i], pch="-", cex = args$cex*1.5)
  }
 }


if(alternative=="greater")
{
 for(i in 1:length(num))
  {
  lines(x = c(num[i],num[i]), y = c(lower[i], uuplot), lty = CIlty, lwd=CIlwd)
  points(x = num[i], y = lower[i], pch="-", cex = args$cex*1.5)
  }
 }

abline(h=line, lty=linelty, lwd=linelwd, col=linecol)

}





# horizontal CI:



if(CIvert==FALSE)
{
if(HL){
 plot.new()

 # the default margin size in inches
  mymai <- par("mai")

 # adjust margin under the x axis according to length of comparison names
  ywidth<- 1.5 * max(strwidth(compn, units = "inches", cex = par("cex.axis"))) 

 if (mymai[2] < ywidth) 
        mymai[2] <- ywidth
 par(mai=mymai, new=TRUE)
}

args$y <- num
args$x <- esti
args$axes <- FALSE
args$xlim <- c(llplot, uuplot) 
args$type <- "p"
args$pch <- 16

do.call("plot", args)

axis(side = 2, at = num, labels=compn, las=2, ...)
axis(side = 1, ...)
box()

if(alternative=="two.sided")
{
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lower[i], upper[i]), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = lower[i], pch="|", cex = args$cex*1.5)
  points(y = num[i], x = upper[i], pch="|", cex = args$cex*1.5)
  }
 }


if(alternative=="less")
{
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(llplot, upper[i]), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = upper[i], pch="|", cex = args$cex*1.5)
  }
 }


if(alternative=="greater")
{
 for(i in 1:length(num))
  {
  lines(y = c(num[i],num[i]), x = c(lower[i], uuplot), lty = CIlty, lwd=CIlwd)
  points(y = num[i], x = lower[i], pch="|", cex = args$cex*1.5)
  }
 }

abline(v=line, lty=linelty, lwd=linelwd, col=linecol)

}

if(HL){
par(old.par)
}

}

