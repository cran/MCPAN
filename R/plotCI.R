"plotCII" <-
function(estimate, lower=NULL, upper=NULL, alternative=c("two.sided","less","greater"),
 lines=NULL, lineslty=2, lineslwd=1, linescol="black",
 CIvert=FALSE, CIlty = 1, CIlwd=1, CIcex=1, CIcol="black",
 HL=TRUE, ...)
{
if(HL){
old.par <- par(no.readonly=TRUE)
}
# estimate should be a named vector

args <- list(...)

k<-length(estimate)
num <- 1:k

if(is.null(names(estimate)))


 {compn <- paste("C", num, sep="")}
else{
  compn <- names(estimate)}

CIlty<-rep(CIlty, length.out=k)
CIlwd<-rep(CIlwd, length.out=k)
CIcol<-rep(CIcol, length.out=k)

mymai <- par("mai")

# define the plot range

llower<-lower
uupper<-upper

if(any(!is.finite(lower)))
 {
 cat("Infinite lower bound \n")
 allpoints<-c(lower, estimate, upper)
 llower<-min(allpoints[is.finite(allpoints)])
 }


if(any(!is.finite(upper)))
 {
 cat("Infinite upper bound \n")
 allpoints<-c(lower, estimate, upper)
 uupper<-max(allpoints[is.finite(allpoints)])
 }

#print(uupper)
#print(llower)


alternative <- match.arg(alternative)


# define the plot range in 
# dependence of the alternative:

switch(alternative,
"two.sided"={
   lplot <- min(llower, lines)
   uplot <- max(uupper, lines)
   },

"less"=
  {
  lplot <- min(lines, estimate) 
  uplot <- max(uupper, lines)
  },

"greater"=
  {

  lplot <- min(lines, llower) 
  uplot <- max(estimate, lines)

  })

# Define the final plot ranges:

 llplot <- lplot - 0.1*abs(lplot-uplot)
 uuplot <- uplot + 0.1*abs(lplot-uplot)

# define the type of interval drawn,
# appropriate for unbounded CI

switch(alternative,

"two.sided"={
code<-rep(3,k)

winfl<-which(!is.finite(lower))
lower[winfl]<-llplot
code[winfl]<-2

winfu<-which(!is.finite(upper))
upper[winfu]<-uuplot
code[winfu]<-1

winfts <- winfl %in% winfu
code[winfts]<-0
},

"less"={
code<-rep(2,k)

winfu<-which(!is.finite(upper))
upper[winfu]<-uuplot
code[winfu]<-0

},

"greater"={
code<-rep(1,k)

winfl<-which(!is.finite(lower))
lower[winfl]<-llplot
code[winfl]<-0
})


# Defien the defaults for main, sub, ylab, xlab:

if (is.null(args$main)) {args$main<-""} 
if (is.null(args$sub)) {args$sub<-""}
if (is.null(args$ylab)) {args$ylab<-""} 
if (is.null(args$xlab)) {args$xlab<-""}


# plot function for vertical CI:

if(CIvert==TRUE)
{

if(HL)
{
 plot.new()

 # adjust margin under the x axis according to length of comparison names

 xwidth<- 1.5 * max(strwidth(compn, units = "inches", cex = par("cex.axis"))) 

 if (mymai[1] < xwidth) 
        mymai[1] <- xwidth
 par(mai=mymai, new=TRUE)
}
args$x<-num
args$y<-estimate
args$axes<-FALSE

if(is.null(args$ylim))
{args$ylim<-c(llplot, uuplot)}

args$type="p"
args$pch=16
args$cex=CIcex

do.call("plot", args)

axis(side = 1, at = num, labels=compn, las=2, ... )
axis(side=2, las=2, ...)
box()

abline(v=num, col="lightgrey", lty=3)

abline(h=lines, lty=lineslty, lwd=lineslwd, col=linescol)

if(length(estimate)<25)
 {arrlength<-0.1}
else
 {arrlength<-0.05}

switch(alternative,

"two.sided"={

 for(i in 1:length(num))
  {
  arrows(x0=num[i], x1=num[i], y0=lower[i], y1=upper[i],
 length = arrlength, angle = 90, code = code[i],
       col = CIcol[i], lty = CIlty[i], lwd = CIlwd[i])

  }
 },

"less"={
 for(i in 1:length(num))
  {
  arrows(x0=num[i], x1=num[i], y0=llplot, y1=upper[i],
  length = arrlength, angle = 90, code = code[i],
       col = CIcol[i], lty = CIlty[i], lwd = CIlwd[i])
  }
 },

"greater"={

 for(i in 1:length(num))
  {
  arrows(x0=num[i], x1=num[i], y0=lower[i], y1=uuplot,
  length = arrlength, angle = 90, code = code[i],
       col = CIcol[i], lty = CIlty[i], lwd = CIlwd[i])
  }
 }
)


}


# plot function for horizontal CI:


if(CIvert==FALSE)
{
if(HL)
{
plot.new()

 # adjust margin under the x axis according to length of comparison names
  ywidth<- 1.5 * max(strwidth(compn, units = "inches", cex = par("cex.axis"))) 

 if (mymai[2] < ywidth) 
        mymai[2] <- ywidth
 par(mai=mymai, new=TRUE)
}

rnum<-rev(num)

args$y<-rnum
args$x<-estimate
args$axes<-FALSE

if(is.null(args$xlim))
{args$xlim<-c(llplot, uuplot)}

args$type<-"p"
args$pch<-16
args$cex<-CIcex

do.call("plot", args)

axis(side = 2, at = rnum, labels=compn, las=2, ...)
axis(side = 1, ...)
box()

abline(h=num, col="lightgrey", lty=3)

abline(v=lines, lty=lineslty, lwd=lineslwd, col=linescol)

if(length(estimate)<25)
 {arrlength<-0.1}
else
 {arrlength<-0.05}

switch(alternative,

"two.sided"={
 for(i in 1:length(num))
  {

  arrows(y0=rnum[i], y1=rnum[i], x0=lower[i], x1=upper[i],
 length = arrlength, angle = 90, code = code[i],
       col = CIcol[i], lty = CIlty[i], lwd = CIlwd[i])

  }
 },

"less"={
 for(i in 1:length(num))
  {
  arrows(y0=rnum[i], y1=rnum[i], x0=llplot, x1=upper[i],
  length = arrlength, angle = 90, code = code[i],
       col = CIcol[i], lty = CIlty[i], lwd = CIlwd[i])
  }
 },

"greater"={
 for(i in 1:length(num))
  {
  arrows(y0=rnum[i], y1=rnum[i], x0=lower[i], x1=uuplot,
  length = arrlength, angle = 90, code = code[i],
       col = CIcol[i], lty = CIlty[i], lwd = CIlwd[i])
  }
 })


}

if(HL){
par(old.par)
}

}

#plotCII(estimate=c(0,1,2,3), lower=c(-Inf, 0,1,2), upper=c(2,3,4,Inf))


#############################


plotCI<-function(x, ...){UseMethod("plotCI")}


#############################

plotCI.default<-function(x,...)
{

args<-list(...)

args$estimate<-x$estimate
args$lower<-x$conf.int[,1]
args$upper<-x$conf.int[,2]
args$alternative<-x$alternative

do.call("plotCII", args)

}

####


plotCI.sci<-function(x,...)
{
args<-list(...)
CI<-x$conf.int
cnames<-rownames(CI)

estimate<-as.numeric(x$estimate)
lower<-as.numeric(x$conf.int[,1])
upper<-as.numeric(x$conf.int[,2])

names(estimate)<-names(lower)<-names(upper)<-cnames

args$estimate<-estimate
args$lower<-lower
args$upper<-upper
args$alternative<-x$alternative

do.call(plotCII, args=args)

}





###########################################

plotCI.confint.glht<-function(x, ...)
{
args<-list(...)

CI<-x$confint
names<-rownames(CI)
estimate<-CI[,1]
names(estimate)<-names

args$estimate<-estimate
args$lower<-CI[,2]
args$upper<-CI[,3]
args$alternative<-x$alternative

do.call("plotCII", args=args)

}


# library(multcomp)

# amod <- aov(breaks ~ wool * tension, data = warpbreaks)
# wht <- glht(amod, linfct = mcp(tension = "Tukey"))
# CIwht<-confint(wht)
 
#windows()
#plotCI(CIwht, lines=c(-20,0,20), linescol=c("red","black","red"))

#windows()
#plot(CIwht)


#################################

plotCI.sci.ratio<-function(x, ...)
{
args<-list(...)

estimate<-as.numeric(x$estimate)
names(estimate)<-x$compnames
args$estimate<-estimate

CI<-x$conf.int

alternative<-x$alternative

switch(alternative,

"two.sided"={
args$lower<-CI[,1]
args$upper<-CI[,2]
},

"less"={
args$upper<-CI[,1]
},

"greater"={
args$lower<-CI[,1]
})

args$alternative<-alternative

do.call("plotCII", args=args)

}


#########################


plot.sci<-function(x,...)
{
args<-list(...)
CI<-x$conf.int
cnames<-rownames(CI)

estimate<-as.numeric(x$estimate)
lower<-as.numeric(x$conf.int[,1])
upper<-as.numeric(x$conf.int[,2])

names(estimate)<-names(lower)<-names(upper)<-cnames

args$estimate<-estimate
args$lower<-lower
args$upper<-upper
args$alternative<-x$alternative

do.call(plotCII, args=args)

}


plot.binomORci<-function(x,...)
{
args<-list(...)
CI<-x$conf.int
cnames<-rownames(CI)

estimate<-as.numeric(x$estimate)
lower<-as.numeric(x$conf.int[,1])
upper<-as.numeric(x$conf.int[,2])

names(estimate)<-names(lower)<-names(upper)<-cnames

args$estimate<-estimate
args$lower<-lower
args$upper<-upper
args$alternative<-x$alternative

do.call(plotCII, args=args)

}


plot.binomRRci<-function(x,...)
{
args<-list(...)
CI<-x$conf.int
cnames<-rownames(CI)

estimate<-as.numeric(x$estimate)
lower<-as.numeric(x$conf.int[,1])
upper<-as.numeric(x$conf.int[,2])

names(estimate)<-names(lower)<-names(upper)<-cnames

args$estimate<-estimate
args$lower<-lower
args$upper<-upper
args$alternative<-x$alternative

do.call(plotCII, args=args)

}



plot.binomRDci<-function(x,...)
{
args<-list(...)
CI<-x$conf.int
cnames<-rownames(CI)

estimate<-as.numeric(x$estimate)
lower<-as.numeric(x$conf.int[,1])
upper<-as.numeric(x$conf.int[,2])

names(estimate)<-names(lower)<-names(upper)<-cnames

args$estimate<-estimate
args$lower<-lower
args$upper<-upper
args$alternative<-x$alternative

do.call(plotCII, args=args)

}




plot.poly3ci<-function(x,...)
{
args<-list(...)
CI<-x$conf.int
cnames<-rownames(CI)

estimate<-as.numeric(x$estimate)
lower<-as.numeric(x$conf.int[,1])
upper<-as.numeric(x$conf.int[,2])

names(estimate)<-names(lower)<-names(upper)<-cnames

args$estimate<-estimate
args$lower<-lower
args$upper<-upper
args$alternative<-x$alternative

do.call(plotCII, args=args)

}




plot.Shannonci<-function(x,...)
{
args<-list(...)
CI<-x$conf.int
cnames<-rownames(CI)

estimate<-as.numeric(x$estimate)
lower<-as.numeric(x$conf.int[,1])
upper<-as.numeric(x$conf.int[,2])

names(estimate)<-names(lower)<-names(upper)<-cnames

args$estimate<-estimate
args$lower<-lower
args$upper<-upper
args$alternative<-x$alternative

do.call(plotCII, args=args)

}



plot.Simpsonci<-function(x,...)
{
args<-list(...)
CI<-x$conf.int
cnames<-rownames(CI)

estimate<-as.numeric(x$estimate)
lower<-as.numeric(x$conf.int[,1])
upper<-as.numeric(x$conf.int[,2])

names(estimate)<-names(lower)<-names(upper)<-cnames

args$estimate<-estimate
args$lower<-lower
args$upper<-upper
args$alternative<-x$alternative

do.call(plotCII, args=args)

}








#library(mratios)
#data(Penicillin)

#CGM<-sci.ratio(diameter~strain, data=Penicillin, type="GrandMean", alternative="greater")

#plotCI(CGM, lines=c(0.95,0.98,1,1/0.98, 1/0.95),
# lineslty=c(3,2,1,2,3), lineslwd=c(1,1,1,2,2), linescol="red",
#CIlwd=2)


#CGM<-sci.ratio(diameter~strain, data=Penicillin, type="GrandMean", alternative="less")

#pdf("CIplot1.pdf", width=8, height=5)
#plotCI(CGM, lines=c( 1/0.95),
# lineslty=2, lineslwd=2, linescol="red",
#CIlwd=2)
#dev.off()


#data(angina)

#CGM<-sci.ratio(response~dose, data=angina, type="Dunnett", alternative="two.sided")

#pdf("CIplot2.pdf", width=8, height=5)
#plotCI(CGM, lines=c(0.8, 1/0.8),
# lineslty=2, lineslwd=2, linescol="black",
#CIlwd=2,
#main="Ratio of means relative to the mean of the control group"
#)
#dev.off()





