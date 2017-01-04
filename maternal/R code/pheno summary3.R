#the blup data were clipped
dta<- read.delim("clipboard",header=F)

library(vioplot)
library(sm)
head(dta)
boxplot(dta$Shunyi,dta$Jinan,dta$SJZ,dta$Hainan,
        names=c("Shunyi", "Jinan", "SJZ", "Hainan"), na.rm = TRUE)

x1<-dta$Shunyi
x2<-dta$Jinan
x3<-dta$SJZ
x4<-dta$Hainan

vioplot(na.omit(x1),na.omit(x2),na.omit(x3),na.omit(x4), names=c("Shunyi", "Jinan", "SJZ", "Hainan"), col="gold")
title("vilin plot of mhir in four environment")

## Changed panel.cor() function to show to lines of text - p-values and correlation coefficients.


## For panel.smooth() function defined cex=, col= and pch= arguments.
panel.smooth<-function (x, y, col = "blue", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}
## To add histograms, panel.hist() functions should be defined (taken from help file of pairs())
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

## Changed panel.cor() function to show to lines of text - p-values and correlation coefficients.
panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor.test(x,y)$estimate
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y,use=complete.obs)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.25, paste("r=",txt))
  text(.5, .75, Signif)
}


head(dta)

pairs(dta[,2:6],lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)


### histgram of the 4 environments BLUP data
par(mfrow=c(2,2))

hist(dta$Shunyi,xlab="mhir(%)",main="",ylim=c(0,50))
text(15,40,"Shunyi",font=2)
arrows(10.9,40,10.9,28,length=0.15,lwd=2)
text(10.9,42,"Chang7-2",font=2)
arrows(12.8,47,12.8,36,length=0.15,lwd=2)
text(12.8,50,"Qi319",font=2)

hist(dta$Jinan,xlab="mhir(%)",main="",ylim=c(0,35))
text(15,25,"Jinan",font=2)
arrows(9.6,23,9.6,14,length=0.15,lwd=2)
text(9.6,25,"Chang7-2",font=2)
arrows(12.8,33,12.8,26,length=0.15,lwd=2)
text(12.8,35,"Qi319",font=2)

hist(dta$SJZ,xlab="mhir(%)",main="",ylim=c(0,35))
text(16,30,"SJZ",font=2)
arrows(10.1,30,10.1,22,length=0.15,lwd=2)
text(10.1,32,"Chang7-2",font=2)
arrows(14.6,20,14.6,12,length=0.15,lwd=2)
text(14.6,22,"Qi319",font=2)

hist(dta$Hainan,xlab="mhir(%)",main="")
text(18,25,"Hainan",font=2)
arrows(11.1,20,11.1,11,length=0.15,lwd=2)
text(11.1,22,"Chang7-2",font=2)
arrows(15.8,30,15.8,22,length=0.15,lwd=2)
text(15.8,32,"Qi319",font=2)

dev.off()

hist(dta$env4,xlab="mhir(%)",main="")
text(16,34,paste("four locations","\n","combine"),font=2)
arrows(9.8,20,9.8,12,length=0.15,lwd=2)
text(9.8,22,"Chang7-2",font=2)
arrows(13.9,30,13.9,20,length=0.15,lwd=2)
text(13.9,32,"Qi319",font=2)


### histgram of the 4 environments the BLUE data
par(mfrow=c(2,2))

hist(dta$Shunyi,xlab="mhir(%)",main="",ylim=c(0,45))
text(17,35,"Shunyi",font=2)
arrows(10.3,39,10.3,32,length=0.15,lwd=2)
text(10.3,40,"Chang7-2",font=2)
arrows(12.8,42,12.8,32,length=0.15,lwd=2)
text(12.8,44,"Qi319",font=2)

hist(dta$Jinan,xlab="mhir(%)",main="",ylim=c(0,40))
text(16,30,"Jinan",font=2)
arrows(9.6,23,9.6,16,length=0.15,lwd=2)
text(9.6,25,"Chang7-2",font=2)
arrows(14.1,33,14.1,26,length=0.15,lwd=2)
text(14.1,35,"Qi319",font=2)

hist(dta$SJZ,xlab="mhir(%)",main="",ylim=c(0,35))
text(17,30,"SJZ",font=2)
arrows(9.7,20,9.7,12,length=0.15,lwd=2)
text(9.7,22,"Chang7-2",font=2)
arrows(16.1,23,16.1,16,length=0.15,lwd=2)
text(16.1,25,"Qi319",font=2)

hist(dta$Hainan,xlab="mhir(%)",main="",ylim=c(0,30))
text(19,25,"Hainan",font=2)
arrows(9.6,18,9.6,10,length=0.15,lwd=2)
text(9.6,20,"Chang7-2",font=2)
arrows(15.9,29,15.8,24,length=0.15,lwd=2)
text(15.9,30,"Qi319",font=2)

dev.off()

hist(dta$env4,xlab="mhir(%)",main="",ylim=c(0,20))
text(17,17,paste("four locations","\n","combine"),font=2)
arrows(9.7,12,9.7,5,length=0.15,lwd=2)
text(9.7,13,"Chang7-2",font=2)
arrows(14.8,19,14.8,13,length=0.15,lwd=2)
text(14.8,20,"Qi319",font=2)

