setwd("D:/Data/barplot")
# y <- 0:235
# barplot(ch[,2],space = 5,ylim = rev(range(y)),type="n",xlab = "",ylab = "",col="white")

ch <- read.table("D:/Data/barplot/mapdraw_geneticmaplength.txt")

plot(c(0,10),c(0,235),type="n",axes = FALSE,xlab = "",ylab = "")

rect(xleft=ch[,1]-1,xright=ch[,1]-0.9, ybottom=0,ytop = ch[,2],col="white")
chm <- read.csv("D:/Data/barplot/mapdraw_markerlocation.csv")

segments(x0=0.5,y0=0,x1=0.5,y1=10) # bar 10cM

#chromosome1
chm1 <- subset(chm,chromsome=="1")
head(chm1)
tail(chm1)
segments(x0=0,y0=chm1[,4],x1=0.1,y1=chm1[,4],col = "grey")
#qMHI1-1 Sanya
points(x=0.5,y=232.8, pch = -9668, col = "grey")
segments(x0=0.6,y0=221.1,x1=0.6,y1=233.8,col="red")
points(x=0.6,y=232.8, pch = -9668, col = "grey")

#chromosome2
chm2 <- subset(chm,chromsome=="2")
head(chm2)
tail(chm2)
segments(x0=1,y0=chm2[,4],x1=1.1,y1=chm2[,4],col = "grey")
#qMHI2-1 SJZ
points(x=1.5,y=17, pch = -9668, col = "grey")
segments(x0=1.6,y0=5.5,x1=1.6,y1=25.1,col="red")
#chromosome3
chm3 <- subset(chm,chromsome=="3")
head(chm3)
tail(chm3)
segments(x0=2,y0=chm3[,4],x1=2.1,y1=chm3[,4],col = "grey")

#qMHI3-1 SJZ
points(x=2.5,y=100, pch = -9668, col = "grey")
segments(x0=2.6,y0=88.9,x1=2.6,y1=103.2,col="red")
#chromosome4
chm4 <- subset(chm,chromsome=="4")
head(chm4)
tail(chm4)
segments(x0=3,y0=chm4[,4],x1=3.1,y1=chm4[,4],col = "grey")

#qMHI4-1 JN
points(x=3.5,y=43.5, pch = -9668, col = "grey")
segments(x0=3.6,y0=39.7,x1=3.6,y1=51.1,col="red")

#qMHI4-2 JN
points(x=3.5,y=103.7, pch = -9668, col = "grey")
segments(x0=3.6,y0=101.6,x1=3.6,y1=106.8,col="red")

#chromosome5
chm5 <- subset(chm,chromsome=="5")
head(chm5)
tail(chm5)
segments(x0=4,y0=chm5[,4],x1=4.1,y1=chm5[,4],col = "grey")

#qMHI5-1 SJZ
points(x=4.6,y=49.8, pch = -9668, col = "grey")
segments(x0=4.7,y0=39.1,x1=4.7,y1=51.1,col="red")
#chromosome6
chm6 <- subset(chm,chromsome=="6")
head(chm6)
tail(chm6)
segments(x0=5,y0=chm6[,4],x1=5.1,y1=chm6[,4],col = "grey")
#chromosome7
chm7 <- subset(chm,chromsome=="7")
head(chm7)
tail(chm7)
segments(x0=6,y0=chm7[,4],x1=6.1,y1=chm7[,4],col = "grey")
#chromosome8
chm8 <- subset(chm,chromsome=="8")
head(chm8)
tail(chm8)
segments(x0=7,y0=chm8[,4],x1=7.1,y1=chm8[,4],col = "grey")
#chromosome9
chm9 <- subset(chm,chromsome=="9")
head(chm9)
tail(chm9)
segments(x0=8,y0=chm9[,4],x1=8.1,y1=chm9[,4],col = "grey")
#qMHI9-1a Shunyi
points(x=8.3,y=21.6, pch = -9668, col = "grey")
segments(x0=8.4,y0=9.1,x1=8.4,y1=31.1,col="red")

#qMHI9-1b JN
points(x=8.4,y=31.1, pch = -9668, col = "grey")
segments(x0=8.5,y0=23.1,x1=8.5,y1=46.4,col="red")

#qMHI9-1c Shunyi
points(x=8.5,y=21.8, pch = -9668, col = "grey")
segments(x0=8.6,y0=15.7,x1=8.6,y1=23.3,col="red")

#chromosome10
chm10 <- subset(chm,chromsome=="10")
head(chm10)
tail(chm10)
segments(x0=9,y0=chm10[,4],x1=9.1,y1=chm10[,4],col = "grey")

#qMHI10-1 SJZ
points(x=9.5,y=78.3, pch = -9668, col = "grey")
segments(x0=9.6,y0=77.5,x1=9.6,y1=79.7,col="red")

#qMHI10-2a SJZ
points(x=9.5,y=67.8, pch = -9668, col = "grey")
segments(x0=9.6,y0=62.6,x1=9.6,y1=69,col="red")

#qMHI10-2b combine
points(x=9.6,y=142.9, pch = -9668,col="grey")
segments(x0=9.7,y0=132.6,x1=9.7,y1=150,col="red")
#qMHI10-2b combine
points(x=9.5,y=144.9, pch = -9668,col="grey")
segments(x0=9.6,y0=144.1,x1=9.6,y1=150,col="red")
#qMHI10-2b SJZ
points(x=9.7,y=144.9, pch = -9668,col="grey")
segments(x0=9.8,y0=144.1,x1=9.8,y1=150,col="red")

