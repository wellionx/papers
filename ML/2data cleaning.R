#through the NIR plot found abnorml data point,remove it

#prepare the nir all data and kernel character data file

#read the nir_datall,put the R code in the same folder with files,open it with Rstudio firstly
setwd("E:/论文发表/近红外鉴别单倍体专利写作")

load(file = "nir_datall.rda")
head(kernal.NIR)
dim(kernal.NIR)

#### load the kernel charcaters file ####

data2 <- read.csv("kernel characters.csv",stringsAsFactors = FALSE)
head(data2)
kernal.classi <- data2[-1]
rownames(kernal.classi) <- data2$kernel.name

#merge the class and NIR data
maize.kernel.nir <- merge(kernal.classi, kernal.NIR,by="row.names")
dim(maize.kernel.nir)
head(maize.kernel.nir)
maize1 <- maize.kernel.nir[c(-1,-4,-5,-6)]
dim(maize1)
tmaize1 <- t(maize1)
dim(tmaize1)
####plot the data, and check it####
####get the xlab wavelength####
xdata <- read.table("wavelength.txt",header = T)
plot(xdata[1:125,1],tmaize1[4:128,1],type="l",col="red",xlab = "wavelength(nm)",ylab = "Percent",ylim=c(0,0.3))

#true haploid

for (i in 1:6597){
  if (tmaize1[2,i]=="haploid")
  lines(xdata[1:125,1],tmaize1[4:128,i],type="l",col="red")
}

#hybrid cross
for (i in 1:6597){
  if (tmaize1[2,i]=="hybridcross")
    lines(xdata[1:125,1],tmaize1[4:128,i],type="l",col="black")
}

#EH line
for (i in 1:6597){
  if (tmaize1[3,i]=="  1")
    lines(xdata[1:125,1],tmaize1[4:128,i],type="l",col="blue")
}

abline(h=0.09,v=1378)

####remove abnormal data set####
names(maize1.clean)
maize1.clean <- subset(maize1, maize1[,80] < 0.09) #abline(h=0.09,v=1378)
maize1.clean <- maize1.clean[!(maize1.clean$putative=="haploid" & maize1.clean[,53] > 0.1 ),] #abline(h=0.09,v=1205)
maize1.clean <- maize1.clean[!(maize1.clean$putative=="hybridcross"& maize1.clean[,100] >0.04 ),] #abline(h=0.04,v=1500)
maize1.clean <- maize1.clean[!(maize1.clean$putative=="hybridcross"& maize1.clean[,19] >0.17 ),] #abline(h=0.04,v=1500)
maize1.clean <- maize1.clean[!(maize1.clean$putative=="hybridcross"& maize1.clean[,35] < 0.025 ),] #abline(h=0.04,v=1500)

colnames(maize1.clean)
dim(maize1.clean)
tmaize1 <- t(maize1.clean)
dim(tmaize1)
#plot the data, and check it
#get the xlab wavelength
xdata <- read.table("wavelength.txt",header = T)
plot(xdata[1:125,1],tmaize1[4:128,1],type="l",col="red",xlab = "wavelength(nm)",ylab = "Percent",ylim=c(0,0.2))

#true haploid

for (i in 1:6578){
  if (tmaize1[2,i]=="haploid")
    lines(xdata[1:125,1],tmaize1[4:128,i],type="l",col="red")
}
# abline(h=0.09,v=1205)
#hybrid cross
for (i in 1:6582){
  if (tmaize1[2,i]=="hybridcross")
    lines(xdata[1:125,1],tmaize1[4:128,i],type="l",col="black")
}
# abline(h=0.025,v=1100)
#EH line
for (i in 1:6582){
  if (tmaize1[3,i]=="  1")
    lines(xdata[1:125,1],tmaize1[4:128,i],type="l",col="blue")
}
kernal.NIR.clean <- maize1.clean
require(mgcv)
save(kernal.NIR.clean, file = "nir_cleandata.rda")
#load(file = "nir_cleandata.rda")

kernal.class2 <- read.csv("maize_minidata_names.csv")

maizex <- merge(kernal.class2, kernal.NIR.clean,by="kernel.name")
dim(maizex)
head(maizex)

##### plot the maize mini data ####
maizext <- t(maizex[-2])
dim(maizext)
#get the xlab wavelength
xdata <- read.table("wavelength.txt",header = T)
plot(xdata[1:125,1],maizext[4:128,1],type="l",col="red",xlab = "wavelength(nm)",ylab = "Percent",ylim=c(0,0.2))
legend("topright",col = c("deeppink", "deepskyblue"),lwd=2,
       lty = c(1,1),legend = c("单倍体", "杂合二倍体"),bty = "n")
#true haploid

for (i in 1:1863){
  if (maizext[2,i]=="haploid")
    lines(xdata[1:125,1],maizext[4:128,i],type="l",col="deeppink")
}

#hybrid cross
for (i in 1:1863){
  if (maizext[2,i]=="hybridcross")
    lines(xdata[1:125,1],maizext[4:128,i],type="l",col="deepskyblue")
}

abline(h=0.04,v=1500)
# remove abnormal
names(maizex)
maizex <- maizex[!(maizex[,101] > 0.04 ),] #abline(h=0.04,v=1500)

#### save the rdata ####
require(mgcv)
save(maizex, file = "nir_cleandata_mini.rda")
#load(file = "nir_cleandata_mini.rda")