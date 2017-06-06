# demo ####
library(chemometrics)
data(glass)
data(glass.grp)
x=glass[,c(2,7)]
require(robustbase)
res <- Moutlier(glass,quantile=0.975,pch=glass.grp)

set.seed(134)
x <- cbind(rnorm(80), rnorm(80), rnorm(80))
y <- cbind(rnorm(10, 5, 1), rnorm(10, 5, 1), rnorm(10, 5, 1))
z <- rbind(x,y)
# execute:
library(mvoutlier)
aq.plot(z, alpha=0.1)


# real data ####
require(mgcv)
load(file="maize1.rda") # original data of maize1
maize2 <- maize1[-c(1,2,3)]
#### plot the nir spectra####
library(prospectr)
wavelength <- read.table("wavelength.txt",header = T)
matplot(wavelength,t(maize2),type='l',col = "red",
        xlab='Wavelength /nm ',ylab='Reflectance')
# matlines(wavelength,t(maize2),type='l',col = "blue",
#          xlab='Wavelength /nm ',ylab='Reflectance')
axis(1,at=seq(950,1650,by=50),lty=5)
mtext('Raw spectra')
# outlier detection ####
res <- aq.plot(maize2, alpha=0.05)
res <- Moutlier(maize2,quantile=0.995)
mhd <- res$rd

# evaluate the distance ####
mhdd <- mhd - mean(res$rd) < 3*sd(res$rd)
maize3 <- maize2[mhdd,]
maize4 <- maize2[!mhdd,]
#### plot the nir spectra####
library(prospectr)
wavelength <- read.table("wavelength.txt",header = T)
matplot(wavelength,t(maize3),type='l',col = "red",
        xlab='Wavelength /nm ',ylab='Reflectance')
# matlines(wavelength,t(maize2),type='l',col = "blue",
#          xlab='Wavelength /nm ',ylab='Reflectance')
axis(1,at=seq(950,1650,by=50),lty=5)
mtext('spectra after')

# outlier ####
matplot(wavelength,t(maize4),type='l',col = "red",
        xlab='Wavelength /nm ',ylab='Reflectance')
