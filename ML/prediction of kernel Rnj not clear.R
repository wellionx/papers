####load the mini data from priveous cleaning data
#### load the rdata ####
require(mgcv)
#save(maizex, file = "nir_cleandata_mini.rda")
load(file = "nir_cleandata_mini.rda") #file name "maizex" can read from the dataset 
head(maizex)

####get the xlab wavelength####
Rnjnotclear <- read.csv("Rnj not clear.csv",header = T)
maizernj <- merge(Rnjnotclear, maizex,by="kernel.name")

maizernjx <- maizernj[c(-1,-2,-3,-5)]

table(maizernjx$putative)
summary(maizernjx)
prop.table(table(maizernjx$putative)) * 100

predicting=predict(plsFit,maizernjx)
confusionMatrix(data = predicting, maizernjx$putative)