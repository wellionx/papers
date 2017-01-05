setwd("E:/实验/数据/近红外大数据分析2016/nir data")

# Get the files names
files.name = list.files(pattern="*.csv")
files.length=length(files.name)
newdata=numeric(0)
for(i in 1:files.length){
  tmp=read.csv(files.name[i],head=T,sep=",")
  newdata=cbind(newdata,tmp[,2])} #A455 data not avaliable,delate the file
dim(newdata)

#extract the file names
library(tools)
# file_ext(files.name[1:files.length])
kernal.names <- file_path_sans_ext(files.name[1:files.length])
head(kernal.names)

colnames(newdata) <- kernal.names
rownames(newdata) <- paste0(tmp$Wavelength,"nm")

kernal.NIR <- t(newdata)

write.csv(newdata,"all nir data2.csv")

require(mgcv)
save(kernal.NIR, file = "nir_datall.rda")
#load(file = "nir_datall.rda")

str(kernal.NIR)

#load the kernel charcaters file 
setwd("E:/实验/数据/近红外大数据分析2016")
data2 <- read.csv("kernal characters.csv",stringsAsFactors = FALSE)
head(data2)
dim(data2)
kernal.class <- data2[-1]
rownames(kernal.class) <- data2$kernel.name

#merge the class and NIR data
maize.kernal.nir <- merge(kernal.class, kernal.NIR,by="row.names")
dim(maize.kernal.nir)
maize1 <- maize.kernal.nir[c(-1,-2,-4,-5,-6)]
head(maize1)
dim(maize1)
as.factor(maize1$putative)

#do a PLS on the kernel nir data
require(caret)
set.seed(2014)
inTrain=createDataPartition(y=maize1$putative,p=0.80,list=FALSE)
maize1train=maize1[inTrain,]
maize1test=maize1[-inTrain,]
#10 fold cv 5 times
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     classProbs = TRUE)
set.seed(1)
plsFitHap <- train(putative ~ .,
                    data = maize1train,
                    method = "pls",
                    tuneLength = 30,
                    trControl = ctrl,
                    preProc = c("center", "scale"))
plsFitHap
summary(plsFitHap)
plot(plsFitHap)


happut_predict=predict(plsFitHap,maize1test)
postResample(predict(plsFitHap,maize1test))
require(gmodels)
CrossTable(maize1test$putative,happut_predict)

set.seed(1)
ldaFitHap <- train(putative ~ .,
                   data = maize1train,
                   method = "lda",
                   trControl = ctrl,
                   preProc = c("center", "scale"))
ldaFitHap
plot(ldaFitHap)


#random forest
library(caret)
ctrl = trainControl(method = "cv",number = 5, repeats = 5)
grid_rf = expand.grid(.mtry = c(2, 4, 8, 16,22,38))
set.seed(300)
rfFitHap = train(putative ~ ., data = maize1train, method = "rf",
             metric = "Kappa", trControl = ctrl,
             tuneGrid = grid_rf)

plot(rfFitHap)

getTrainPerf(plsFitHap)
getTrainPerf(ldaFitHap)
getTrainPerf(rfFitHap)

postResample(predict(plsFitHap, maize1test), maize1test$putative)
