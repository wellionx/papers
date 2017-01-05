# open the training model emvironmet before

##### feature selection ####
## 自动feature selection

# caret包提供了Recursive Feature Elimination(RFE)
require(caret)
set.seed(2014)
control = rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm 
# class must be a factor
happred$putative <- as.factor(happred$putative)
str(PimaIndiansDiabetes)
str(happred)

results = rfe(happred[,2:125], happred[,1], sizes=c(1:125), rfeControl=control)

print(results)
# list the chosen features
predictors(results)
plot(results, type=c("g", "o"))

#### model combination ####
#### https://www.r-bloggers.com/an-intro-to-ensemble-learning-in-r/
#Adapted from the caret vignette
library("caret")
library("mlbench")
library("pROC")
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
)
library("rpart")
library("caretEnsemble")
model_list <- caretList(
  Class~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)
p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)
library("mlbench")
library("randomForest")
library("nnet")
model_list_big <- caretList(
  Class~., data=training,
  trControl=my_control,
  metric="ROC",
  methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
    nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
  )
)
greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
summary(greedy_ensemble)
