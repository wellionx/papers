####load the mini data from priveous cleaning data
#### load the rdata ####
require(mgcv)
#save(maizex, file = "nir_cleandata_mini.rda")
load(file = "nir_cleandata_mini.rda") #file name "maizex" can read from the dataset 
head(maizex)
happred <- maizex[c(-1,-2,-4)]

#### Data exploration ####
str(happred)
dim(happred)
table(happred$putative)
summary(happred)
prop.table(table(happred$putative)) * 100

#### dataset for training and test ####
require(caret)
set.seed(2014)
inTrain=createDataPartition(y=happred$putative,p=0.80,list=FALSE)
training=happred[inTrain,]
testing=happred[-inTrain,]

#### traincontrol 10 fold cv 5 times ####
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     classProbs = TRUE)
#### PLS method ####
set.seed(1)
plsFit <- train(putative ~ .,
                   data = training,
                   method = "pls",
                   tuneLength = 30, #change the number see the best one
                   trControl = ctrl,
                   preProc = c("center", "scale"))
plsFit
summary(plsFit)
plot(plsFit)

predicting=predict(plsFit,testing)
pls.cmx <- confusionMatrix(data = predicting, testing$putative)
#rpart with tree visualization
require(rpart)
require(rpart.plot)
require(partykit)
treemodel <- rpart(putative~. , data=training)
summary(treemodel)
str(treemodel)
plot(treemodel)
text(treemodel)
prp(treemodel)  				
prp(treemodel,varlen=100)	

plotcp(treemodel)	
#Predict using the decision tree
prediction <- predict(treemodel, newdata=testing, type='class' )
#Use contingency table to see how accurate it is
table(prediction, testing$putative)
rpart.cmx<-confusionMatrix(data=prediction,testing$putative)
#### Decision Trees ####
#C50
library(caret)
set.seed(300)
m = train(putative ~ ., data = training, method = "C5.0")

ctrl = trainControl(method = "cv", number = 10,selectionFunction = "oneSE")
grid = expand.grid(.model = "tree",
                   .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                   .winnow = FALSE)
set.seed(300)
m2 = train(putative ~ ., data = training, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
predicting=predict(m2,testing)
c50.cmx<-confusionMatrix(data = predicting, testing$putative)

plot(m2,metric="Accuracy")

#### Gradient Boosting Decision Tree ####
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1, 
                        n.minobsinnode = 20)
set.seed(825)
gbmFit <- train(putative ~ .,
                data = training,
                method = "gbm",
                trControl = ctrl,
                verbose = FALSE,
                tuneGrid = gbmGrid,
                metric = "ROC")
plot(gbmFit)
ggplot(gbmFit)
predicting=predict(gbmFit,testing)
gbm.cmx<-confusionMatrix(data = predicting, testing$putative)
#### 几何模型，借助几何概（平面，距离）念构建实例空间
#### support vector machine model ####
set.seed(825)
svmFit <- train(putative ~ ., data = training, 
                method = "svmRadial", 
                trControl = ctrl, 
                preProc = c("center", "scale"),
                tuneLength = 30,
                metric = "ROC")
require(mgcv)
save(svmFit, file = "SVM_trainmodel.rda")
#load(file = "SVM_trainmodel.rda")
svmFit  
plot(svmFit,xlim = c(0,4200))
predicting=predict(svmFit,testing)
svm.cmx<-confusionMatrix(data = predicting, testing$putative)

#boosting svm
bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)
set.seed(300)
svmbaging = train(putative ~ ., data = training, 
               method="bag",trControl = ctrl, bagControl = bagctrl)
svmbaging
#### Neural Network ####
set.seed(2)
nnFit <- train(putative ~ ., data = training, 
                method = "nnet", 
                trControl = ctrl, 
                preProc = c("center", "scale"),
                tuneLength = 30,
                metric = "ROC")
nnFit  
plot(nnFit)

predicting=predict(nnFit,testing)
nnet.cmx<-confusionMatrix(data = predicting, testing$putative)

#### kNN method ####
set.seed(400)
knnFit <- train(putative ~ ., data = training, 
                method = "knn", trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneLength = 30,
                metric = "ROC")
#### save the model result ####
require(mgcv)
save(knnFit, file = "KNN_trainmodel.rda")
#load(file = "KNN_trainmodel.rda")

plot(knnFit)
plot(knnFit, metric = "Kappa")
knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
knn.cmx <- confusionMatrix(knnPredict, testing$putative )

#### Naive Bayes model####
set.seed(400)
naibyFit <- train(putative ~ ., data = training, 
                method = "nb", trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneLength = 30,
                metric = "ROC")
require(mgcv)
save(naibyFit, file = "NaiveBayes_trainmodel.rda")
#load(file = "NaiveBayes_trainmodel.rda")
plot(naibyFit)
predicting=predict(naibyFit,testing)
nb.cmx <- confusionMatrix(data = predicting, testing$putative)

#### Random forrest ####
rfFit <- train(putative ~ ., data = training, 
               method = "rf", trControl = ctrl, 
               preProcess = c("center","scale"), 
               tuneLength = 30,
               metric = "ROC")
plot(rfFit)
rfPredict <- predict(rfFit,newdata = testing )
rf.cmx <- confusionMatrix(rfPredict, testing$putative )

#### Between-Models ####
# make statistical statements about their performance differences
resamps <- resamples(list(GBM = gbmFit,
                          SVM = svmFit,
                          RF = rfFit,
                          PLS = plsFit,
                          NNet = nnFit,
                          KNN = knnFit,
                          NaiveBayes=naibyFit))
resamps

summary(resamps)
str(resamps)
#There are several lattice plot methods that can be used to 
#visualize the resampling distributions: 
#density plots, box-whisker plots, 
#scatterplot matrices and scatterplots of summary statistics
trellis.par.set(theme1) 
bwplot(resamps, layout = c(2, 1), border = T)

trellis.par.set(caretTheme())
dotplot(resamps, metric = "Accuracy")

##compare performance statistics from each of the models ####
# Here I'm setting up a matrix to host some performance statistics from each of the models
cmatrix.metrics = data.frame(model = c("naiby", "knn", "rpart","c50", "svm","gbm","rf","pls","nnet"), 
                             Accuracy = rep(NA,9),kappa = rep(NA,9), sensitivity = rep(NA,9), specificity = rep(NA,9))

# comfusionMatix result to a list of 9
cmx = list(nb.cmx,knn.cmx,rpart.cmx,c50.cmx,svm.cmx,gbm.cmx,rf.cmx,pls.cmx,nnet.cmx)
cmx[[1]]

# For each of the models, I use confusionMatrix to give me the performance statistics that I want
for (i in 1:9) {
  cmatrix.metrics[i,"Accuracy"] = cmx[[i]]$overall[1][[1]]
  cmatrix.metrics[i,"kappa"] = cmx[[i]]$overall[2][[1]]
  cmatrix.metrics[i, "sensitivity"] = cmx[[i]]$byClass[1]
  cmatrix.metrics[i, "specificity"] = cmx[[i]]$byClass[2]
}


# Now I transform my cmatrix.metrics matrix into a long format suitable for ggplot, graph it, and then post it to plot.ly
library(reshape)
library(ggplot)
cmatrix.long = melt(cmatrix.metrics, id.vars=1, measure.vars=c(2:5))

ggplot(cmatrix.long, aes(x=model, y=value, fill=variable,colour=variable)) + 
  geom_point(stat="identity", size=3) + 
  facet_grid(~variable) + 
  theme(axis.text.x=element_text(angle=90,vjust=0.5, colour="black",size=12), 
        axis.text.y=element_text(colour="black",size=12), 
        strip.text=element_text(size=12), axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_text(size=14, face="bold")) + 
  ggtitle("Performance Stats for Contraceptive Choice Models") + 
  scale_x_discrete("Model") + 
  scale_y_continuous("Value")
                                                                                                                                 
                                                                                                                                 

#### Systme runtime comparison####
system.time()
