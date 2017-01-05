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
confusionMatrix(data=prediction,testing$putative)

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
confusionMatrix(data = predicting, testing$putative)

plot(m2,metric="Accuracy")



