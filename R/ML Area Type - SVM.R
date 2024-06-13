library(caret)
library(mltools)
load("./Data/x.after.log.rel.abund.RData")
load("./Data/sampleDataAreaType.RData")

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

set.seed(1996)
train_rows <- createDataPartition(sampleDataAreaType$Area.Type, p = .8, 
                                  list = FALSE, 
                                  times = 1)
x.train <- x.after.log.rel.abund[train_rows, ]
x.test <- x.after.log.rel.abund[-train_rows, ]

y.train <- data.frame(Area.Type = sampleDataAreaType[train_rows,])
y.test <- data.frame(Area.Type = sampleDataAreaType[-train_rows,])

y.test.Area <- as.data.frame(model.matrix(~ .-1, y.test))


data.train <- cbind(x.train,y.train)

Sys.time()
# Linear SVM
## Fit the model
set.seed(135)
svmLinearAreaType <- train(Area.Type ~., data = data.train, method = "svmLinear", trControl = train_control,  tuneGrid = expand.grid(C = seq(0, 1, length = 20)))
svmLinearAreaType$results$Accuracy[which(svmLinearAreaType$results$C==svmLinearAreaType$bestTune$C)]
Sys.time()

## Test the model
svmLinearAreaTypeTest <- predict(svmLinearAreaType, newdata = x.test)
svmLinearAreaTypeConfusionMatrix <- confusionMatrix(data = svmLinearAreaTypeTest, y.test$Area.Type)
svmLinearAreaTypeMCC <-mcc(svmLinearAreaTypeTest,y.test$Area.Type)
  
svmLinearAreaTypeTest.Area <- as.data.frame(model.matrix(~ .-1, data.frame(Area.Type = svmLinearAreaTypeTest)))
svmLinearAreaTypeMCC.Area <- list()
for (i in seq_along(svmLinearAreaTypeTest.Area)) {
  svmLinearAreaTypeMCC.Area[[i]] <- mcc(svmLinearAreaTypeTest.Area[[i]],y.test.Area[[i]])
}


#Radial SVM
## Fit the model
set.seed(135)
svmRadialAreaType <- train(Area.Type ~., data = data.train, method = "svmRadial", trControl = train_control, tuneLength = 10)
svmRadialAreaType$results$Accuracy[which(svmRadialAreaType$results$C==svmRadialAreaType$bestTune$C)]
Sys.time()

## Test the model
svmRadialAreaTypeTest <- predict(svmRadialAreaType, newdata = x.test)
svmRadialAreaTypeConfusionMatrix <- confusionMatrix(data = svmRadialAreaTypeTest, y.test$Area.Type)
svmRadialAreaTypeMCC <-mcc(svmRadialAreaTypeTest,y.test$Area.Type)

svmRadialAreaTypeTest.Area <- as.data.frame(model.matrix(~ .-1, data.frame(Area.Type = svmRadialAreaTypeTest)))
svmRadialAreaTypeMCC.Area <- list()
for (i in seq_along(svmRadialAreaTypeTest.Area)) {
  svmRadialAreaTypeMCC.Area[[i]] <- mcc(svmRadialAreaTypeTest.Area[[i]],y.test.Area[[i]])
}


# #Save models
# save(svmLinearAreaType, file = "./Data/svmLinearAreaType.RData")
# save(svmLinearAreaTypeTest, file = "./Data/svmLinearAreaTypeTest.RData")
# save(svmLinearAreaTypeConfusionMatrix, file = "./Data/svmLinearAreaTypeConfusionMatrix.RData")
# save(svmLinearAreaTypeMCC, file = "./Data/svmLinearAreaTypeMCC.RData")
# save(svmLinearAreaTypeMCC.Area, file = "./Data/svmLinearAreaTypeMCC.Area.RData")
# 
# save(svmRadialAreaType, file = "./Data/svmRadialAreaType.RData")
# save(svmRadialAreaTypeTest, file = "./Data/svmRadialAreaTypeTest.RData")
# save(svmRadialAreaTypeConfusionMatrix, file = "./Data/svmRadialAreaTypeConfusionMatrix.RData")
# save(svmRadialAreaTypeMCC, file = "./Data/svmRadialAreaTypeMCC.RData")
# save(svmRadialAreaTypeMCC.Area, file = "./Data/svmRadialAreaTypeMCC.Area.RData")


