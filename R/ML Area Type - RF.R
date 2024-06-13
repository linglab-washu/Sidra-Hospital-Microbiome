library(caret)

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
# Random Forest
## Fit the model
set.seed(135)
rfAreaType <- train(Area.Type ~., data = data.train, method = "rf", trControl = train_control,  tuneGrid = expand.grid(mtry = seq(13, 15, length = 20)))
Sys.time()
rfAreaType$results$Accuracy[which(rfAreaType$results$mtry==rfAreaType$bestTune$mtry)]


## Test the model
rfAreaTypeTest <- predict(rfAreaType, newdata = x.test)
rfAreaTypeConfusionMatrix <- confusionMatrix(data = rfAreaTypeTest, y.test$Area.Type)
rfAreaTypeMCC <-mcc(rfAreaTypeTest,y.test$Area.Type)

rfAreaTypeTest.Area <- as.data.frame(model.matrix(~ .-1, data.frame(Area.Type = rfAreaTypeTest)))
rfAreaTypeMCC.Area <- list()
for (i in seq_along(rfAreaTypeTest.Area)) {
  rfAreaTypeMCC.Area[[i]] <- mcc(rfAreaTypeTest.Area[[i]],y.test.Area[[i]])
}


# #Save models
# save(rfAreaType, file = "./Data/rfAreaType.RData")
# save(rfAreaTypeTest, file = "./Data/rfAreaTypeTest.RData")
# save(rfAreaTypeConfusionMatrix, file = "./Data/rfAreaTypeConfusionMatrix.RData")
# save(rfAreaTypeMCC, file = "./Data/rfAreaTypeMCC.RData")
# save(rfAreaTypeMCC.Area, file = "./Data/rfAreaTypeMCC.Area.RData")
