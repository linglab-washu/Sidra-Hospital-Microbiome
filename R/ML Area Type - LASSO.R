library(caret)
load("./Data/x.after.log.rel.abund.RData")
load("./Data/sampleDataAreaType.RData")

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

parameters <- 10^seq(-3, 3, length = 100)

set.seed(1996)
train_rows <- createDataPartition(sampleDataAreaType$Area.Type, p = .8, 
                                  list = FALSE, 
                                  times = 1)
x.train <- x.after.log.rel.abund[train_rows, ]
x.test <- x.after.log.rel.abund[-train_rows, ]

y.Area.Type <- as.data.frame(model.matrix(~ .-1, sampleDataAreaType))
names(y.Area.Type)<-make.names(names(y.Area.Type),unique = TRUE)
y.Area.Type <- data.frame(lapply(y.Area.Type,factor))

y.AreaName <- c()
lassoAreaType <- list()
lassoAreaTypeTest <- list()
lassoAreaTypeConfusionMatrix <- list()
lassoAreaTypeMCC <- list()
for (i in seq_along(y.Area.Type)) {
  y.AreaName <- c(y.AreaName, substring(colnames(y.Area.Type)[i],10))
  y <- dplyr::select(y.Area.Type, colnames(y.Area.Type)[i])
  y.train <- y[train_rows,]
  y.test <- y[-train_rows,]
  
  Sys.time()
  # Fit the model
  set.seed(135)
  lassoAreaType[[i]] <- train(
    x = as.data.frame(x.train),
    y = y.train ,
    method = "glmnet",
    trControl = train_control,
    tuneGrid = expand.grid(
      alpha = 1,
      lambda = parameters)
    )
  Sys.time()
  
  # Test the model
  lassoAreaTypeTest[[i]] <- predict(lassoAreaType[[i]], newdata = x.test)
  lassoAreaTypeConfusionMatrix[[i]] <- confusionMatrix(data = lassoAreaTypeTest[[i]], y.test)
  lassoAreaTypeMCC[[i]] <-mcc(lassoAreaTypeTest[[i]],factor(y.test))
}


# #Save models
# save(lassoAreaType, file = "./Data/lassoAreaType.RData")
# save(lassoAreaTypeTest, file = "./Data/lassoAreaTypeTest.RData")
# save(lassoAreaTypeConfusionMatrix, file = "./Data/lassoAreaTypeConfusionMatrix.RData")
# save(lassoAreaTypeMCC, file = "./Data/lassoAreaTypeMCC.RData")
