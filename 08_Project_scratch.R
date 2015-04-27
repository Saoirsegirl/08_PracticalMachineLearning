library(caret) # pls

setwd("~/Documents/Coursera/08-PracticalMachineLearning/08_Project")
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(trainURL, "./Data/training.csv", method = "curl")
download.file(testURL, "./Data/testing.csv", method = "curl")

# Load the data - iterations were run to detirmine final function variables
trainRaw <- read.table("./Data/training.csv", header = TRUE, sep = ",",
                    stringsAsFactors = FALSE)


# first make a data.frame of the name, class, and existance of NA for each column
cNames <- names(trainRaw)
cClass <- as.character()
for (i in 1:dim(trainRaw)[2])  {cClass <- c(cClass, class(trainRaw[ , i]))}
hasNA <- as.character()
for (i in 1:dim(trainRaw)[2])  {
    hasNA <- c(hasNA, anyNA(trainRaw[ , i]))}
trainRaw_str <- as.data.frame(cbind(cNames = cNames, cClass = cClass, hasNA = hasNA),
                              stringsAsFactors = FALSE)

# now evaluate the metrics for suitability for use - in this case having no NA and being a metric
useCol <- as.numeric()
for (i in 8:159) {
    if (trainRaw_str[i, 2] != "character") {
        if (trainRaw_str[i, 3] == "FALSE") {
            useCol <- c(useCol, as.numeric(i))  } } }

# create the usable training set and a pre-test evaluator set
use <- trainRaw[ , c(160, useCol)]
use[ ,1] <- as.factor(use[ , 1])
set.seed <- 432
inTrain <- createDataPartition(y=use$classe, # vector of outcomes
                               p = 0.80, # Percentage to training
                               list=FALSE) 
training <- use[inTrain, ] 
compare <- use[ - inTrain, ]
summary(training)

# variable selection to reduce data set
    # find mean, median and sd for the metrics col 9:60
cMeans <- apply(training[2:53], 2, FUN = mean)
cStdDev <- apply(training[2:53], 2, FUN = sd)
evalSpread <- as.data.frame(cbind(cMeans, cStdDev, index = rep(1:52)))
par(mfrow = c(1,2))
qplot(cMeans, cStdDev, data = evalSpread)
qplot(index, cMeans, data = evalSpread)
par(mfrow = c(1,2))
plot(training$roll_belt, col=training$classe)

# fit, predict and cross-validate a Tree Model
fitBit_tree <- train(classe ~ . , method = "rpart", data = training)
print(fitBit_tree$finalModel)
# Visualize tree
par(mfrow = c(1,1))
plot(fitBit_tree$finalModel, uniform = TRUE)
text(fitBit_tree$finalModel, use.n = TRUE, all=TRUE, cex = 1)
predict_tree <- predict(fitBit_tree,newdata=compare)
confusionMatrix(predict_tree, compare$classe)
resample_tree <- print(fitBit_tree$resample)
hist(resample_tree[[ , 1])
     
)
# Accuracy .4922

# fit, predict and cross-validate a Random Forrest Model
fitBit_rf <- train(classe ~ ., data = training, method = "rf", prox=TRUE)
resample_rf <- print(fitBit_rf$resample)
# Visualize tree
par(mfrow = c(1,1))
#plot(fitBit_tree$finalModel, uniform = TRUE)
#text(fitBit_tree$finalModel, use.n = TRUE, all=TRUE, cex = 1)
predict_rf1 <- predict(fitBit_rf, newdata=compare)
confusionMatrix(predict_rf1, compare$classe)
# Accuracy = 0.9949
save(fitBit_rf, file = "fitBit_rf.rda")

predict_rf <- predict(fitBit_rf, newdata=training)
confusionMatrix(predict_rf2, training$classe)

# fit, predict and cross-validate a GBM Model
fitBit_gbm <- train(classe ~ ., method = 'gbm', data = training, verbose = FALSE)
resample_gbm <- print(fitBit_gbm$resample)
# Visualize tree
par(mfrow = c(1,1))
#plot(fitBit_tree$finalModel, uniform = TRUE)
#text(fitBit_tree$finalModel, use.n = TRUE, all=TRUE, cex = 1)
predict_gbm <- predict(fitBit_gbm, newdata=compare)
#qplot(predict(fitBit_gbm, compare),wage,data=compare[1])
confusionMatrix(predict_gbm, compare$classe)
# Accuracy = 0.9643

# Generate vector of predictions for submission into grading engine
# Apply all data procedures use on training set to the test set.
testRaw <- read.table("./Data/testing.csv", header = TRUE, sep = ",",
                       stringsAsFactors = FALSE)
use_test <- testRaw[ , c(160, useCol)]
use_test[ ,1] <- as.factor(use_test[ , 1])
str(use_test[ ,1])
predict_submit <- predict(fitBit_rf, newdata=use_test)
answers <- as.character(predict_submit)

# create files for submission
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(answers)
-----------------------