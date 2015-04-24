library(caret)

setwd("~/Documents/Coursera/08-PracticalMachineLearning/08_Project")
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(trainURL, "./Data/training.csv", method = "curl")
download.file(testURL, "./Data/testing.csv", method = "curl")
