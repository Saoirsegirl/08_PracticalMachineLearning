# 08_Identifying Proper Exercise Technique Using Accelerometer Data
Barb Dornseif - Saoirsegirl  
April 23, 2015  
# Summary:  
The following analysis will evaluate a data set taken from a set of accelerometers placed on the belt, forearm, arm, and dumbell of six (6) participants. These six participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions:  
    - Class A: exactly according to the specification
    - Class B: throwing the elbows to the front  
    - Class C: lifting the dumbbell only halfway  
    - Class D: lowering the dumbbell only halfway  
    _ Class E: throwing the hips to the front  
Class A is considered a correct execution of the excercise.  

From this data set, a series of model algorithms were run and it was detirmined that a Random Forrest model yielded the best accuracy in predicting the outcomes on the test set.

# Data Extraction, Formatting and Examinations
The data provided by the original study designers was made available at the [Weight Lifting Excercise summary page] (http://groupware.les.inf.puc-rio.br/static/WLE/WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv)  however for the class were were expected to use a shortened version for [training](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv ) and 20 random rows for [testing](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv).  

For purposes of this analysis the data was brought in as follows:

```r
library(caret)
setwd("~/Documents/Coursera/08-PracticalMachineLearning/08_Project")
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(trainURL, "./Data/training.csv", method = "curl")
download.file(testURL, "./Data/testing.csv", method = "curl")

# Load the data - iterations were run to detirmine final function variables
trainRaw <- read.table("./Data/training.csv", header = TRUE, sep = ",",
                    stringsAsFactors = FALSE)
```

You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

Rubric -- Do the authors describe what they expect the out of sample error to be and estimate the error appropriately with cross-validation?

## Experiment Description  
Participants were asked to perform one set of 10 repetitions
of the Unilateral Dumbbell Biceps Curl in five different fashions:
exactly according to the specification (Class A), throwing
the elbows to the front (Class B), lifting the dumbbell
only halfway (Class C), lowering the dumbbell only halfway
(Class D) and throwing the hips to the front (Class E). Class
A corresponds to the specified execution of the exercise,
while the other 4 classes correspond to common mistakes.  


Read more: http://groupware.les.inf.puc-rio.br/har#ixzz3YCIDMg00
Citations and Thanks - Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H.  "Qualitative Activity Recognition of Weight Lifting Exercises" Read more: http://groupware.les.inf.puc-rio.br/work.jsf?p1=11201#ixzz3YCGaaFml 
