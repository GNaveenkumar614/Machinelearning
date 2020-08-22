---

title: "Practical Machine Learnig course project"
author: "Naveen kumar"
date: "8/21/2020"
output: md_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Loading the required libraries

```{r}
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
#install.packages('caret', dependencies = TRUE)
set.seed(125)
```

Getting the data
```{r}
trainurl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
```

Loading the data
```{r}
training <- read.csv(url(trainurl))
testing <- read.csv(url(testurl))
```

```{r}
dim(training)
dim(testing)
```
we will clean the data and get rid of observations with missing values as well as some unnecessary variables.
```{r}
sum(complete.cases(training))
training$classe <- as.factor(training$classe)  
```

Now,we remove columns the that contain NA missing values
```{r}
training <- training[, colSums(is.na(training)) == 0] 
testing <- testing[, colSums(is.na(testing)) == 0] 
```
 
 we also remove some columns like index,date etc,. for easy computation
```{r}
classe <- training$classe
trainRemove <- grepl("^X|timestamp|window", names(training))
training <- training[, !trainRemove]
trainupdated <- training[, sapply(training, is.numeric)]
trainupdated$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(testing))
testing <- testing[, !testRemove]
testupdated <- testing[, sapply(testing, is.numeric)]
```

```{r}
dim(trainupdated)
dim(testupdated)
```

we can split the cleaned training set into  training data set (70%) and a validation data set (30%). 
 
```{r}
Train <- createDataPartition(trainupdated$classe, p=0.70, list=F)
trainData <- trainupdated[Train, ]
testData <- trainupdated[-Train, ]
```

Training by RANDOM FOREST:

We fit a predictive model for activity recognition using **Random Forest** algorithm because it automatically selects important variables and is robust to correlated covariates & outliers in general.
We will use **5-fold cross validation** when applying the algorithm.  
```{r}
controlRf <- trainControl(method="cv", 5)
#install.packages('e1071', dependencies=TRUE)
```


```{r}
modelRf <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)
modelRf
```
Prediction:

```{r}
predictRf <- predict(modelRf, testData)
confusionMatrix(testData$classe, predictRf) #using confusion matrix to see the predictions
```
Accuracy:

```{r}
accuracy <- postResample(predictRf, testData$classe)
accuracy
```
So, the estimated accuracy of the model is 99.99% and the estimated out-of-sample error is 0.01%.

DECISION TREE:

we will train the model using decision tree
```{r}
modFitDC <- rpart(classe ~ ., data=trainData, method="class")
```

To view the decision tree with fancy to knnow how the data is segregated
```{r}
fancyRpartPlot(modFitDC)

```
Prediction:

```{r}
predictionDC <- predict(modFitDC,testData, type = "class")
```

Using confusion Matrix to test results
```{r}
confusionMatrix(predictionDC, testData$classe)
```
Twsting the model:

```{r}
testingPred <- predict(modelRf, testupdated)
testingPred
```

