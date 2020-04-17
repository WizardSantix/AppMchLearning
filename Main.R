require(caret)
require(randomForest)
require(parallel)
require(doParallel)
set.seed(2128)

#Load the archives
training <- read.csv("./pml-training.csv", na.strings=c("#DIV/0!") )
training[,8:ncol(training)-1]<-sapply(training[,8:ncol(training)-1], function(x) as.numeric(as.character(x)))
finaltesting <- read.csv("./pml-testing.csv")
#finaltesting[,8:ncol(finaltesting)-1]<-sapply(finaltesting[,8:ncol(finaltesting)], function(x) as.numeric(as.character(x)))

#Find NA columns
na_count_train <-sapply(training, function(y) sum(length(which(is.na(y)))))
na_count_test <-sapply(finaltesting, function(y) sum(length(which(is.na(y)))))
na_traintest<- rbind(na_count_train,na_count_test)

#Filter NA columns from both train and test sets
trainingnames <- colnames(training[colSums(is.na(training)) == 0])[-(1:7)]
ftesting <- finaltesting[trainingnames[1:length(trainingnames)-1]]
#fna <-sapply(ftesting, function(y) sum(length(which(is.na(y))))) #NA remaining in the test set
#trainingnames<-trainingnames[-which(fna==20)]
ftraining <- training[trainingnames]
#ftesting <- finaltesting[trainingnames[1:length(trainingnames)-1]] #Updates ftesting
#ftesting <- cbind(ftesting,finaltesting$problem_id)

#Partition dataset into test and train data
workdata <- createDataPartition(y=ftraining$classe, p=3/4, list=FALSE )
traindata <- ftraining[workdata,]
testdata <- ftraining[-workdata,]

#Fit random forest predictor
#model<-randomForest(x=traindata[-ncol(traindata)],y=traindata$classe,ntree=500)
fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
model<- train(x=traindata[-ncol(traindata)],y=traindata$classe,method="rf",preProcess = "pca", trControl= fitControl)

#Check accuracy of model fit with the test data
predictions <- predict(model, newdata=testdata)
confusionMatrix(predictions,testdata$classe)
