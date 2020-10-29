
data <- read.csv("OriginalDataSet.csv",TRUE,",") #import data
class(data)
data <- data[,1:8]

#select rows for train and test data
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)

# Build X_train, y_train, X_test, y_test (x-features, y-target columns)
traindata=data[train_index,]
X_train <- data[train_index, -8]
y_train <- data[train_index, "Vital_Status"]

testdata=data[test_index,]
X_test <- data[test_index, -8]
y_test <- data[test_index, "Vital_Status"]

#import libraries (to plot and for svm)
library(ggplot2)
library(kernlab)

inssvp=ksvm(Vital_Status~Size_Of_Cancer+Mobility,data=traindata,type="C-svc",C=10, kernel="polydot")
plot(inssvp, data=traindata) 
accuracy<-sum(testdata$Vital_Status==predict(inssvp,testdata))/nrow(testdata)
accuracy


