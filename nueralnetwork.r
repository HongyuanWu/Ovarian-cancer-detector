
data <- read.csv("OriginalDataSet.csv",TRUE,",") #import data
class(data)
data <- data[,1:8]

data <- na.omit(data)

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


#normalization(as nueral network is sensitive to scale)
data$Size_Of_Cancer <- (data$Size_Of_Cancer - min(data$Size_Of_Cancer))/(max(data$Size_Of_Cancer) - min(data$Size_Of_Cancer)) 
data$Mobility <- (data$Mobility -min(data$Mobility))/(max(data$Mobility) - min(data$Mobility)) 
data$Age <- (data$Age -min(data$Age))/(max(data$Age) - min(data$Age)) 
data$Surface <- (data$Surface -min(data$Surface))/(max(data$Surface) - min(data$Surface)) 
data$Consistency<- (data$Consistency -min(data$Consistency))/(max(data$Consistency) - min(data$Consistency)) 

#NEURAL NETWORK 
library(neuralnet) 
set.seed(0) 
#Neural Network

nn <- neuralnet(Vital_Status~Size_Of_Cancer+Mobility+Age+Surface+Consistency+trestbps+chol,data=traindata, hidden = 4, linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)

output <- compute(nn,testdata[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2<- table(pred2, testdata$Vital_Status) 
tab2
accuracy<- 1-sum(diag(tab2))/sum(tab2)
accuracy
