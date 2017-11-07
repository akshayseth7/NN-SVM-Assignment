#####################################################################

#SVMfor Diabities dataset

#####################################################################

#Diabetes.csv

require(caTools)
require(e1071)

setwd("C:/Users/Akshay.Akshay-PC.000/Downloads/CI SVM")

# Input data
data <- read.csv("Diabetes.csv",header=TRUE)

# convert output class(target) into a factor
data$out <- as.factor(data$out)

# spliting the data into Train & Test
set.seed(2000)
check <- sample.split(data$out, SplitRatio=0.75)
train <- subset(data,check==TRUE)
test <- subset(data,check==FALSE)

# DEFAULT SVM Model 
set.seed(2000)
default <- svm(out~.,train)
pr <- predict(default,test)
table(pr,test$out)



#pr    0   1
#0 108  38
#1  17  29


# Polynomial SVM 
set.seed(2000)
poly <- svm(out~., train, kernel="polynomial")
pr <- predict(poly,test)
table(pr,test$out)


#pr    0   1
#0 115  50
#1  10  17

# Radial SVM 
set.seed(2000)
poly <- svm(out~., train, kernel="radial")
pr <- predict(poly,test)
table(pr,test$out)

#pr    0   1
#0 108  38
#1  17  29

set.seed(2001)
tune.out <- tune(svm,out~.,data=train,ranges=list(cost=c(0.00001,0.001,0.034,0.6,0.98,1.9,3,99)))
# we find that best cost paramters = 0.6
radial <- svm(out~., train, kernel="radial", cost=0.6)
pr <- predict(radial, test) 
table(pr,test$out)

#pr    0   1
#0 110  39
#1  15  28

