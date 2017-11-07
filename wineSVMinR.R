#####################################################################

#SVMfor Wine dataset

#####################################################################

#wine.csv

require(caTools)
require(e1071)

setwd("C:/Users/Akshay.Akshay-PC.000/Downloads/CI SVM")

# Input Data
data <- read.csv("winequality-white.csv",header=TRUE)


# convert output class(target) into a factor
data$quality <- as.factor(data$quality)

# spliting data set into Train & Test
set.seed(2000)
check <- sample.split(data$quality, SplitRatio=0.75)
train <- subset(data,check==TRUE)
test <- subset(data,check==FALSE)

# DEFAULT SVM Model 
set.seed(2000)
default <- svm(quality~.,train)
pr <- predict(default,test)
table(pr,test$quality)

#pr    3   4   5   6   7   8   9
#3   0   0   0   0   0   0   0
#4   0   2   1   0   0   0   0
#5   2  27 220 113   9   0   0
#6   3  12 143 410 165  36   1
#7   0   0   0  27  46   8   0
#8   0   0   0   0   0   0   0
#9   0   0   0   0   0   0   0


# Polynomial SVM 
set.seed(2000)
poly <- svm(quality~., train, kernel="polynomial")
pr <- predict(poly,test)
table(pr,test$quality)

#pr    3   4   5   6   7   8   9
#3   1   0   0   0   0   0   0
#4   0   5   2   1   0   0   0
#5   2  19 138  70   1   0   0
#6   2  17 223 468 199  37   1
#7   0   0   1  11  19   7   0
#8   0   0   0   0   1   0   0
#9   0   0   0   0   0   0   0

# radial SVM 
set.seed(2000)
poly <- svm(quality~., train, kernel="radial")
pr <- predict(poly,test)
table(pr,test$quality)

#pr    3   4   5   6   7   8   9
#3   0   0   0   0   0   0   0
#4   0   2   1   0   0   0   0
#5   2  27 220 113   9   0   0
#6   3  12 143 410 165  36   1
#7   0   0   0  27  46   8   0
#8   0   0   0   0   0   0   0
#9   0   0   0   0   0   0   0


set.seed(2001)
tune.out <- tune(svm,quality~.,data=train, kernel="radial",ranges=list(cost=c(0.00001,0.001,0.034,0.6,0.98,1.9,3,99)))
# we find that best cost paramters = 99
radial <- svm(quality~., train, kernel="radial", cost=99)
pr <- predict(radial, test) 
table(pr,test$quality)

#pr    3   4   5   6   7   8   9
#3   0   0   1   0   0   1   0
#4   1   9   5   7   0   0   0
#5   4  19 220  99   3   2   0
#6   0  11 130 383 106  10   1
#7   0   0   5  58 103  16   0
#8   0   2   3   3   8  15   0
#9   0   0   0   0   0   0   0

