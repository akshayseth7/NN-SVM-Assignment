#install package for SVM
install.packages("e1071")
#install package for spliting of data
install.packages("caTools")
install.packages('caret')
require("caret")
#loading library
require("caTools")
require(e1071)
#Reading Data from the computer memory by passing the path
data<-read.csv("~/Documents/wine.csv", header = TRUE)
#setting the target field
data$quality<-as.factor(data$quality)
str(data) #dispalying the details of the data
set.seed(1000) #setting seed value
#spliting the data in to training and testing set 75:25
check<-sample.split(data$quality,SplitRatio=0.75)
# allocation of training data
train<-subset(data, check==TRUE)
#Allocation of Testing data
test<-subset(data,check==FALSE)
#SVM model development
model<-svm(quality~.,train)
#Testing of model
pred<-predict(model,test)
#Displaying the result
table(pred,test$quality)
#OUTPUT
#  pred   3   4   5   6   7   8   9
#     3   0   0   0   0   0   0   0
#     4   0   2   2   0   0   0   0
#     5   3  24 211  90   8   0   0
#     6   2  14 151 430 159  28   1
#     7   0   1   0  30  53  16   0
#     8   0   0   0   0   0   0   0
#     9   0   0   0   0   0   0   0
xtab <- table(true=test$quality, predicted=pred)


#Creating confusion Matrix
confusionMatrix(xtab)


#LINEAR KERNEL
set.seed(1002)
lin<-svm(quality~.,train,kernel="linear")
pred<-predict(lin,test)
table(pred,test$quality)

xtab <- table(true=test$quality, predicted=pred)


#Creating confusion Matrix
confusionMatrix(xtab)


#OUTPUT

#   pred   3   4   5   6   7   8   9
#      3   0   0   0   0   0   0   0
#      4   0   0   0   0   0   0   0
#      5   2  24 200  96   9   4   0
#      6   3  17 164 454 211  40   1
#      7   0   0   0   0   0   0   0
#      8   0   0   0   0   0   0   0
#      9   0   0   0   0   0   0   0

#POLYNOMIAL KERNEL
set.seed(1002)
poly=svm(quality~.,train,kernel="polynomial")
pred<-predict(poly,test)
table(pred,test$quality)
xtab <- table(true=test$quality, predicted=pred)


#Creating confusion Matrix
confusionMatrix(xtab)


#OUTPUT


#     pred   3   4   5   6   7   8   9
#        3   0   1   0   0   0   0   0
#        4   0   3   2   1   0   0   0
#        5   2  22 139  47   3   0   0
#        6   3  14 223 488 181  35   1
#        7   0   1   0  14  35   8   0
#        8   0   0   0   0   1   1   0
#        9   0   0   0   0   0   0   0


#Apply tune & cost
set.seed(1002)
tune.out=tune(svm,quality~.,data=test,ranges=list(cost=c(0.001,0.01,0.1,5,10,100)))
lin1<-svm(quality~.,test,kernel="linear",cost=0.01)
tune.out
lin1<-svm(quality~.,test,kernel="linear",cost=5)
pred<-predict(lin1,test)
table(pred,test$quality)
xtab <- table(true=test$quality, predicted=pred)


#Creating confusion Matrix
confusionMatrix(xtab)


#OUTPUT

#     pred      3   4   5   6   7   8   9
#            3   1   0   0   0   0   0   0
#            4   0   0   0   0   0   0   0
#            5   2  23 196  86   9   0   0
#            6   2  18 168 464 211  44   1
#            7   0   0   0   0   0   0   0
#            8   0   0   0   0   0   0   0
#            9   0   0   0   0   0   0   0



set.seed(1002)
tune.out=tune(svm,quality~.,data=test,Kernel="polynomial",ranges=list(cost=c(0.001,0.01,0.1,5,10,100)))
lin1<-svm(quality~.,test,kernel="polynomial",cost=0.01)
tune.out

#OUTPUT
#Parameter tuning of 'svm':  
#  - sampling method: 10-fold cross validation 
#
#- best parameters:
#  cost
#5
#
#- best performance: 0 
lin1<-svm(quality~.,test,kernel="linear",cost=5)
pred<-predict(lin1,test)
table(pred,test$quality)

xtab <- table(true=test$quality, predicted=pred)


#Creating confusion Matrix
confusionMatrix(xtab)



#OUTPUT
#     pred   3   4   5   6   7   8   9
#        3   1   0   0   0   0   0   0
#        4   0   0   0   0   0   0   0
#        5   2  23 196  86   9   0   0
#        6   2  18 168 464 211  44   1
#        7   0   0   0   0   0   0   0
#        8   0   0   0   0   0   0   0
#        9   0   0   0   0   0   0   0

