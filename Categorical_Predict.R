#Import Files
getwd()
setwd("C:/Users/sxx180001/Desktop/data_for_ex.csv_Pieces")

files = list.files(pattern="*.csv")
install.packages("data.table")
install.packages("readr")
library(data.table)
library(readr)
library(dplyr)

data = rbindlist(lapply(files, fread))

#change character ot factor datatype at once

for(i in 1:ncol(data)){
  if(is.character(data[,i])){
    data[,i]=as.factor(data[,i])
  }
}
#or can do it manually
data$v69<-as.factor(data$v69)
data$v20<-as.factor(data$v20)
data$v15<-as.factor(data$v15)
data$v16<-as.factor(data$v16)
data$v13<-as.factor(data$v13)
data$v6<-as.factor(data$v6)
data$v5<-as.factor(data$v5)
data$v7<-as.factor(data$v7)
data$v8<-as.factor(data$v8)
data$v9<-as.factor(data$v9)
data$v10<-as.factor(data$v10)
data$v418<-as.factor(data$v418)
data$v423<-as.factor(data$v423)
data$v424<-as.factor(data$v424)
data$v425<-as.factor(data$v425)
data$v426<-as.factor(data$v426)
data$v427<-as.factor(data$v427)
data$v428<-as.factor(data$v428)
data$v429<-as.factor(data$v429)
data$v433<-as.factor(data$v433)
data$v434<-as.factor(data$v434)
data$v435<-as.factor(data$v435)
data$v436<-as.factor(data$v436)
data$v437<-as.factor(data$v437)
data$v438<-as.factor(data$v438)
data$v439<-as.factor(data$v439)
data$v440<-as.factor(data$v440)
data$v441<-as.factor(data$v441)
data$v442<-as.factor(data$v442)
data$v443<-as.factor(data$v443)
data$v444<-as.factor(data$v444)
data$v445<-as.factor(data$v445)
data$v446<-as.factor(data$v446)
data$v447<-as.factor(data$v447)
data$v448<-as.factor(data$v448)
data$v449<-as.factor(data$v449)
data$v450<-as.factor(data$v450)
data$v451<-as.factor(data$v451)
data$v452<-as.factor(data$v452)
data$v453<-as.factor(data$v453)
data$v454<-as.factor(data$v454)
data$v455<-as.factor(data$v455)
data$v456<-as.factor(data$v456)
data$v457<-as.factor(data$v457)
data$v458<-as.factor(data$v458)
data$v459<-as.factor(data$v459)
data$v460<-as.factor(data$v460)
data$v461<-as.factor(data$v461)
data$V1<-as.numeric(data$V1)
#check character columns and factor columns
data[, sapply(data, class) == 'character']

#divide data into training and testing
sample_size <- floor(0.8*nrow(data))
train_new <- sample(seq_len(nrow(data)), size = sample_size)
train<-data[train_new,]
test<-data[-train_new,]
attach(data)

#random forests to check variable importance
library(randomForest)
#The mtry is taken sqrt(no of variables) by deafualt
rf.hd = randomForest(target~., data=data,subset=train,mtry = 20, importance = TRUE)
rf.hd
#use the importance() function, we can view the importance of each variable
importance(rf.hd)
varImpPlot(rf.hd,type=2)
#important variables
#v187,211,212,191,293,186,210,199,188,351,406,215,260,243
##########################################################################################


#Models
#run Logistic, LDA, QDA, KNN
#Logistic
##With the selected variables
##The top 10 most important variables are v187,v211,v212,v191,v293,v186,v210,v199,v188,v351
log.pred=glm(target~v187+v211+v212+v191+v293+v186+v210+v199+v188+v351,data=train,family=binomial)
summary(log.pred)
test$prob=predict(log.pred,test,type="response")
test$predDir=0
test$predDir[test$prob>0.5]=1
with(test,
     table(predDir,target))
#testing the model performance with test data
with(test,
     mean(predDir==target))
#[1] 0.971025

#LDA

library(MASS)
#fit the LDA model
lda.pred=lda(target~v187+v211+v212+v191+v293+v186+v210+v199+v188+v351,data=train)
lda.pred
plot(lda.pred)
#predictions
ldatest=predict(lda.pred,test)
names(ldatest)

table(ldatest$class,test$target)
#testing the model performance with test data
mean(ldatest$class==test$target)
#[1] 0.9487

#QDA
qda.pred=qda(target~v187+v211+v212+v191+v293+v186+v210+v199+v188+v351,data=train)
qda.pred
qdatest=predict(qda.pred,test)
names(qdatest)
table(qdatest$class,test$target)

#testing the model performance with test data
mean(qdatest$class==test$target)
#[1] 0.977225

##KNN

library(class)
knntrain=train[,c(1,3:463)]
knntest=test[,c(1,3:463)]
knnlabel=as.matrix(train[,464])
knn.pred=knn(knntrain,knntest,knnlabel,k=1)
table(knn.pred,test$hd)
#testing the model performance with test data
mean(knn.pred==test$hd)
#[1] 0.98275

knntrain=train[,c(1:13)]
knntest=test[,c(1:13)]
knnlabel=as.matrix(train[,14])
knn.pred=knn(knntrain,knntest,knnlabel,k=100)
table(knn.pred,test$hd)
#testing the model performance with test data
mean(knn.pred==test$hd) 
#[1] 0.97075

#Run decison tree

install.packages('tree')
library(tree)
library(ISLR)

tree.hd= tree(target~v187+v211+v212+v191+v293+v186+v210+v199+v188+v351, train)
summary(tree.hd)
tree.hd

#Pruning the tree
set.seed(3)
cv.hd = cv.tree(tree.hd, FUN = prune.misclass)
cv.hd
#Get the tree that gives the lowest error
tree.hd = prune.misclass(tree.hd, best = 6)
plot(tree.hd)
text(tree.hd, pretty = 0)
tree.hd
#For predictions of the tree, we use predict function. 
tree.pred = predict(tree.hd,test, type = "class")
table(tree.pred, test$target)
#testing the model performance with test data
mean(tree.pred==test$target)
#[1] 0.9977

#Run Random forest
install.packages('randomForest')
library(randomForest)
train$target<-as.factor(train$target)
test$target<-as.factor(test$target)
rf.hd = randomForest(target~v187+v211+v212+v191+v293+v186+v210+v199+v188+v351, data=train, mtry = 4, importance = TRUE,ntree=40)
rf.hd
rf.y = predict(rf.hd, newdata = test)
#testing the model performance with test data
mean((rf.y==test$target))#[1] 0.9999

#check the least error out of all methods.
#After MSE the least error comes out in Random forest>Decision tree>(KNN and QDA performs similar)
#The problem of overfittong might occur when we get test error more than training error, I think that will
#occur when the training set is very large.
#Here I have taken 80 % in the training set and 20 % in the test set.
#Since the data is quite large it gets enough samples to test the prediction
#The mtry is taken sqrt(no of variables) by deafualt, it can also be calculated using cross validation.
#ntree is taken which has minimum error.
#text column can be converted to factor and its indivisual effects of all the models can on testing with 
#text can be seen. Need to check special character in each text