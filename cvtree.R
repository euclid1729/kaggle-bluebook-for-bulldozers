dtree<-function()
{

library(caret)
library(rpart)

data <- read.csv("md1.csv")
#testdata<-read.csv("dataProcessed/combinedprocessedTest.data.csv")

library(imputation)
cv.meanImpute(data)


for (i in 1:ncol(testdata))
{
 rids<-which(is.na(testdata[,i]))
 if (length(rids) >0)
 {
  testdata[rids,i]<- -1
 }
}


#creating 10 folds
Y<-data[,3]
folds <- createFolds(Y,k=10,list=FALSE)

fold.rows1 <- which(folds == 1)
cv.train1 <- data[fold.rows1,]
cv.tree1 <- rpart(SalePrice~.,data=cv.train1,control=rpart.control(cp=0.01))

fold.rows2 <- which(folds ==2)
cv.train2 <- data[fold.rows2,]
cv.tree2 <- rpart(SalePrice~.,data=cv.train2,control=rpart.control(cp=0.01))

fold.rows3 <- which(folds == 3)
cv.train3 <- data[fold.rows3,]
cv.tree3 <- rpart(SalePrice~.,data=cv.train3,control=rpart.control(cp=0.01))

fold.rows4 <- which(folds == 4)
cv.train4 <- data[fold.rows4,]
cv.tree4 <- rpart(SalePrice~.,data=cv.train4,control=rpart.control(cp=0.01))

fold.rows5 <- which(folds == 5)
cv.train5 <- data[fold.rows5,]
cv.tree5 <- rpart(SalePrice~.,data=cv.train5,control=rpart.control(cp=0.01))

fold.rows6 <- which(folds == 6)
cv.train6 <- data[fold.rows6,]
cv.tree6 <- rpart(SalePrice~.,data=cv.train6,control=rpart.control(cp=0.01))

fold.rows7 <- which(folds == 7)
cv.train7 <- data[fold.rows7,]
cv.tree7 <- rpart(SalePrice~.,data=cv.train5,control=rpart.control(cp=0.01))

fold.rows8 <- which(folds == 8)
cv.train8 <- data[fold.rows8,]
cv.tree8 <- rpart(SalePrice~.,data=cv.train8,control=rpart.control(cp=0.01))

fold.rows9 <- which(folds == 9)
cv.train9 <- data[fold.rows9,]
cv.tree9 <- rpart(SalePrice~.,data=cv.train9,control=rpart.control(cp=0.01))

fold.rows10 <- which(folds == 10)
cv.train10 <- data[fold.rows10,]
cv.tree10 <- rpart(SalePrice~.,data=cv.train10,control=rpart.control(cp=0.01))

#making predictions
pred.values1 <- predict(cv.tree1,newdata=testdata)
pred.values2 <- predict(cv.tree2,newdata=testdata)
pred.values3 <- predict(cv.tree3,newdata=testdata)
pred.values4 <- predict(cv.tree4,newdata=testdata)
pred.values5 <- predict(cv.tree5,newdata=testdata)
pred.values6 <- predict(cv.tree6,newdata=testdata)
pred.values7 <- predict(cv.tree7,newdata=testdata)
pred.values8 <- predict(cv.tree8,newdata=testdata)
pred.values9 <- predict(cv.tree9,newdata=testdata)
pred.values10 <- predict(cv.tree10,newdata=testdata)

pred.avg=(pred.values1+pred.values2+pred.values3+pred.values4+pred.values5+pred.values6+pred.values7+pred.values8+pred.values9+pred.values10)/10 
write.csv(pred.avg,"dataset/results.csv",row.names=FALSE)

}


