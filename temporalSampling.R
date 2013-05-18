data <- data.origin[which(data.origin$SaleYear > 2008) ,]
#data <- data[sample(1:nrow(data),50000,replace=F),]
#data$fiBaseModel<-NULL;

#data <- clean(data);
test <- read.csv("Test.csv", sep=",", header=TRUE, na.strings="", stringsAsFactors=TRUE);
test <- clean(test);
#x <- uLevels(colnames(test),data,test);
#data <- x[1]
#test <- x[2]
#Call ulevels


library(caret)
folds <- createFolds(data,k=5,list=FALSE)
grid = expand.grid(list(cp=c(0.01),maxdepth=c(5)))
output = data.frame("rsmse"=c(),"fold_no"= c());
for(i in 1:5) {
  fold.rows <- which(folds != i)
  cv.train <- data[-fold.rows,]
  cv.test <- data[fold.rows,]
  for(j in 1:nrow(grid)){
    print(paste(i,j))
    cv.tree <- rpart(SalePrice~.,data=cv.train,control=rpart.control(cp=grid[j,1],maxdepth=grid[j,2]))
    pred.values <- predict(cv.tree,newdata=cv.test)
    rmlse <- sqrt(sum ( (log(pred.values)-log(cv.test$SalePrice))^2 )/length(pred.values) );
    output<-rbind(output, c(rmlse=rmlse, fold=i));
  }
}
colnames(output) <- c("rmsle","fold_no");
output<-cbind(grid,output);
output

cv.tree <- rpart(SalePrice~.,data=data,control=rpart.control(cp=0.01,maxdepth=5))

pred.values <- predict(cv.tree,test)
write.csv(pred.values,"values.csv",row.names=F)

