#data <- data[sample(1:nrow(data),50000,replace=F),]
#data$fiBaseModel<-NULL;

#data <- clean(data);
test <- read.csv("Test.csv", sep=",", header=TRUE, na.strings="", stringsAsFactors=TRUE);
train <- read.csv("TrainAndValid.csv", sep=",", header=TRUE, na.strings="", stringsAsFactors=TRUE);
test$SalePrice <- -1

all <- rbind(train,test)
all <- clean(all);
all.origin <- all
all <- defactor32(all); 
all <- defactor32(all);
data.test <- all[which(all$SalePrice == -1),]
data.origin <- all[which(all$SalePrice != -1),]
data.train <- data.origin[which(data.origin$SaleYear > 2011) ,]

cv.tree <- gbm(SalePrice~.,data=data.train)

#test <- clean(test);
#x <- uLevels(colnames(test),data,test);
#data <- x[1]
#test <- x[2]
#Call ulevels


library(caret)
library(randomForest)
colnames(output) <- c("rmsle","fold_no");
output<-cbind(grid,output);
output

cv.tree <- randomForest(SalePrice~.,data=data.train,mtry=5,ntree=2)

pred.values <- predict(cv.tree,data.test,n.trees=200)
write.csv(pred.values,"values.csv",row.names=F)
