
data = read.csv("md1.csv", header=TRUE)

#one col has NAs
rids<-which(is.na(data[,51]))
x = as.character(data[,51])
x[rids]<- -1
data[,51] <- as.factor(x)

#cols with >100 levels, convert to numeric
colsToNum <- c("fiBaseModel","fiSecondaryDesc","fiModelSeries","fiModelDescriptor","fiManufacturerDesc")
for (i in colsToNum){
  mergeddata[[i]] <- as.numeric(mergeddata[[i]])
}

library(caret)
newdata <- mergeddata[,0]
for(i in names(mergeddata)){
  if (is.factor(mergeddata[,i])){
    print(i)
    x = as.data.frame(model.matrix(~mergeddata[,i]-1))
    #x[,1] <- x[,1] - 1
    names(x) <- paste(i,levels(mergeddata[,i]),sep=".")
    newdata = cbind(newdata,x)
  }
}


data.numercial = mergeddata[,0]
for(i in names(mergeddata)){
  if (!is.factor(mergeddata[[i]])){
    data.numercial[[i]] <- mergeddata[,i]
  }
}
alldata = cbind.data.frame(data.numercial, newdata)
names(alldata) <- paste("s",seq(1:881),sep="")





for (i in names(alldata)){
  print(sum(alldata[[i]]))
  #print(i)
}
  
#pca
alldata$Age <- as.numeric(alldata$Age)
score <- which(colnames(alldata) == "SalePrice")
alldata_pca <- prcomp(alldata[, -score], scale = TRUE)

alldata_pca$sdev

pca.data <- predict(alldata_pca, alldata)