
############# 
# Be careful
library(gdata);
names <- colnames(test);
for(c in names){
  print(c)
  
  if ( is.factor(data[,c]) ) {
    #train[,c] <- factor(as.character(train[,c]))
    
    #test[,c] <- factor(as.character(test[,c]))
    
    
    map <- mapLevels(x=list(data[,c], test[,c]), codes=FALSE, combine=TRUE)
    
    mapLevels(data[,c]) <- map
    mapLevels(test[,c]) <- map
  }
}
test$PrimaryLower <- as.numeric(test$PrimaryLower)
test$PrimaryUpper <- as.numeric(test$PrimaryUpper)

