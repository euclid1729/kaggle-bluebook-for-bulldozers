## Helper function to parse the SaleDate Column ##
getDate <- function(saleDateCol) {
  result <- matrix(0,nrow=length(saleDateCol),ncol=3)
  for(i in 1:length(saleDateCol)) {
    spt <- strsplit(as.character(saleDateCol[i]),'/')[[1]]
    result[i,1] <- as.numeric(as.character(spt[1]))
    result[i,2] <- as.numeric(as.character(spt[2]))
    result[i,3] <- as.numeric(as.character(strsplit(spt[3]," ")[[1]][1]))
  }
  
  colnames(result) <- c("SaleMonth","SaleDay","SaleYear")
  return(result)
}

## Read in the data ## 
#dat <- read.csv("dataset/TrainAndValid.csv")
appendix <- read.csv("Machine_Appendix_edited.csv")
datTest <- read.csv("Test.csv")

## Pull in the data from the machine appendix ##
#merged <- merge(dat,appendix,by="MachineID",all.x=TRUE)
mergedTest <- merge(datTest,appendix,by="MachineID",all.x=TRUE)
## Get rid of duplicated columns ##
#old.cols <- grep("\\.x",names(merged))
old.colsTest<-grep("\\.x",names(mergedTest))
mergedTest <- mergedTest[,-old.colsTest]
new.cols <- grep("\\.y",names(mergedTest))
new.names <- unlist(lapply(names(mergedTest)[new.cols],strsplit,'\\.'))[seq(1,13,2)]
names(mergedTest)[new.cols] <- new.names

## Pull in the machineID, SalesID ##
final.data <- cbind(mergedTest[,1:2])
feature.map <- read.csv("feature.map.csv")
col.index <- 3
for(i in 3:ncol(mergedTest)) {
  print(i)
  ## Parse the saledate column and break it up into day, month, year columns ##
  if(names(mergedTest)[i] == "saledate") {
    print("Parsing saledate column...")
    date <- getDate(mergedTest[,i])
    final.data <- cbind(final.data,date)
    col.index <- ncol(final.data) + 1
  } else {
    current.col <- mergedTest[,i]   
    ## Test to see if it's factor, if it is convert it to an int representation ##
    if(is.factor(current.col)) {
       feature.name <- names(mergedTest)[i]
       print(feature.name)
       
       #fetching rowids from feature.map for ith column 
       frids<-which(feature.map[,1] == feature.name)
       fmap.currentcol=feature.map[frids,3]
       ## Set all missing values to NA ##
       missing.vals <- which(current.col == "")
       current.col[missing.vals] <- NA
       
       ## Remove the "" level from current.col ##
       current.col <- factor(current.col)
       
       ## Keep track of the mapping ##
       
      # feature.map <- rbind(feature.map,cbind(feature.name,1:length(levels(current.col)), levels(current.col)))
         
       # Convert to an int #
       int.version <- match(current.col,fmap.currentcol)
       current.col <- int.version
       
    }
    final.data <- cbind(final.data,current.col)
    names(final.data)[col.index] <- names(mergedTest)[i]
    col.index <- col.index + 1
  }
}

write.csv(final.data,"combinedprocessedTest.data.csv",row.names=FALSE)
