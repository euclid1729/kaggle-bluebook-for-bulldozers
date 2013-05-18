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

clean = function (train){
  machineapp = read.csv("Machine_Appendix.csv", sep=",", header=TRUE, na.strings="", stringsAsFactors=TRUE);
  newma = data.frame(machineapp$MachineID, machineapp$fiManufacturerID, machineapp$fiManufacturerDesc, machineapp$PrimarySizeBasis, machineapp$PrimaryLower, machineapp$PrimaryUpper, machineapp$MfgYear)
  colnames(newma) <- c('MachineID', 'fiManufacturerID', 'fiManufacturerDesc', 'PrimarySizeBasis', 'PrimaryLower', 'PrimaryUpper', 'MfgYear');
  print("dad")
  mergeddata = merge(train, newma, by="MachineID");
  
  print("here")
  mergeddata$YearMade <- NULL
  mergeddata$YearMade <- mergeddata$MfgYear
  mergeddata$MfgYear <- NULL
  mergeddata$SalesID <- NULL
  
  
  rIds <- which(is.na(mergeddata$YearMade))
  length(rIds)
  mergeddata$YearMade[rIds] <- 0
  rIds <- which(mergeddata$YearMade==9999)
  mergeddata$YearMade[rIds] <- 0
  rIds <- which(mergeddata$YearMade==1000)
  mergeddata$YearMade[rIds] <- 0
  rIds <- (which(mergeddata$YearMade>2013))
  mergeddata$YearMade[rIds] <- 0
  rIds <- (which(mergeddata$YearMade<1900))
  mergeddata$YearMade[rIds] <- 0

  #if training data we compute average
  rIds <- which(mergeddata$YearMade==0)
  mergeddata$YearMade[rIds]<-1994
  
  saledate = as.Date(gsub(' 0:00','',mergeddata$saledate),format="%m/%d/%Y")
  YearMade = as.Date(as.character(mergeddata$YearMade),format="%Y")
  Age = saledate - YearMade
  names(Age) <- c("Age")
  mergeddata <- cbind(mergeddata,Age)
  dates <- getDate(mergeddata$saledate)
  mergeddata<-cbind(mergeddata, dates)
  mergeddata$saledate<-NULL
  mergeddata$fiModelDesc <- NULL
  
  mergeddata = drop_cols(mergeddata)
  mergeddata = missingvals(mergeddata)
  #mergeddata = cleanNos(mergeddata)
  #uLevels(colnames(data),data,cv.test);
  return (mergeddata)
}

drop_cols<-function(data)
{
  data$MachineID<-NULL
  data$ModelID<-NULL
  # data$fiBaseModel<-NULL
  data$datasource<-NULL
  data$MachineHoursCurrentMeter<-NULL
  #data$YearMade<-NULL
  #data$SaleYear<-NULL
  #data$SaleMonth<-NULL
  #data$SaleDay<-NULL
  data$fiProductClassDesc<-NULL
  data$fiManufacturerDesc<-NULL
  data$ProductGroupDesc<-NULL
  
  #maybe we should not
  data$fiModelSeries<-NULL
  data$Engine_Horsepower<-NULL
  data$Coupler_System<-NULL
  data$Grouser_Tracks<-NULL
  data$Backhoe_Mounting<-NULL
  data$Blade_Extension<-NULL
  
  
  data$Age <- as.numeric(data$Age)
  data$auctioneerID <- as.factor(data$auctioneerID)
  return(data)
}

missingvals<-function(data){
  
  for (i in 1:ncol(data))
  {
    rids<-which(is.na(data[,i]))
    if (length(rids) >0)
    {
      data[,i] <- as.character(data[,i])
      data[rids,i]<- -1
      data[,i] <- as.factor(data[,i])
    }
  }
  return(data)
}

cleanNos <- function(data){
  rids<-which(data$Drive_System == 'No')
  data$Drive_System[rids]=-1
  
  rids<-which(data$Blade_Type =="NO")
  data$Blade_Type[rids]=-1
  
  rids<-which(data$Pattern_Changer =="NO") 
  data$Pattern_Changer[rids]=-1
  
  rids<-which(data$Blade_Type =="NO") 
  data$Blade_Type[rids]=-1
  return (data)
}
defactor32 <- function(data){
  for (i in names(data)){
    if ( is.factor(data[[i]]) && length(levels(data[[i]])) > 1024){
      data[[paste(i,"0",sep="")]] <- as.factor(round(as.numeric(data[[i]]) / 32))
      data[[paste(i,"1",sep="")]] <- as.factor(round(as.numeric(data[[i]]) %% 32))
      data[[i]] <- NULL
      print(i)
    }
  }
  return (data)
}



