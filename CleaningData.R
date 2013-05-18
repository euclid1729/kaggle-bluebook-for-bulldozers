#dimension reduction and formatting on merged data
data=mergeddata

drop_cols<-function(data)
{
  data$MachineID<-NULL
  data$ModelID<-NULL
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
rids<-which(data$Drive_System == 'No')
data$Drive_System[rids]=-1

rids<-which(data$Blade_Type =="NO")
data$Blade_Type[rids]=-1

rids<-which(data$Pattern_Changer =="NO") 
data$Pattern_Changer[rids]=-1

rids<-which(data$Blade_Type =="NO") 
data$Blade_Type[rids]=-1