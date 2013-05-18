#filling missing values with -1
for (i in 1:ncol(data))
{
  rids<-which(is.na(data[,i]))
  if (length(rids) >0)
  {
    data[rids,i]<- -1
  }
}
