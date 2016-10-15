value <- c('20121231_20130801_FRY15 Snapshot All','20131231_20140801_FRY15 Snapshot All','20141231_20150803_FRY15 Snapshot All')

combine = NULL

for (i in value){
  test <- data.frame(read.csv(paste(i,".csv"), header = TRUE))
  #head (test)
  combine <- rbind(combine,test)
}

#combine <- data.frame(combine,na.rm = TRUE)
newdata <- combine[complete.cases(combine),]

head(newdata)

finaldata <- data.frame(newdata)
finaldata[,] <- as.character.default(gsub("<NULL>", 0, as.matrix(finaldata[,])))
final <- as.data.frame(finaldata)
final$Date <- as.Date.character(final$DT,"%Y%m%d")
newdata <- final[complete.cases(final),]
head(final)


#writing as a stacked CSV for part3
write.csv(newdata, file = "Stacked_part3.csv")
