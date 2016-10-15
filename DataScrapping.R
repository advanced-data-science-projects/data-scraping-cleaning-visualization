#==================CREATING CSVs=======================

install.packages("rvest")
library(rvest)

library(RSelenium)

checkForServer()

startServer()


Sys.sleep(5)

remDrv <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "firefox")

remDrv$open()

remDrv$navigate("https://www.ffiec.gov/nicpubweb/nicweb/HCSGreaterThan10B.aspx")

iframe <- remDrv$findElement(using='id', value="DateDropDown")



library(XML)
webpage <- readLines("https://www.ffiec.gov/nicpubweb/nicweb/HCSGreaterThan10B.aspx")
htmlpage <- htmlParse(webpage, asText = TRUE)
htmlpage
timeframe <- xpathSApply(htmlpage, "//option", function(u) xmlAttrs(u)["value"])
as.numeric(timeframe)

timeframe

for(i in timeframe){
  
  
  option <- remDrv$findElement(using = 'xpath', paste('//*/option[@value=',i,"]"))
  option$clickElement()
  
  
  
  doc <- htmlParse(remDrv$getPageSource()[[1]])
  
  
  Sys.sleep(2)
  tables <- getNodeSet(doc, "//table")
  Sys.sleep(2)
  xt <- readHTMLTable(tables[[3]],
                      header = TRUE,
                      as.data.frame = TRUE,
                      colClasses = c("numeric","character","character",
                                     "character"),
                      trim = TRUE, stringsAsFactors = FALSE
                      
  )
  
  
  
  write.csv(xt, file = paste("quarter_",i,'.csv'))
}


#======================MERGING CSVs========================

w = NULL

for (i in timeframe){
  y <- data.frame(read.csv(paste("quarter_",i,".csv"), header = TRUE))
  x<-replicate(nrow(y),i)
  y$quarter <- as.Date(x,"%Y%m%d")
  colnames(y) <- c('SNo', 'Rank','Company', 'Location', 'Assets', 'Quarter')
  w <- rbind(w,y)
}

#sorting based on Company name
w <- w[order(w$"Company"),]  

#splitting company ID and Location
install.packages('reshape2')
library(reshape2)

w1 <- with(w, cbind(Rank, colsplit(w$Company, pattern = "\\(", names = c('Institution Name', 'RSSDId')),colsplit(w$Location, pattern = "\\,", names = c('City', 'State')), Assets, Quarter))
w2 <- as.data.frame(sapply(w1,gsub,pattern=")",replacement=""))

head(w2)

#writing as a stacked CSV 
write.csv(w2, file = "Stacked.csv")


#######====== writing unstacked data ========#################

counter = 1

timeframe
#i <- "20130331"
for (i in timeframe){
  unstacked_ <- data.frame(read.csv(paste("quarter_",i,".csv")))
  quarter_row<-rep(i,nrow(unstacked_))
  
  
  unstacked_$quarter <- as.Date(quarter_row,"%Y%m%d")
  #colnames(unstacked_) <- c('SNo', 'Rank','Company', paste("location_", i, sep=""), 'Assets', 'Quarter')
  
  
  
  #unstacked_
  #colnames(unstacked_)
  #colnames(unstacked_)[4] <- gsub('(X[0-9]*.[0-9]*.[0-9]*.)()','\\1d\\2',colnames(unstacked_)[4])
  #df2.trial <- data.frame(t(sapply(df2[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2', y))))
  #unstacked <- with(unstacked, cbind(Rank, colsplit(unstacked$Company, pattern = "\\(", names = c('Company', 'CompanyId')),colsplit(unstacked$Location, pattern = "\\,", names = c('City', 'State')), Assets, Quarter))
  
  
  w1 <- with(unstacked_, cbind(unstacked_[,2], colsplit(unstacked_$Institution.Name..RSSD.ID., pattern = "\\(", names = c('Institution Name', 'RSSDId')),colsplit(unstacked_$Location, pattern = "\\,", names = c('City', 'State')), unstacked_[,5]))
  w2 <- as.data.frame(sapply(w1,gsub,pattern=")",replacement=""))
  colnames(w2)[1] <- paste("Rank ",i)
  colnames(w2)[6] <- paste("Assets",i)
  
  
  if (counter == 1){
    unstacked <- w2
    counter = 2
  }
  
  else{
    unstacked <- merge(unstacked,w2, all.x=TRUE,all.y = TRUE)
  }
  
  #head(unstacked)
}

head(unstacked)
write.csv(unstacked, file = "unstackedStacked.csv")
