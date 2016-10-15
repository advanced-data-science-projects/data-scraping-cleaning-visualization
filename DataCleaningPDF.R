
### PACKAGE TO READ PDF's
### NOT IN CRAN AS A LIBRARY YET, BUT A PROJECT EXTRACTED FROM GITHUB
### https://github.com/ropenscilabs/tabulizer

install.packages("devtools", dependencies = TRUE)
library(devtools)
devtools::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"), args = "--no-multiarch")

### SET THIS TO YOUR JRE LINK IN THE SYSTEM
Sys.setenv(JAVA_HOME='C://Program Files//Java//jre1.8.0_101') 

install.packages("rJava")
library(rJava)

### TO READ TABLES FROM PDF, DATA MANIPULATION and 
### STRING SPLITTIG OF COLUMN CELLS TO CLEAN DIRTY/COMBINED DATA
library(tabulizer)
library(dplyr)
library(stringr)

## EXTRACTING THE FILE
f <-system.file("examples", "PeerGroup_1_March2016.pdf",package="tabulizer")

## MAKING TABULAR STRUCTURE OF EACH PAGE [LIST OF 28 PAGES as 28 TABLES]
out1 <- extract_tables(f,guess=FALSE,method = "data.frame")

### INITIALIZE THE FINAL RESULT DATA SET

dfresult <- NULL


### NOW THE PDF STRUCTURE IS SUCH THAT THE PDF's EACH TABLE IS BROKEN IN 2 HALVES
### SET 1:- PAGE 1- PAGE 13
### SET 2:- PAGE 14- PAGE 26
### SET 3:- PAGE 27,28 
j <- 14
i<-1

#for(i in length(out1)/2-1)
while(i<14)
{     
  ### LOGIC TO REPLACE EMPTY VALUES AS NA, if "ALL" CELL VALUES ARE NA we "remove" the ROW
  ### ALSO REMOVE THE ROWS havin '---' as the values
  ### FIRST FOR 2nd HALF OF PDF's, SECOND FOR 1st HALF
  
  df2 <-as.data.frame(apply(out1[[j]], 2, function(x) gsub("^$|^ $", NA, x)))
  row.has.na <-(apply(df2, 1, function(x){all(is.na(x))}))
  sum(row.has.na)
  df2 <-as.data.frame(df2[!row.has.na,])
  del<-which(apply(df2, 1, function(x) any(grepl("\\--", x))))
  df2<-df2[-del,]
  
  df1 <-as.data.frame(apply(out1[[i]], 2, function(x) gsub("^$|^ $", NA, x)))
  row.has.na <-(apply(df1, 1, function(x){all(is.na(x))}))
  sum(row.has.na)
  df1 <-as.data.frame(df1[!row.has.na,])
  del<-which(apply(df1, 1, function(x) any(grepl("\\--", x))))
  df1<-df1[-del,]
  
  #############################################################################################
  
  ### LOGIC TO UNLIST THE VALUES and FLATTEN OUT THE COMMON CELL VALUES AS A SINGLE LIST
  ### BASICALLY DONE TO MERGE THE 2 SUBSETS of DATA to FORM 1 LOGICAL TABLE
  ### LOGIC TO MATCH THE 'NUMBER OF ROWS' to be able to do 'BIND BY COLUMN'
  
  if(i==1){
    df2<-df2[-c(1:3),]
    names(df2)=unlist(df2[c(1),])
    names(df2)
    
    df2 <- as.data.frame(df2[-1,])
    
    
    df1<-df1[-1,]
    names(df1)=unlist(df1[c(1),])
    names(df2)
    df1 <- as.data.frame(df1[-1,])
    
    
  }
  
  else if(i==8){
    
    df2<-df2[-c(1:2),]
    names(df2)=unlist(df2[c(1),])
    names(df2)
    
    df2 <- as.data.frame(df2[-1,])
    
    df1<-df1[-c(1:2),]
    names(df1)=unlist(df1[c(1),])
    names(df2)
    df1 <- as.data.frame(df1[-1,])
    
    
  }
  
  else{
    df2<-df2[-c(1:2),]
    names(df2)=unlist(df2[c(1),])
    names(df2)
    df2 <- as.data.frame(df2[-1,])
    
    df1<-df1[-1,]
    names(df1)=unlist(df1[c(1),])
    names(df2)
    df1 <- as.data.frame(df1[-1,])
    
    
  }
  print(nrow(df1))
  print(nrow(df2))
  
  ########################### NEW ADDITION ############################## 
  
  ### CLEAN THE DATA BY USING REGEX PATTERNS FOR SUBSET 2
  ### VALUES THAT ARE STUCK IN 1 COLUMN
  ### USED 'GSUB' FUNCTION
  
  ### TO BE ABLE TO REMOVE COLUMNS WITH ENTIRE 'NA' VALUES
  df2<-df2[, colSums(is.na(df2)) != nrow(df2)]
  divisions<-2
  info <- names(df2[1])
  names(df2)=""
  
  if(i==1){
    
    df2.trial <- data.frame(t(sapply(df2[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2', y))))
    divisions<-2
    
  } else if(i==9){
    df2.trial <- data.frame(t(sapply(df2[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2d\\3d\\4d\\5d\\6d\\7d\\8', y))))
    divisions<-8
    
  } else if(i==2 | i==4 | i==11){
    df2.trial <- data.frame(t(sapply(df2[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2d\\3d\\4d\\5d\\6', y))))
    divisions<-6
    
  } else if(i==3 | i==6 | i==8 | i==12 | i==13){
    df2.trial <- data.frame(t(sapply(df2[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2d\\3d\\4d\\5', y))))
    divisions<-5
    
  } else if(i==5 | i==10){
    df2.trial <- data.frame(t(sapply(df2[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2d\\3', y))))
    divisions<-3
    
  } else{
    df2.trial <- data.frame(t(sapply(df2[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2d\\3d\\4', y))))
    divisions<-4
    
  }
  
  df2[[2]] = t(df2.trial)
  
  df2.yo2 <- cbind(df2[1],str_split_fixed(df2[[2]], "d", divisions),df2[,c(3:ncol(df2))])
  
  header.df <- c(info,"Ratio","5%","10%","25%","50%","75%","90%","95%","BHC COUNT")
  
  names(df2.yo2)  <- header.df
 
  ################################################################################################
  ### CLEAN THE DATA BY USING REGEX PATTERNS FOR SUBSET 1
  ### VALUES THAT ARE STUCK IN 1 COLUMN
  ### USED 'GSUB' FUNCTION
  
  ### TO BE ABLE TO REMOVE COLUMNS WITH ENTIRE 'NA' VALUES
  df1<-df1[, colSums(is.na(df1)) != nrow(df1)]
  divisions<-1
  if(i==1){
    names(df1)[1] <- "Summary Ratios"
  }
  info <- names(df1[1])
  names(df1)=""
  
  if(i==2 | i==4 | i==6 | i==8 | i==11 ){
    df1.trial <- data.frame(t(sapply(df1[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2d\\3', y))))
    divisions<-3
    
  } else if(i==3 | i==5 | i==7 | i==10 | i==12 | i==13){
    df1.trial <- data.frame(t(sapply(df1[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2', y))))
    divisions<-2
    
  } else if(i==9){
    df1.trial <- data.frame(t(sapply(df1[,2], function(y) gsub('(^-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})(-*[0-9]*.[0-9]{2})', '\\1d\\2d\\3d\\4', y))))
    divisions<-4
    
  } 
  
  ################################################################################################
  
  
  
  if(i==1){
    
    header.df <- c(info,"03/31/2016","03/31/2015","12/31/2015","12/31/2104","12/31/2013")
    names(df1)  <- header.df
    df1.yo1 <- df1
    
  } else{
    df1[[2]] = t(df1.trial)
    df1.yo1 <- cbind(df1[1],str_split_fixed(df1[[2]], "d", divisions),df1[,c(3:ncol(df1))])
    header.df <- c(info,"03/31/2016","03/31/2015","12/31/2015","12/31/2104","12/31/2013")
    names(df1.yo1)  <- header.df
  }

  
  ################################################################################################
  
  ### CBIND TO FORM THE ORIGINAL DATA TABLES
  ### THAT WERE DISTORTED WHILE BEING WRITTEN ON PDF's
  
  if(nrow(df1.yo1)==nrow(df2.yo2))
    dffinal1 <-(cbind(as.data.frame(df1.yo1),as.data.frame(df2.yo2)))
  
  #write.csv(dffinal1, file = paste("page_",i,'.csv'))
  
  dffinal1.new<- dffinal1[,-7]
  
  #to_be_removed <- names(dffinal1.new)[1]
  colnames(dffinal1.new)[1] <- "Parameters"
  
  ### FINAL ROW BINDING OF ALL THE DATA FRAMES TO MAKE 1 CSV
  dfresult <- bind_rows(dfresult,dffinal1.new)
  
  ### OUTPUTTING THE CSV
  write.csv(dfresult, file = paste("PEER1_2016","_Q1",'.csv'))
  
  
  i<-i+1
  j<-j+1
  
}



