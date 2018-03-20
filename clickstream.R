
#************************
# Description: Data Cleaning - Parse and have the click stream data
#              organized in a way that can be read and understood
#              easily.
# Author: Anand Mohan
# Date: Apr72016
#************************

# This function allows reading in files of large size. 
#1. Extract the file into an data frame using fread function.
#2. Extract the rows for March into a new file for further processing

library(data.table)
library("ff")
readfromBigTxtFile <- function(fname){
    # Read the large file into a data frame using fread
    bigCSData <- fread(fname, nrows=10000000, header=TRUE)
    
    #Spliting the data set into 3
    
 
    colnames(bigCSData)[1] <- c("Global_ID")
    keeps <- c("Global_ID", "CREATE_DATETIME","CLICK_STRING_NAME", "ACTION_DONE", "EXISTING_CUSTOMER",
         "PAGE_NAME", "URL", "ADDRESS_ID")
          bigCSData <- as.data.frame(bigCSData)
          bigCSData <- bigCSData[keeps]
      
     #Change the names of columns to ones that can help understand the columns. We can also use
     # colnames(bigCSData)[2] <- "Column_Name" if we want to change only 1 column name
     #colnames(bigCSData)[2] <- c("Global_ID")
      
      
    bigCSData$CREATE_DATETIME <- as.POSIXct(as.character(bigCSData$CREATE_DATETIME), format="%Y-%m-%d %H:%M:%S")
    bigCSData <- subset(bigCSData, CREATE_DATETIME >= "2016-03-01 00:00:00" & CREATE_DATETIME <= "2016-03-31 23:59:59")
      
    #Find the names of colums in a data frame
    #names(bigCSData)
    write.csv(bigCSData, "ClickStream/bigCSData1.csv")
    #write.csv(bigCSData2, "ClickStream/bigCSData2.csv")
    #write.csv(bigCSData3, "ClickStream/bigCSData3.csv")
  
}

removeUnnecCol <- function(){
    
}

readFromFile <- function(filename, nofRows, outputFileName){
  
  fileOp <- read.csv(filename,sep="^", header=FALSE, nrows=nofRows)
  
  dfile <- data.frame()
  for(i in 1:nrow(fileOp)){
    if(fileOp[i,"V1"] == "B10881281280")
      dfile <- rbind(dfile, fileOp[i,])
    
  }
  write.csv(dfile, outputFileName)
}

# sort by most occuring global id duplicates and write to a new table
sortByDuplicates <- function(fname, outfname){
  #read from the csv file
  #t2DS <- new data.frame( )
  tempDataSet <- as.data.frame(fread(fname, nrows=8400000, header=TRUE))
  
  t <- sort(table(tempDataSet$Global_ID), decreasing=TRUE)[1:5]
  dfile <- data.frame()

  for(j in 1:10000){
      for(i in 1:nrow(t)){
          if(tempDataSet[j,]$Global_ID == dimnames(t)[[1]][i] && tempDataSet[j,]$ACTION_DONE != "BannerLoad")
              dfile <- rbind(dfile, tempDataSet[j,])
              
      }
  }
  
  #tdata[,"order_close"] <- tdata$Global_ID %in% tfdataTNum$GLOBAL_SESSION_ID
  
  write.csv(dfile, outfname)
  
}

urlExtract <- function(fname, outfname){
    tempDataSet <- as.data.frame(fread(fname, nrows=8400000, header=TRUE))
    urlDataSet <- as.data.frame(tempDataSet$URL)
    write.csv(urlDataSet, outfname)
    
}
