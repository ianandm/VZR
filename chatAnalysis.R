library(tm)
library(data.table)
library(ggplot2)
library(SnowballC)
library(wordcloud) 
extractChatData <- function(){
    # read the positive and negative words from the files
    
    
    file_list <- list.files(path="chat/Apr", pattern=".csv", full.names=TRUE)
    
    for(file in file_list){
        
        if(!exists("cData")){
            tData <- as.data.frame(fread(file, stringsAsFactors = FALSE))
            tData <- subset(tData, (V10 == "Verizon Internet Services" & V15 == "eCenter Sales"), select=c(V16))
            t <- nrow(tData)
            tData <- tData[grep("Account Number", tData$V16, ignore.case=TRUE),]
            cData <- as.data.frame(tData)
            rm(tData)
        }
        else{
            
            tData <- as.data.frame(fread(file, stringsAsFactors = FALSE))
            tData <- subset(tData, (V10 == "Verizon Internet Services" & V15 == "eCenter Sales"), select=c(V16))
            t <- t + nrow(tData)
            tData <- tData[grep("Account Number", tData$V16, ignore.case=TRUE),]
            tData <- as.data.frame(tData)
            cData <- rbind(cData, tData)
            rm(tData)
        }
        
        
    }
    print (t)
    print (nrow(cData))
    
    write.csv(cData, "chat/AprChatDataAccNum.csv")
    
}

#Char regarding order Summary

chatDataFilter1 <- function(fname){
    tData <- as.data.frame(fread(fname, stringsAsFactors = FALSE))
    f1Data <- tData[grep(c("fios"), tData$tData),]
    f2Data <- as.data.frame(f1Data[grep(c("Order"), f1Data$tData),])
    f2Data <- as.data.frame(f2Data[-grep(c("Order Confirmation number"), f2Data$tData),])
    print(nrow(f2Data))
    write.csv(f2Data, "chat/fiosOrderFailFilteredChat1.csv")
}

chatDataFilter2 <- function(fname){
    tData <- as.data.frame(fread(fname, stringsAsFactors = FALSE))
    f1Data <- tData[grep(c("fios"), tData$tData),]
    f2Data <- as.data.frame(f1Data[grep("order number", f1Data$tData, ignore.case=TRUE),])
    write.csv(f2Data, "chat/ON.csv")
    
    print(nrow(f2Data))
}

chatDataFilter3 <- function(fname){
    tData <- as.data.frame(fread(fname, stringsAsFactors = FALSE))
    f1Data <- tData[grep(c("Order Summary"), tData$tData),]
    #f2Data <- as.data.frame(f1Data[grep(c("Order"), f1Data$tData),])
    #f2Data <- as.data.frame(f2Data[-grep(c("Congratulations"), f2Data$tData),])
    print(nrow(f1Data))
    write.csv(f1Data, "chat/fiosOrderSummary.csv")
}



