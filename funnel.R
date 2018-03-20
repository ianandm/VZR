# reading a csv file

library("ff")
library("data.table")
library("plotly")
readFunnelCSV <- function(fName, mName, fOutName){
  
  funFile <- as.data.frame(fread(fName, header=TRUE, colClasses = "character"))
  funFile <- as.data.frame(funFile)
  colnames(funFile)[1] <- "DATE" #if we want to change only 1 column name
  newFunFile <- data.frame()
  str(funFile)
    

  newFunFile <- subset(funFile, substring(DATE, 1,3) == mName )
 
  write.csv(newFunFile, fOutName)

}

# To carry out operations on the march file
# 1. Pages from where maximum drop outs happen

dSetOps <- function(val, fname){
    
    if(val == 1)
        maxDropOut(fname)
    else if (val == 2)
        channelUsage(fname)
    else if (val == 3)
        maxDropOutByChannel(fname)
    else if (val==4)
        stateView(fname)
}

dSetRead <- function(fname){
    dsForView <- as.data.frame(fread(fname, header=TRUE, colClasses = "character"))
    #dsForView <- data.frame()
    return(dsForView)
}

maxDropOut <- function(fname){
    
    marFile <- data.frame()
    marFile <- dSetRead(fname)
    
    dsForView <- subset(marFile, (substring(application_id, 1,1) == "N" & tracking_nbr == 0 & fttpcapableflag == "Y" & lastpagedropped=="OROMOS"))
    print(nrow(dsForView))
    d <- dsForView[which(dsForView$lastpagedropped=="OROMOS"),]
    
    t <- sort(table(dsForView$lastpagedropped), decreasing=TRUE)[1:10]
    
    str(t)
    
    t<- as.data.frame(t)
    str(t)
    colnames(t) <- c("PageName", "Frequency")
    t
    
    #barplot(t, main="Pages from where max drop outs happen", xlab="Page abbreviations", col="light blue")
    
}

firstVistFlow <- function(fname){
    
    marFile <- data.frame()
    marFile <- dSetRead(fname)
    
    dsForView <- subset(marFile, (substring(application_id, 1,1) == "N" & tracking_nbr == 0 & 
                                      substring(cartinfo, 1,1) == "FIOSDATA" & fttpcapableflag == "Y" &
                        RO_visited==1 & OS_Visited==1))
    
    print(nrow(dsForView))
    
    t <- sort(table(dsForView$CREDIT_STATUS_DESCRIPTION), decreasing=TRUE)[1:10]
    
    
    
    #write.csv(t, "funnel/firstVistFlow.csv")
    barplot(t, main="First Order Page accessed during session - did buy", xlab="Page abbreviations", col="maroon")
    #ggplot(as.data.frame(t, aes(x=FIRST_VISITED_FLOW))) + geom_bar()
    
}

didnotbuy <- function(fname){
    
    marFile <- data.frame()
    marFile <- dSetRead(fname)
    
    dsForView <- subset(marFile, (substring(application_id, 1,1) == "N" & tracking_nbr == 0 & fttpcapableflag == "Y"))
    dsForView1 <- subset(marFile, (substring(application_id, 1,1) == "N" & tracking_nbr != 0 & fttpcapableflag == "Y"))
    
    didNotB <- dsForView$FIRST_VISITED_FLOW
    didB <- dsForView1$FIRST_VISITED_FLOW
    didDidNotBuy <- cbind(didB, didNotB)
    
    colnames(didDidNotBuy) <- c("Did Buy", "Did not buy")
    rownames(didDidNotBuy) <- dsForView$FIRST_VISITED_FLOW
    
    windows()
    barplot(t(didDidNotBuy), beside=T, ylab="number of species", 
            cex.names=0.8, las=2, ylim=c(0,120), col=c("darkblue","red"))
    box(bty="l")
    
    #t <- sort(table(dsForView$lastpagedropped), decreasing=TRUE)[1:10]
    
    #barplot(t, main="Pages from where max drop outs happen", xlab="Page abbreviations", col="light blue")
    #write.csv(dsForView, "Funnel/didnotbuy-March.csv")
}

didbuy <- function(fname){
    
    marFile <- data.frame()
    marFile <- dSetRead(fname)
    
    dsForView <- subset(marFile, (substring(application_id, 1,1) == "N" & tracking_nbr != 0 & fttpcapableflag == "Y"))
    
    #t <- sort(table(dsForView$lastpagedropped), decreasing=TRUE)[1:10]
    
    #barplot(t, main="Pages from where max drop outs happen", xlab="Page abbreviations", col="light blue")
    write.csv(dsForView, "Funnel/didbuy-March.csv")
}

channelUsage <- function(fname){
    marFile <- data.frame()
    marFile <- dSetRead(fname)
    
    dsForView <- subset(marFile, (substring(application_id, 1,1) == "N" & fttpcapableflag == "Y"))
    
    t <- table(dsForView$channel_name)
    
    barplot(t, main="Channels used for ordering", xlab="Channel Names", col= c("gray"))
    
    
}

channelUsageByWin <- function(fname){
    marFile <- data.frame()
    marFile <- dSetRead(fname)
    
    dsForView <- subset(marFile, (substring(application_id, 1,1) == "N" & fttpcapableflag == "Y" & tracking_nbr != 0))
    
    #t <- table(dsForView$channel_name)
    
    #barplot(t, main="Channels used for ordering", xlab="Channel Names", col= c("gray"))
    
    ggplot(dsForView, aes(x=channel_name)) + geom_bar()
}

stateView <- function(fname){
    marFile <- data.frame()
    marFile <- dSetRead(fname)
    
    dsForView <- subset(marFile, (substring(application_id, 1,1) == "N" & fttpcapableflag == "Y"))
    
    t <- table(dsForView$state)
    
    barplot(t, main="States", xlab="States", col= c("light blue"))
}


maxDropOutByChannel <- function(fname){
    marFile <- data.frame()
    marFile <- dSetRead(fname)
    
    dsForView <- subset(marFile, (substring(application_id, 1,1) == "N" & fttpcapableflag == "Y"))
    
    t <- table(dsForView$channel_name)
    
    barplot(t, main="Pages from where max drop outs happen", xlab="Page abbreviations", col= c("gray"), legend=rownames(t))
    
}

extractFebData <- function(fname){
    x <- as.data.frame(fread(fname, header=TRUE))
    str(x)
    x$Date <- as.POSIXct(as.character(x$Date), format="%Y-%m-%d %H:%M:%S")
    xfeb <- subset(x, CREATE_DATETIME >= "2016-02-23 00:00:00" & CREATE_DATETIME <= "2016-02-28 23:59:59")
    
    write.csv(xfeb, "febgood.csv")
}

plotdropoutfunnel <- function(fname){
    
    x <- as.data.frame(fread(fname, header=TRUE))
    
    dsFunnel <- data.frame()
    
    dsOAI <- subset(x, (substring(application_id, 1,1) == "N" & tracking_nbr == 0 & fttpcapableflag == "Y"))

    
    
    str(dsOAI)
    #dsFunnel <- c(Order_Addr_Info, SSB_or_DOL, CART_Visited, 
    #              Review_Order, Order_Summary)
    Names <- c("Order_Addr_Info", "SSBDOL_Visited", "CART_Visited", "Review_Order", "Order_Summary")
    Values <- c(sum(as.numeric(dsOAI$OAI_visited)), sum(as.numeric(dsOAI$SSBDOL_visited)), sum(as.numeric(dsOAI$CART_visited)), sum(as.numeric(dsOAI$RO_visited)), sum(as.numeric(dsOAI$OS_visited)))
                
    dsFunnel <- data.frame(Names, Values)
    
    write.csv(dsFunnel, "funnel/dsFunnelJan.csv")
    
}

plotFunnel <- function(){
    x <- read.csv("funnel/dsFunnel.csv", header=TRUE)
    ggplot(data=x, aes(x$Names)) + geom_histogram()
}


