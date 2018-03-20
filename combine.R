library(data.table)
library(plotly)
tsample <- function(){
    
    #Read the Click Stream Data that we separated for March
    tdata <- as.data.frame(fread("ClickStream/bigCSData1.csv", nrows=10000, header=TRUE))
    #write.csv(tdata, "clickstream/tdata.csv")
    
    #Read the funnel data for March
    tfdata <- fread("clickstream/FunnelDataMarch.csv", header=TRUE)
    
    #Extract 2 columsn for the funnel data.
    tfdata <- as.data.frame(tfdata)[ ,c("GLOBAL_SESSION_ID", "tracking_nbr", "application_id", "fttpcapableflag")]
    tfdataTNum <- data.frame()
    
    #Make a dataset of all transactions successfully closed online
    tfdataTNum <- subset(tfdata, (tracking_nbr!="0" & substring(application_id, 1,1) == "N" & fttpcapableflag == "Y"))
    
    #print(nrow(tfdataTNum))
    
    # add a new column to know the Global ids that closed successfully in the clickstream file
    tdata[,"order_close"] <- tdata$Global_ID %in% tfdataTNum$GLOBAL_SESSION_ID
    
    write.csv(tdata, "clickstream/tempd.csv")
    
}



getWinLoseURL <- function(){
    adata <- as.data.frame(fread("ClickStream/t1data.csv", header=TRUE))
    aDataView <- subset(adata, (order_close == "TRUE"))
    t <- as.data.frame(sort(table(aDataView$URL), decreasing=TRUE)[1:50])
    write.csv(t, "clickstream/URLWin.csv")
    aDataView1 <- subset(adata, (order_close == "FALSE"))
    t1 <- as.data.frame(sort(table(aDataView1$URL), decreasing=TRUE)[1:50])
    write.csv(t1, "clickstream/URLLose.csv")
    
}

getWinLoseGID <- function(){
    adata <- as.data.frame(fread("ClickStream/t1data.csv", header=TRUE))
    aDataView <- subset(adata, (order_close == "TRUE"))
    t <- as.data.frame(sort(table(aDataView$Global_ID), decreasing=TRUE)[1:50])
    write.csv(t, "clickstream/GIDWin.csv")
    aDataView1 <- subset(adata, (order_close == "FALSE"))
    t1 <- as.data.frame(sort(table(aDataView1$Global_ID), decreasing=TRUE)[1:50])
    write.csv(t1, "clickstream/GIDLose.csv")
    
}

extractSuccessfulGUID <- function(fname, guid){
    tempDataSet <- as.data.frame(fread(fname, nrows=8400000, header=TRUE))
    dguid <- tempDataSet[ which(tempDataSet$Global_ID == guid), ]
    write.csv(dguid, "guidCS2.csv")
}

dsMerge <- function(f1, f2, mVar){
    
    f11 <- read.csv(f1, header=TRUE)
    f22 <- read.csv(f2, header=TRUE)
    f <- merge(f11, f22, by=mVar)
    write.csv(f, "clickstream/URLWinLose.csv")
}


scatterplot <- function(){
    
    
    #temp <- read.csv("clickstream/guidCS.csv", header=TRUE)
    
    #t <- sort(table(temp$PAGE_NAME), decreasing=TRUE)[1:10]
    
    #barplot(t, main="Pages visited by a user who closed a transaction", xlab="Page Names", col="light blue")

    x <- read.csv("ClickStream/guidCS2.csv", header=TRUE)
    #plot_ly(data=x, x = CREATE_DATETIME, y = PAGE_NAME, mode = "markers")
    ggplot(x, aes(x=CREATE_DATETIME, y=PAGE_NAME)) +
        geom_point(shape=19)
}


#barplot(t, main="Pages from where max drop outs happen", xlab="Page abbreviations", col="light blue")


