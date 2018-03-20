##****************************
#Text Mining using R
# Author: Anand Mohan
# Compoany: Infosys Limited
# Date:Apr 26 2016
#****************************


# To be run only once
#Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(Needed, dependencies=TRUE)   

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library(tm)
library(data.table)
library(ggplot2)
library(SnowballC)
library(wordcloud) 

textMine <- function(filepath){
    docs <- Corpus(DirSource(cname))
    #Remove Punctuation
    docs <- remPunct(docs)
    write.csv(docs, "chat/t.csv")
}


getFilePath <- function(){
    
    cname <- file.path("ol") 
    docs <- Corpus(DirSource(cname))
    docs <- remPunct(docs)
    #summary(docs)
    
    #Staging the data
    dtm <- dataStage(docs)
    
    #data explore
    wf <-dataExpl(dtm)
    
    #plot
    plottextres(wf)
    
}

remPunct <- function(docs){
    #inspect(docs(1))
    docs <- tm_map(docs, removePunctuation)
    
    for(j in seq(docs))   
    {   
        docs[[j]] <- gsub("/", " ", docs[[j]])   
        docs[[j]] <- gsub("@", " ", docs[[j]])   
        docs[[j]] <- gsub("\\|", " ", docs[[j]])   
    }  
    
    docs <- tm_map(docs, removeNumbers)   
    docs <- tm_map(docs, tolower)
    
    # remove stopwords("english") 
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    docs <- tm_map(docs, removeWords, c("department", "email", "please", "can", "will"))
    
    docs <- tm_map(docs, stemDocument) 
    
    docs <- tm_map(docs, stripWhitespace)
    
    docs <- tm_map(docs, PlainTextDocument)
    
    
    return(docs)
}

dataStage <- function(docs){
    dtm <- DocumentTermMatrix(docs)
    return(dtm)
}

dataExpl <- function(dtm){
    freq <- colSums(as.matrix(dtm))
    length(freq)
    ord <- order(freq)
    m <- as.matrix(dtm)   
    dim(m)   
    #write.csv(m, file="dtm.csv") 
    
    dtms <- removeSparseTerms(dtm, 0.1)
    freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
    wf <- data.frame(word=names(freq), freq=freq) 
    return (wf)
    
}

plottextres <- function(wf){
    #p <- ggplot(subset(wf, freq>50), aes(word, freq))    
    #p <- p + geom_bar(stat="identity")   
    #p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
    #p 
    #wf <- subset(wf, freq>50)
    #wf
    set.seed(142)   
    #wordcloud(names(wf), freq, min.freq=100) 
    wordcloud(names(wf), max.words = 500, min.freq=10, random.order = FALSE, colors=pal2)
}