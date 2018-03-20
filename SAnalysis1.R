library(tm)
library(data.table)
library(ggplot2)
library(SnowballC)
library(wordcloud) 
SentimentAnalysis <- function(){
    # read the positive and negative words from the files
    VZ.pos = scan('chat/positive-words.txt',
                  what='character', comment.char=';')
    
    VZ.neg = scan('chat/negative-words.txt',
                  what='character', comment.char=';')
    
    file_list <- list.files(path="chat/Apr", pattern=".csv", full.names=TRUE)
    
    for(file in file_list){
        
        if(!exists("cData")){
            tData <- as.data.frame(fread(file, stringsAsFactors = FALSE))
            tData <- subset(tData, (V10 == "Verizon Internet Services" & V15 == "eCenter Sales"), select=c(V16))
            cData <- as.data.frame(tData)
            rm(tData)
        }
        else{
            
            temp_data <- as.data.frame(fread(file, stringsAsFactors = FALSE))
            temp_data <- subset(temp_data, (V10 == "Verizon Internet Services" & V15 == "eCenter Sales"), select=c(V16))
            cData <- rbind(cData, temp_data)
            rm(temp_data)
        }
            
        
    }

    write.csv(cData$V16, "chat/AprChatData.csv")
    
    
    #cData <- as.data.frame(fread('chat/Jan/aims_chat_file_date_010116_created_2016_03_21_10_23_58_162.txt', stringsAsFactors = FALSE))
    
    #cData1 <- subset(cData, (V10 == "Verizon Internet Services" & V15 == "eCenter Sales"))
    
    #Convert data frame to a corpus
    
    sample <- Corpus(VectorSource(as.character(cData))) 
    
    
    #sample <- read.delim(file="chat/jan/test.txt", header=FALSE, stringsAsFactors=FALSE)
    #sample <- paste(readLines("chat/jan/aims_chat_file_date_010116_created_2016_03_21_10_23_58_162.txt"), collapse=" ")
    #sample <- unlist(strsplit(sample, split=","))
    #str(sample)
    #cname <- file.path("Chat/Jan") 
    #sample <- Corpus(DirSource(cname))
    #docs <- VCorpus(VectorSource(docs))
    #docs <- remPunct(sample)
    sample <- unlist(strsplit(as.character(sample), split=","))
    str(sample)
    #sample <-  paste(scan("chat/Jan/test.txt", what="character", sep=" "), collapse=" ")
    
    sc <- score.sentiment(sample, VZ.pos, VZ.neg, .progress='text')
    #print(sc$score)
    
    #hist(sc$score)
    
    #head(sc)
    
    ggplot(sc, aes(x = score), stat = "bin") + geom_histogram(bins=200, binwidth = 0.1)
    #wordcloud(sc$text, max.words = 100, min.freq=50, random.order = FALSE)
    
    #print(head(sc))
    
}


VZSA <- function(){
    
    VZ.words = scan('chat/VZwords.txt',
                  what='character', comment.char=';')
    
    file_list <- list.files(path="chat/Jan", pattern=".txt", full.names=TRUE)
    
    for(file in file_list){
        
        if(!exists("cData")){
            tData <- as.data.frame(fread(file), stringsAsFactors = FALSE, full.names=TRUE)
            tData <- subset(tData, (V10 == "Verizon Internet Services" & V15 == "eCenter Sales"))
            cData <- tData$V16
            rm(tData)
        }
        else{
            
            temp_data <- as.data.frame(fread(file), stringsAsFactors = FALSE)
            temp_data <- subset(temp_data, (V10 == "Verizon Internet Services" & V15 == "eCenter Sales"))
            cData <- cbind(cData, temp_data$V16)
            rm(temp_data)
        }
    }
    
    sample <- Corpus(VectorSource(as.character(cData))) 
    
    sample <- unlist(strsplit(as.character(sample), split=","))
    str(sample)
    
    sc <- score.sentimentVZ(sample, VZ.words, .progress='text')
    
    ggplot(sc, aes(x = score), stat = "bin") + geom_histogram(bins=200, binwidth = 0.1)
    
}

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none'){
    
    require(plyr)
    require(stringr)
    
    #sentences <- as.character(sentences)
    
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
        
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        # and convert to lower case:
        sentence = tolower(sentence)
        
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
        
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
        
        return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=unlist(sapply(sentences, '[')), stringsAsFactors=F)
    return(scores.df)
}


score.sentimentVZ <- function(sentences, VZ.words, .progress='none'){
    
    require(plyr)
    require(stringr)
    
    #sentences <- as.character(sentences)
    
    scores = laply(sentences, function(sentence, VZ.words) {
        
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        # and convert to lower case:
        sentence = tolower(sentence)
        
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
        
        # compare our words to the dictionaries of positive & negative terms
        VZ.matches = match(words, VZ.words)
        #neg.matches = match(words, neg.words)
        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        VZ.matches = !is.na(VZ.matches)
        #neg.matches = !is.na(neg.matches)
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(VZ.matches)
        
        return(score)
    }, VZ.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=unlist(sapply(sentences, '[')), stringsAsFactors=F)
    return(scores.df)
}

remPunct <- function(docs){
    #inspect(docs(1))
    
    #docs <- Corpus(VectorSource(docs))
    
    
    
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
    
    docs <- tm_map(docs, removeWords, c('the', 'this', 'that', 'chat', 'will', 'Thank', 'Verizon', 'number', 'can', 'service', 'okay', 'You', 'need', 'may', 'yes', 'Please'))
    docs <- tm_map(docs, removeWords, c('first', 'account', 'agent', 'bill', 'inform', 'session', 'back', 'still', 'sent', 'image', 'set', 'Day', 'need', 'Chat', 'Servic', 'please'))
    
    docs <- tm_map(docs, stemDocument) 
    
    docs <- tm_map(docs, stripWhitespace)
    
    docs <- tm_map(docs, PlainTextDocument)
    
    
    return(as.character(docs))
}


