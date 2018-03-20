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
    
    
    #ol <- as.data.frame(fread("chat/ol/export_32276_2015_01_01-2016_04_07.csv", header=TRUE, stringsAsFactors=FALSE))
    
    
    #sample <- Corpus(VectorSource(ol$Comments))
    
    
    #sample <- read.delim(file="chat/jan/test.txt", header=FALSE, stringsAsFactors=FALSE)
    #sample <- paste(readLines("chat/jan/aims_chat_file_date_010116_created_2016_03_21_10_23_58_162.txt"), collapse=" ")
    #sample <- unlist(strsplit(sample, split=","))
    #str(sample)
    cname <- file.path("Forum/Forum") 
    sample <- Corpus(DirSource(cname))
    #docs <- VCorpus(VectorSource(docs))
    #docs <- remPunct(sample)
    sample <- unlist(strsplit(as.character(sample), split=","))
    str(sample)
    #sample <-  paste(scan("chat/Jan/test.txt", what="character", sep=" "), collapse=" ")
    
    sc <- score.sentiment(sample, VZ.pos, VZ.neg, .progress='text')
    #print(sc$score)
   
    #hist(sc$score)
    
    #head(sc)
    
    ggplot(sc, aes(x = score), stat = "bin") + geom_histogram(bins=100, binwidth = 0.1)
    
    #print(head(sc))
    
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
    
    docs <- tm_map(docs, stemDocument) 
    
    docs <- tm_map(docs, stripWhitespace)
    
    docs <- tm_map(docs, PlainTextDocument)
    
    
    return(as.character(docs))
}


