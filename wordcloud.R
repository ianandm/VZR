library(tm)
library(SnowballC)

library(wordcloud) 

jeopQ <- as.data.frame(fread('chat/ol/aims_chat_file_date_012816_created_2016_03_23_13_14_13_64.txt', stringsAsFactors = FALSE))

#head(jeopQ)

#write.csv(jeopQ, "chat/jeopq.csv")

#jeopQ <- Corpus(DirSource(cname))

#jeopCorpus <- Corpus(VectorSource("chat/ol/tempdata.txt"))

jeopCorpus <- Corpus(VectorSource(jeopQ$V16))
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, removeWords, c('the', 'this', 'that', 'chat', 'will', 'thank', 'Verizon', 'number', 'can', 'service', 'okay', 'you', 'need', 'may', 'Yes', 'email', stopwords('english')))
jeopCorpus <- tm_map(jeopCorpus, removeWords, c('the', 'this', 'that', 'chat', 'will', 'Thank', 'Verizon', 'number', 'can', 'service', 'okay', 'You', 'need', 'may', 'yes', 'Please'))
jeopCorpus <- tm_map(jeopCorpus, removeWords, c('first', 'account', 'agent', 'bill', 'inform', 'session', 'back', 'still', 'sent', 'image', 'set', 'Day', 'need', 'Chat', 'Servic', 'please'))


jeopCorpus <- tm_map(jeopCorpus, stemDocument)

wordcloud(jeopCorpus, max.words = 500, min.freq=10, random.order = FALSE, colors=pal2)