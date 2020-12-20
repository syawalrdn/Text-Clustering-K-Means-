#installPackages
install.packages("tm")
install.packages("proxy")
install.packages("string")
install.packages("devtools")
library(devtools)
install.packages("katadasaR")
install_github("nurandi/katadasaR")
install.packages("wordcloud")
install.packages("cluster")
install.packages("factoextra")
install.packages("tau")
install.packages("parallel")
install.packages("ggplot2")

#importLibrary
library(tm)
library(proxy)
library(stringr)
library(katadasaR)
library(wordcloud)
library(cluster)
library(factoextra)
library(tau)
library(parallel)
library(ggplot2)
library(dplyr)

#cariLokasi&bacaFile
setwd("F:/.TEKNIK INFORMATIKA/GEMASTIK XIII")
data_tweets<-read.csv("dataset.csv", stringsAsFactors = TRUE, sep=";")

#corpusdokumen
corpus_tweets <- Corpus(VectorSource(data_tweets$Text))
inspect(corpus_tweets[1:3])

#CaseFolding
casefolding_tweets <- tm_map(corpus_tweets, content_transformer(tolower))
inspect(casefolding_tweets[1:3])

#TextCleaning(HapusURL)
hapusURL <- function(x) gsub("http[^[:space:]]*", "", x)
URL_tweets <- tm_map(casefolding_tweets, content_transformer(hapusURL))
inspect(URL_tweets[1:3])

#TextCleaning(HapusMention)
remove.mention <- function(x) gsub("@\\S+", "", x)
mention_tweets <- tm_map(URL_tweets, remove.mention)
inspect(mention_tweets[1:3])

#TextCleaning(HapusHashtag)
remove.hashtag <- function(x) gsub("#\\S+", "", x)
hashtag_tweets <- tm_map(mention_tweets, remove.hashtag)
inspect(hashtag_tweets[1:3])

#TextCleaning(HapusTandaBaca)
hapus_tandabaca<-tm_map(hashtag_tweets,content_transformer(removePunctuation))
inspect(hapus_tandabaca[1:3])
                                    
#TextCleaning(HapusAngka)
hapus_angka<-tm_map(hapus_tandabaca, content_transformer(removeNumbers))
inspect(hapus_angka[1:3])

#TextCleaning(HapusSimbol)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
hapus_simbol <- tm_map(hapus_angka, toSpace, "â???¦")
inspect(hapus_simbol[1:10])

#TextCleaning(SlangWord)
slang <- read.csv("slangword_list.csv", header=T)
old_slang <- as.character(slang$old)
new_slang <- as.character(slang$new)
slangword<-function(x)Reduce(function(x,r)gsub(slang$old[r],slang$new[r],x,fixed=T),seq_len(nrow(slang)),x)
slangword_tweets <- tm_map(hapus_simbol,slangword)
inspect(slangword_tweets[1:3])

#Stemming
stem_text<-function(text,mc.cores=1)
{
  stem_string<-function(str)
  {
    str<-tokenize(x=str)
    str<-sapply(str,katadasaR)
    str<-paste(str,collapse = "")
    return(str)
  }
  x<-mclapply(X=text,FUN=stem_string,mc.cores=mc.cores)
  return(unlist(x))
}
stemming_tweets<-tm_map(slangword_tweets,stem_text)
inspect(stemming_tweets[1:3]) 

#StopWord
cStopwordID<-readLines("stopwords.csv")
stopword_tweets <- tm_map(stemming_tweets, removeWords, cStopwordID)
inspect(stopword_tweets[1:3])

#HasilPreprocessing(HapusSpasi)
whitespace_tweets<-tm_map(stopword_tweets,stripWhitespace)
inspect(whitespace_tweets[1:3])

#exportHasilPreprocessing
databersih<-data.frame(text=unlist(sapply(whitespace_tweets,`[`)), tringsAsFactors=F)
write.csv(databersih,file="databersih.csv")

#HitungDocumentFrequency
dtm <- TermDocumentMatrix(whitespace_tweets)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#BuatWordCLoudDF
dtm <- TermDocumentMatrix(whitespace_tweets)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          
          colors=brewer.pal(8, "Dark2"))

#HitungBobotTFIDF
tdm <- DocumentTermMatrix(whitespace_tweets)
inspect(tdm[1:9, 1:10])

#HitungNilaiTFIDF
tdm.tfidf <- weightTfIdf(tdm)
inspect(tdm.tfidf[1:9, 1:10])

#MembuatTabelTFIDF
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.99)
tfidf.matrix <- as.matrix(tdm.tfidf)
View(tfidf.matrix)


#HitungJarakCentroid
dist.matrix = dist(tfidf.matrix, method = "cosine")
dist_centroid <-as.matrix(dist.matrix)

#PenentuanKMeansCLuster
kmeans_cluster <- kmeans(dist_centroid, 5, nstart = 25)
print(kmeans_cluster)

#CLusterPlot
fviz_cluster(kmeans_cluster, data = dist_centroid)

#cariTopWord5Klaster
k <- 5
kMC <- kmeans(tfidf.matrix, centers = k)
str(kMC)
valueMatrix <-as.data.frame(kMC$centers)

changed_ValueMatrix <- as.data.frame(t(valueMatrix))
View(changed_ValueMatrix)


top5_values1 <- head(sort(changed_ValueMatrix$`1`,decreasing = TRUE),5)
words1 <- c()
for (i in top5_values1){
  clus1 <- rownames(changed_ValueMatrix)[changed_ValueMatrix$`1`== i]
  words1 <- append(words1,clus1)
}
Cluster1_words <- as.data.frame(cbind(words1,top5_values1))
write.table(Cluster1_words, "Cluster1_words.txt")

top5_values2 <- head(sort(changed_ValueMatrix$`2`,decreasing = TRUE),5)
words2 <- c()
for (i in top5_values2){
  clus2 <- rownames(changed_ValueMatrix)[changed_ValueMatrix$`2`== i]
  words2 <- append(words2,clus2)
}
Cluster2_words <- as.data.frame(cbind(words2,top5_values2))
write.table(Cluster2_words, "Cluster2_words.txt")

top5_values3 <- head(sort(changed_ValueMatrix$`3`,decreasing = TRUE),5)
words3 <- c()
for (i in top5_values3){
  clus3 <- rownames(changed_ValueMatrix)[changed_ValueMatrix$`3`== i]
  words3 <- append(words3,clus3)
}
Cluster3_words <- as.data.frame(cbind(words3,top5_values3))
write.table(Cluster3_words, "Cluster3_words.txt")


top5_values4 <- head(sort(changed_ValueMatrix$`4`,decreasing = TRUE),5)
words4 <- c()
for (i in top5_values4){
  clus4 <- rownames(changed_ValueMatrix)[changed_ValueMatrix$`4`== i]
  words4 <- append(words4,clus4)
}
Cluster4_words <- as.data.frame(cbind(words4,top5_values4))
write.table(Cluster4_words, "Cluster4_words.txt")

top5_values5 <- head(sort(changed_ValueMatrix$`5`,decreasing = TRUE),5)
words5 <- c()
for (i in top5_values5){
  clus5 <- rownames(changed_ValueMatrix)[changed_ValueMatrix$`5`== i]
  words5 <- append(words5,clus5)
}
Cluster5_words <- as.data.frame(cbind(words5,top5_values5))
write.table(Cluster5_words, "Cluster5_words.txt")
