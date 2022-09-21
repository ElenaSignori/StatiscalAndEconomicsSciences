
library(readxl)
covidvac <- read_xlsx("covidvac_definitivo.xlsx", col_types = c("date", "text", "text"))
covidvac <- covidvac[,-3]
covidvac.original <- covidvac
#covidvac <- covidvac.original

n <- nrow(covidvac.original)

##### CREAZIONE DEL CORPUS ####
library(tidyverse)
covidvac <- as_tibble(covidvac)
covidvac <- separate(covidvac, date, into = c("date", "time"), sep=" ")
covidvac <- as.data.frame(covidvac[,-2])
covidvac <- cbind(covidvac, covidvac[,1])[,-1]
covidvac <- cbind(seq(1:nrow(covidvac)), covidvac)
colnames(covidvac) <- c("doc_id", "text", "date")

                  
#library(corpus)
#covid.corpus <- corpus_frame(title=covidvac$date, text=covidvac$content)
#covid.corpus.original <- covid.corpus 
#rm(list=("covid.corpus"))

library(tm)
#library(qdap)
covid.corpus <- Corpus(DataframeSource(covidvac))
covid.corpus.original <- covid.corpus
#covid.corpus <- covid.corpus.original

writeLines(as.character(covid.corpus[15]))


#####  PULIZIA DEL CORPUS  #####
covid.corpus <- tm_map(covid.corpus, content_transformer(tolower))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeTAG <- function(x) gsub("@[^[:space:]]*", "", x)
removeHASHTAG <- function(x) gsub("#[^[:space:]]*", "", x)
covid.corpus <- tm_map(covid.corpus, content_transformer(removeURL))
covid.corpus <- tm_map(covid.corpus, content_transformer(removeTAG))
covid.corpus <- tm_map(covid.corpus, content_transformer(removeHASHTAG))
#covid.corpus <- bracketX(covid.corpus)
#covid.corpus <- replace_abbreviation(covid.corpus)

my.stopwords <- c(stopwords("english"), "covidvaccine", "cant", "get", "got", "will", "still", "amp", "can",
                  "just", "jab", "india", "need", "via", "given", "give", "tell", "told", "every", "since",
                  "try", "yet", "also", "even", "let", "next", "use", "want", "getting", "come", "see", 
                  "say", "don't", "can't", "jab")
covid.corpus <- tm_map(covid.corpus, content_transformer(removeWords), my.stopwords)

toSpace <- function(x, symbol) {gsub(symbol, " ", x)}
covid.corpus <- tm_map(covid.corpus, toSpace, "â")
covid.corpus <- tm_map(covid.corpus, toSpace, "€")
covid.corpus <- tm_map(covid.corpus, toSpace, "™")
covid.corpus <- tm_map(covid.corpus, toSpace, "ð")
covid.corpus <- tm_map(covid.corpus, toSpace, "ÿ")
covid.corpus <- tm_map(covid.corpus, toSpace, "‘")
covid.corpus <- tm_map(covid.corpus, toSpace, "‡")
covid.corpus <- tm_map(covid.corpus, toSpace, "…")
covid.corpus <- tm_map(covid.corpus, toSpace, "¦")
covid.corpus <- tm_map(covid.corpus, toSpace, "ª")
covid.corpus <- tm_map(covid.corpus, toSpace, "¤")
covid.corpus <- tm_map(covid.corpus, toSpace, "½")
covid.corpus <- tm_map(covid.corpus, toSpace, "ï")
covid.corpus <- tm_map(covid.corpus, toSpace, "\u008f")
covid.corpus <- tm_map(covid.corpus, toSpace, "‰")
covid.corpus <- tm_map(covid.corpus, toSpace, "¼")
covid.corpus <- tm_map(covid.corpus, toSpace, "’")
covid.corpus <- tm_map(covid.corpus, toSpace, "˜")
covid.corpus <- tm_map(covid.corpus, toSpace, "‚")
covid.corpus <- tm_map(covid.corpus, toSpace, "“")
covid.corpus <- tm_map(covid.corpus, content_transformer(removeNumbers))
covid.corpus <- tm_map(covid.corpus, content_transformer(removePunctuation))


covid.corpus <- tm_map(covid.corpus, content_transformer(stripWhitespace))
covid.corpus.pulito <- covid.corpus
#covid.corpus <- covid.corpus.pulito


#writeLines(as.character(covid.corpus[20]))

covid.corpus <- tm_map(covid.corpus, stemDocument, language="english")
#covid.corpus <- tm_map(covid.corpus[20], stemCompletion, covid.corpus.pulito)


#####  ANALISI INIZIALI #####
set.seed(456)
tdm <- TermDocumentMatrix(covid.corpus)#, control=list(wordLengths=c(3,30)) ) #, bounds=list(global = c(80, 840)))
#dim(tdm)
tdm.nos <- removeSparseTerms(tdm, 0.996) 
tdm.def <- as.TermDocumentMatrix(tdm.nos)
dim(tdm.def) #437 497644
#as.matrix(tdm.def)

dimnames(tdm.def)$Terms 
rownames(tdm.def) <- stemCompletion(rownames(tdm.def), covid.corpus.pulito)
  tm_map(dimnames(tdm.def)$Terms, stemCompletion, covid.corpus.pulito)

tdm.sum <- rowSums(as.matrix(tdm.def))
tdm.v <- sort(tdm.sum, decreasing=T)
tdm.wo <- tdm.v[1:200]
#freq <- colSums(as.matrix(tdm.def))
#freq <- sort(freq, decreasing = T)
#findFreqTerms(tdm.def, lowfreq = 80)

library(ggplot2)
df <- data.frame(term=names(tdm.v), occurrences=tdm.v)
pl <- ggplot(subset(df, occurrences>10000) , aes(term, occurrences)) + 
  geom_bar(stat="identity", fill="lightblue", col="lightsteelblue")+ 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size=15)) + ggtitle("Most frequent terms")
pl    

df[which(df$occurrences>7000),]
names(tdm.v)[1:50] <- c("vaccine", "covid", "dose", "people", "first", "today", "now", "one", 
                        "day", "thank", "shot", "take", "receive", "like", "show", "work", "help",
                        "health",  "week"  ,"time", "make", "please", "effect",  "know","second",
                        "appointment", "new"  ,"year" , "slot" , "feel"  , "book", "good","safe", 
                        "country" ,"protect", "available", "state", "live", "great" , "many",   
                        "done", "pfizer"  ,"news","wait", "hope", "everyone", "care", "say",
                        "start", "call")
library(wordcloud)
wordcloud(names(tdm.v), tdm.v, min.freq = 7000, max.words=200, random.color = T, colors = rainbow(10),
          fixed.asp = T, rot.per = 0.30, random.order=F)


##### SENTIMENT SCORE #####
library(syuzhet)
#bing <- get_sentiment(covidvac.original$content, method="bing")
#afinn <- get_sentiment(covid.corpus.pulito, method="afinn")


##### EMOTION CLASSIFICATION #####
nrc <- get_nrc_sentiment(covidvac.original$content, language="english")
nrc.df <- data.frame(t(nrc))
#nrc.df[,2:5]
nrc.new <- rowSums(nrc.df)
nrc.new <- as.data.frame(nrc.new)
nrc.def <- cbind(rownames(nrc.new), as.numeric(nrc.new[,1]))
nrc.def <- as.data.frame(nrc.def)
colnames(nrc.def) <- c("sentiment", "tot")

which.max(nrc.def$tot[1:8])

nrc.graf <- nrc.def[1:8,] 
quickplot(sentiment, data=nrc.graf, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count", 
          ylim =c(0,360000) )+ ggtitle("Survey sentiments")

barplot(
  sort(colSums(prop.table(nrc[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",
  xlim=c(0, 0.25), col = palette("Pastel 1")[1:8]
)

#covid.corpus.original$content[which(nrc$trust>0)]

# grafico temporale
sent <- as.numeric((nrc[,10])-(nrc[,9]))
text <- c()
for(i in 1:n) {text <- c(text, covid.corpus.pulito[[i]]$content)}

covidvac.tibble <- as_tibble(cbind(date=covidvac$date, text, sent))
head(covidvac.tibble)
covidvac.tibble[,1] <- as.Date(covidvac.tibble[,1]$date)
covidvac.tibble[,3] <- sent

sintesi <- covidvac.tibble[,-2] %>%
              group_by(date) %>%
              summarise(sentiment=sum(sent))
sintesi <- sintesi[-1,]
barplot(sintesi$tot)

time.pl <- ggplot(data = sintesi, mapping = aes(date,sentiment))+
              geom_col(fill="lightseagreen" , color="black")+ 
              geom_smooth(col= "red2", se=F) +
              scale_x_date(date_breaks = "2 weeks")+
              theme(axis.text.x = element_text(angle=45,hjust = 1))+
              labs(title="#covidvaccine")
time.pl
colors()

#####  CLUSTERING #####
tdm.clus <- removeSparseTerms(tdm, 0.993) 
tdm.clus <- t(as.matrix(tdm.clus))
tdm.clus.orig <-  tdm.clus#       t(as.matrix(tdm.def))#t(tdm.clus)
tdm.clus <- tdm.clus.orig

set.seed(345)
elim <- sample(1:n, 350000)
tdm.clus <- tdm.clus.orig[-elim,]
dim(tdm.clus)


library(proxy)
library(cluster)
library(lsa)
dis.c <- dist((tdm.clus), method = "cosine")
dis.c <- cosine(tdm.clus)

dis.e <- dist(tdm.clus)
dist(tdm.clus)
rm(list="sintesi", "covidvac", "covid.corpus", "tdm.nos", "sent", "tdm.sum", "tdm.v", "my.stopwords")
dim(dis.c)

test =c(1:8)
wh.vec <- c()
for(i in 1:length(test)){km <- kmeans(tdm.clus, centers = test[i], nstart=50, iter.max = 25)
                         wh <- km$tot.withinss
                         wh.vec <- c(wh.vec, wh) }

plot(1:length(test), wh.vec,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

km5 <- kmeans(tdm.clus, centers = 5, nstart=50, iter.max = 50)
sil <- silhouette(km$cluster, dist = dis.c)
sil.vec <-sum(sil[,3])/length(km$cluster)
test[which.max(sil.vec)]



cluplot2 <- clusplot(tdm.clus, km2$cluster, color=T, shade=T, labels=F, lines=0, 
                     col.clus =c("deepskyblue2", "orange2"),#, "chartreuse3","orangered2","hotpink1"),
                     col.p = "black", cex =1, main = "Clusplot #covidvaccine")

colors()

cluplot2
cluplot5

clus1 <- covidvac.original$content[which(km2$cluster==1)]
clus2 <- covidvac.original$content[which(km2$cluster==2)]



nrc.cl1 <- get_nrc_sentiment(clus1, language="english") 
nrc.df.cl1 <- data.frame(t(nrc.cl1))
nrc.new1 <- rowSums(nrc.df.cl1)
nrc.new1 <- as.data.frame(nrc.new1)
nrc.def1 <- cbind(rownames(nrc.new1), as.numeric(nrc.new1[,1]))
nrc.def1 <- as.data.frame(nrc.def1)
colnames(nrc.def1) <- c("sentiment", "tot")
nrc.graf1 <- nrc.def1[1:8,] 
quickplot(sentiment, data=nrc.graf1, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 1")
colSums(prop.table(nrc.cl1[, 1:8]))
#anger      anticipation      disgust     fear          joy        sadness     surprise        trust 
# 0.10153021   0.16047909   0.05553208   0.14248887   0.11513003   0.11247865   0.07866586   0.23369522 
length(clus1) #56225


nrc.cl2 <- get_nrc_sentiment(clus2, language="english") 
nrc.df.cl2 <- data.frame(t(nrc.cl2))
nrc.new2 <- rowSums(nrc.df.cl2)
nrc.new2 <- as.data.frame(nrc.new2)
nrc.def2 <- cbind(rownames(nrc.new2), as.numeric(nrc.new2[,1]))
nrc.def2 <- as.data.frame(nrc.def2)
colnames(nrc.def2) <- c("sentiment", "tot")
nrc.graf2 <- nrc.def2[1:8,] 
quickplot(sentiment, data=nrc.graf2, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 2")
colSums(prop.table(nrc.cl2[, 1:8]))
#  anger    anticipation      disgust         fear          joy      sadness     surprise        trust 
# 0.10138180   0.16255048   0.05454762   0.14279504   0.11388826   0.11240948   0.07878770   0.23363962
length(clus2) # 91419

covidvac.original$content[which(km2$cluster==1)][1:5]
id <- c(1:56225)
is.character(clus1)
df.1 <- as.data.frame(cbind(doc_id=id, text=as.character(clus1)))
colnames(df.1)
corp1 <- Corpus(DataframeSource(df.1))
corp1 <- tm_map(corp1, content_transformer(tolower))
corp1 <- tm_map(corp1, content_transformer(removeURL))
corp1 <- tm_map(corp1, content_transformer(removeTAG))
corp1 <- tm_map(corp1, content_transformer(removeHASHTAG))
corp1 <- tm_map(corp1, content_transformer(removeWords), my.stopwords)
corp1 <- tm_map(corp1, content_transformer(removeNumbers))
corp1 <- tm_map(corp1, content_transformer(removePunctuation))
corp1 <- tm_map(corp1, content_transformer(stripWhitespace))
corp1 <- tm_map(corp1, stemDocument, language="english")
tdm1 <- TermDocumentMatrix(corp1)
tdm1 <- removeSparseTerms(tdm1, 0.995)
tdm1.sum <- rowSums(as.matrix(tdm1))
tdm1.v <- sort(tdm1.sum, decreasing=T)
names(tdm1.v)[80:150]

covidvac.original$content[which(km2$cluster==2)][1:5]


covidvac.original$content[which(km2$cluster==1)][1:5]
id2 <- c(1:91419)
is.character(clus1)
df.2 <- as.data.frame(cbind(doc_id=id2, text=as.character(clus2)))
colnames(df.2)
corp2 <- Corpus(DataframeSource(df.2))
corp2 <- tm_map(corp2, content_transformer(tolower))
corp2 <- tm_map(corp2, content_transformer(removeURL))
corp2 <- tm_map(corp2, content_transformer(removeTAG))
corp2 <- tm_map(corp2, content_transformer(removeHASHTAG))
corp2 <- tm_map(corp2, content_transformer(removeWords), my.stopwords)
corp2 <- tm_map(corp2, content_transformer(removeNumbers))
corp2 <- tm_map(corp2, content_transformer(removePunctuation))
corp2 <- tm_map(corp2, content_transformer(stripWhitespace))
corp2 <- tm_map(corp2, stemDocument, language="english")
tdm2 <- TermDocumentMatrix(corp2)
tdm2 <- removeSparseTerms(tdm2, 0.995)
tdm2.sum <- rowSums(as.matrix(tdm2))
tdm2.v <- sort(tdm2.sum, decreasing=T)
names(tdm2.v)[80:150]

ggplot(subset(tdm2.v, tdm2.v>1500) , aes(tdm2.v)) + 
  geom_bar(stat="identity", fill="lightblue", col="lightsteelblue")+ 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size=15)) + ggtitle("Most frequent terms")

library(factoextra)
fviz_cluster(km2, data = tdm.clus,
             palette = c("#2E9FDF", "#E7B800"),# "#E7B800",  "#FED9A6" ), #00AFBB
             geom = "point", pointsize = 0.8,
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

rm(list="sintesi", "covidvac", "covid.corpus", "tdm.nos", "sent", "tdm.sum", "tdm.v", "my.stopwords",
   "covid.corpus.original", "covid.corpus.pulito", "covidvac.tibble", "km", "km5", "nrc.def1", 
   "nrc.def2", "nrc.graf1", "nrc.graf2", "tdm", "tdm.clus", "tdm.def", "time.pl", "elim", "text"   )

#______________________________________________________________________________________________

##### PFIZER ######

pfizer <- read_xlsx("Pfizer_definitivo.xlsx", col_types = c("date", "text", "text"))
pfizer <- pfizer[,-3]
pfizer.original <- pfizer
#pfizer <- pfizer.original

n.p <- nrow(pfizer.original)

##### CREAZIONE DEL CORPUS ####
pfizer <- as_tibble(pfizer)
pfizer <- separate(pfizer, date, into = c("date", "time"), sep=" ")
pfizer <- as.data.frame(pfizer[,-2])
pfizer <- cbind(pfizer, pfizer[,1])[,-1]
pfizer <- cbind(seq(1:nrow(pfizer)), pfizer)
colnames(pfizer) <- c("doc_id", "text", "date")

pfizer.corpus <- Corpus(DataframeSource(pfizer))
pfizer.corpus.original <- pfizer.corpus
#pfizer.corpus <- pfizer.corpus.original

#writeLines(as.character(pfizer.corpus[15]))


#####  PULIZIA DEL CORPUS  #####
pfizer.corpus <- tm_map(pfizer.corpus, content_transformer(tolower))
pfizer.corpus <- tm_map(pfizer.corpus, content_transformer(removeURL))
pfizer.corpus <- tm_map(pfizer.corpus, content_transformer(removeTAG))
pfizer.corpus <- tm_map(pfizer.corpus, content_transformer(removeHASHTAG))

pf.stopwords <- c(stopwords("english"), "covidvaccine", "cant", "get", "got", "will", "still", "amp", "can",
                  "just", "jab", "india", "need", "via", "given", "give", "tell", "told", "every", "since",
                  "try", "yet", "also", "even", "let", "next", "use", "want", "getting", "come", "see", 
                  "say", "don't", "can't", "jab", "pfizer")
pfizer.corpus <- tm_map(pfizer.corpus, content_transformer(removeWords), pf.stopwords)

pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "â")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "€")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "™")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "ð")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "ÿ")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "‘")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "‡")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "…")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "¦")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "ª")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "¤")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "½")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "ï")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "\u008f")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "‰")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "¼")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "’")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "˜")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "‚")
pfizer.corpus <- tm_map(pfizer.corpus, toSpace, "“")
pfizer.corpus <- tm_map(pfizer.corpus, content_transformer(removeNumbers))
pfizer.corpus <- tm_map(pfizer.corpus, content_transformer(removePunctuation))


pfizer.corpus <- tm_map(pfizer.corpus, content_transformer(stripWhitespace))
pfizer.corpus.pulito <- pfizer.corpus
#pfizer.corpus <- pfizer.corpus.pulito

pfizer.corpus <- tm_map(pfizer.corpus, stemDocument, language="english")


#####  ANALISI INIZIALI #####

p.tdm <- TermDocumentMatrix(pfizer.corpus)#, control=list(wordLengths=c(3,30)) ) #, bounds=list(global = c(80, 840)))
dim(p.tdm)
p.tdm.nos <- removeSparseTerms(p.tdm, 0.996)
p.tdm.def <- as.TermDocumentMatrix(p.tdm.nos)
dim(p.tdm.def)

dimnames(p.tdm.def)$Terms 
rownames(p.tdm.def) <- tm_map(dimnames(p.tdm.def)$Terms, stemCompletion, covid.corpus.pulito)


p.tdm.v <- sort(rowSums(as.matrix(p.tdm.def)), decreasing=T)

p.df <- data.frame(term=names(p.tdm.v), occurrences=p.tdm.v)
p.pl <- ggplot(subset(p.df, occurrences>2000) , aes(term, occurrences)) + 
  geom_bar(stat="identity", fill="lightblue", col="lightsteelblue")+ 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size=15)) + ggtitle("Most frequent terms")
p.pl    


wordcloud(names(p.tdm.v), p.tdm.v, min.freq = 2000, max.words=200, random.color = T, colors = rainbow(10),
          fixed.asp = T, rot.per = 0.30, random.order=F)
pf.fterms <- p.df[which(p.df$occurrences>2000),]

##### EMOTION CLASSIFICATION #####
p.nrc <- get_nrc_sentiment(pfizer.original$content, language="english") 
p.nrc.df <- data.frame(t(p.nrc))
p.nrc.df[,2:5]
p.nrc.new <- rowSums(p.nrc.df)
p.nrc.new <- as.data.frame(p.nrc.new)
p.nrc.def <- cbind(rownames(p.nrc.new), as.numeric(p.nrc.new[,1]))
p.nrc.def <- as.data.frame(p.nrc.def)
colnames(p.nrc.def) <- c("sentiment", "tot")

which.max(p.nrc.def$tot[1:8])

p.nrc.graf <- p.nrc.def[1:8,] 
quickplot(sentiment, data=p.nrc.graf, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Survey sentiments")


barplot(
  sort(colSums(prop.table(p.nrc[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",
  xlim=c(0, 0.25), col = palette("Pastel 1")[1:8]
)

#  anger     anticipation   disgust         fear      joy         sadness     surprise        trust 
#0.10709334   0.15722183   0.05501285   0.15092205   0.10368821   0.12255274   0.09800913   0.20549983 


#grafico temporale
p.sent <- as.numeric((p.nrc[,10])-(p.nrc[,9]))
p.text <- c()
for(i in 1:n.p) {p.text <- c(p.text, pfizer.corpus.pulito[[i]]$content)}

pfizer.tibble <- as_tibble(cbind(date=pfizer$date, p.sent))# p.text,
head(pfizer.tibble)
pfizer.tibble[,1] <- as.Date(pfizer.tibble[,1]$date)
pfizer.tibble[,3] <- p.sent

p.sintesi <- pfizer.tibble [,-2]%>%
  group_by(date) %>%
  summarise(sentiment=sum(p.sent))
p.sintesi <- p.sintesi[-1,]
barplot(p.sintesi$sentiment)

p.time.pl <- ggplot(data = p.sintesi, mapping = aes(date,sentiment))+
  geom_col(fill="skyblue2", color="black")+ 
  geom_smooth(col= "red2", se=F) +
  scale_x_date(date_breaks = "2 weeks")+
  theme(axis.text.x = element_blank(), # text(angle=45,hjust = 1)) #+
        axis.title.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = 'gray95'))#, colour = 'red'))    
  #labs(title="#pfizer")

p.time.pl

theme(panel.background = element_rect(fill = 'green', colour = 'red'))


#####  CLUSTERING #####
p.tdm.clus.orig <- t(as.matrix(p.tdm.def))
#p.tdm.clus <- p.tdm.clus.orig

p.elim <- sample(1:n, 350000)
p.tdm.clus <- p.tdm.clus.orig#[-p.elim,]
dim(p.tdm.clus)


library(proxy)
library(cluster)
library(lsa)
p.dis.c <- dist((p.tdm.clus), method = "cosine")
p.dis.c <- cosine(p.tdm.clus)

#p.dis.e <- dist(tdm.clus)

rm(list="p.wh.vec")
dim(p.dis.c)

p.test =c(1:10)
p.wh.vec <- c()
for(i in 1:length(p.test)){km <- kmeans(p.tdm.clus, centers = p.test[i], nstart=50, iter.max = 25)
                        wh <- km$tot.withinss
                        p.wh.vec <- c(p.wh.vec, wh) }

p.whplot <- plot(1:length(p.test), p.wh.vec,
             type="b", pch = 19, col="orangered3", cex=1.2 , frame = FALSE, 
             xlab="Number of clusters K",
              ylab="Total within-clusters sum of squares", main="Elbow-method")

p.km3 <- kmeans(p.tdm.clus, centers = 3, nstart=50, iter.max = 50)
p.cluplot3 <- clusplot(p.tdm.clus, p.km3$cluster, color=T, shade=T, labels=F, lines=0, 
                       col.clus =c("deepskyblue2", "orange2", "chartreuse3"),#"orangered2","hotpink1"),
         col.p = "black", cex =0.5, main = "Clusplot Term-Document Matrix")

library(factoextra)
fviz_cluster(p.km3, data = p.tdm.clus,
             palette = c("#2E9FDF", "#E7B800","chartreuse2"), # "#E7B800",  "#FED9A6" ), #00AFBB
             geom = "point", pointsize = 0.8,
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

p.clusplot
p.clusplot2

colors()

p.clus1 <- pfizer.original$content[which(p.km3$cluster==1)]
p.clus2 <- pfizer.original$content[which(p.km3$cluster==2)]
p.clus3 <- pfizer.original$content[which(p.km3$cluster==3)]
p.clus3 <- pfizer.original$content[which(p.km4$cluster==4)]


p.nrc.cl1 <- get_nrc_sentiment(p.clus1, language="english") 
p.nrc.df.cl1 <- data.frame(t(p.nrc.cl1))
p.nrc.new1 <- rowSums(p.nrc.df.cl1)
p.nrc.new1 <- as.data.frame(p.nrc.new1)
p.nrc.def1 <- cbind(rownames(p.nrc.new1), as.numeric(p.nrc.new1[,1]))
p.nrc.def1 <- as.data.frame(p.nrc.def1)
colnames(p.nrc.def1) <- c("sentiment", "tot")
p.nrc.graf1 <- p.nrc.def1[1:8,] 
quickplot(sentiment, data=p.nrc.graf1, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 1")
colSums(prop.table(p.nrc.cl1[, 1:8]))
#  anger    anticipation      disgust         fear          joy      sadness     surprise        trust 
# 0.12372573   0.14813799   0.05831138   0.15517998   0.09745391   0.12770705   0.10442884   0.18505512
# n = 15833
length(p.clus3)

p.nrc.cl2 <- get_nrc_sentiment(p.clus2, language="english") 
p.nrc.df.cl2 <- data.frame(t(p.nrc.cl2))
p.nrc.new2 <- rowSums(p.nrc.df.cl2)
p.nrc.new2 <- as.data.frame(p.nrc.new2)
p.nrc.def2 <- cbind(rownames(p.nrc.new2), as.numeric(p.nrc.new2[,1]))
p.nrc.def2 <- as.data.frame(p.nrc.def2)
colnames(p.nrc.def2) <- c("sentiment", "tot")
p.nrc.graf2 <- p.nrc.def2[1:8,] 
quickplot(sentiment, data=p.nrc.graf2, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 2")
colSums(prop.table(p.nrc.cl2[, 1:8]))
#   anger    anticipation      disgust         fear          joy      sadness     surprise        trust 
 #0.09319151   0.16038206   0.05150347   0.15184213   0.10699468   0.11875266   0.09185005   0.22548344 
# n=44033


p.nrc.cl3 <- get_nrc_sentiment(p.clus3, language="english") 
p.nrc.df.cl3 <- data.frame(t(p.nrc.cl3))
p.nrc.new3 <- rowSums(p.nrc.df.cl3)
p.nrc.new3 <- as.data.frame(p.nrc.new3)
p.nrc.def3 <- cbind(rownames(p.nrc.new3), as.numeric(p.nrc.new3[,1]))
p.nrc.def3 <- as.data.frame(p.nrc.def3)
colnames(p.nrc.def3) <- c("sentiment", "tot")
p.nrc.graf3 <- p.nrc.def3[1:8,] 
quickplot(sentiment, data=p.nrc.graf3, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 3")
colSums(prop.table(p.nrc.cl3[, 1:8]))
#   anger     anticipation      disgust         fear          joy      sadness     surprise        trust 
#  0.08834010   0.18110059   0.05360848   0.13234100   0.11657645   0.11517817   0.09321155   0.21964366 
#n =50954
palette()

#________________________________________________________________________________________________

##### MODERNA ######
moder <- read_xlsx("Moderna_definitivo.xlsx", col_types = c("date", "text"))
head(moder)
moder.original <- moder
#covidvac <- covidvac.original

n.m <- nrow(moder.original)

##### CREAZIONE DEL CORPUS ####
moder <- as_tibble(moder)
moder <- separate(moder, date, into = c("date", "time"), sep=" ")
moder <- as.data.frame(moder[,-2])
moder <- cbind(moder, moder[,1])[,-1]
moder <- cbind(seq(1:nrow(moder)), moder)
colnames(moder) <- c("doc_id", "text", "date")

moder.corpus <- Corpus(DataframeSource(moder))
moder.corpus.original <- moder.corpus
#moder.corpus <- moder.corpus.original

writeLines(as.character(moder.corpus[15]))


#####  PULIZIA DEL CORPUS  #####
moder.corpus <- tm_map(moder.corpus, content_transformer(tolower))
moder.corpus <- tm_map(moder.corpus, content_transformer(removeURL))
moder.corpus <- tm_map(moder.corpus, content_transformer(removeTAG))
moder.corpus <- tm_map(moder.corpus, content_transformer(removeHASHTAG))

mo.stopwords <- c(stopwords("english"), "covidvaccine", "cant", "get", "got", "will", "still", "amp", "can",
                  "just", "jab", "india", "need", "via", "given", "give", "tell", "told", "every", "since",
                  "try", "yet", "also", "even", "let", "next", "use", "want", "getting", "come", "see", 
                  "say", "don't", "can't", "jab", "moderna")
moder.corpus <- tm_map(moder.corpus, content_transformer(removeWords), mo.stopwords)

moder.corpus <- tm_map(moder.corpus, toSpace, "â")
moder.corpus <- tm_map(moder.corpus, toSpace, "€")
moder.corpus <- tm_map(moder.corpus, toSpace, "™")
moder.corpus <- tm_map(moder.corpus, toSpace, "ð")
moder.corpus <- tm_map(moder.corpus, toSpace, "ÿ")
moder.corpus <- tm_map(moder.corpus, toSpace, "‘")
moder.corpus <- tm_map(moder.corpus, toSpace, "‡")
moder.corpus <- tm_map(moder.corpus, toSpace, "…")
moder.corpus <- tm_map(moder.corpus, toSpace, "¦")
moder.corpus <- tm_map(moder.corpus, toSpace, "ª")
moder.corpus <- tm_map(moder.corpus, toSpace, "¤")
moder.corpus <- tm_map(moder.corpus, toSpace, "½")
moder.corpus <- tm_map(moder.corpus, toSpace, "ï")
moder.corpus <- tm_map(moder.corpus, toSpace, "\u008f")
moder.corpus <- tm_map(moder.corpus, toSpace, "‰")
moder.corpus <- tm_map(moder.corpus, toSpace, "¼")
moder.corpus <- tm_map(moder.corpus, toSpace, "’")
moder.corpus <- tm_map(moder.corpus, toSpace, "˜")
moder.corpus <- tm_map(moder.corpus, toSpace, "‚")
moder.corpus <- tm_map(moder.corpus, toSpace, "“")
moder.corpus <- tm_map(moder.corpus, content_transformer(removeNumbers))
moder.corpus <- tm_map(moder.corpus, content_transformer(removePunctuation))


moder.corpus <- tm_map(moder.corpus, content_transformer(stripWhitespace))
moder.corpus.pulito <- moder.corpus
#moder.corpus <- moder.corpus.pulito

moder.corpus <- tm_map(moder.corpus, stemDocument, language="english")


#####  ANALISI INIZIALI #####

m.tdm <- TermDocumentMatrix(moder.corpus)#, control=list(wordLengths=c(3,30)) ) #, bounds=list(global = c(80, 840)))
dim(m.tdm)
m.tdm.nos <- removeSparseTerms(m.tdm, 0.996)
m.tdm.def <- as.TermDocumentMatrix(m.tdm.nos)
dim(m.tdm.def)

dimnames(m.tdm.def)$Terms 
rownames(m.tdm.def) <- tm_map(dimnames(m.tdm.def)$Terms, stemCompletion, moder.corpus.pulito)


m.tdm.v <- sort(rowSums(as.matrix(m.tdm.def)), decreasing=T)

m.df <- data.frame(term=names(m.tdm.v), occurrences=m.tdm.v)
m.pl <- ggplot(subset(m.df, occurrences>1200) , aes(term, occurrences)) + 
  geom_bar(stat="identity", fill="lightblue", col="lightsteelblue")+ 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size=15)) + ggtitle("Most frequent terms")
m.pl    

m.df[which(m.df$occurrences>7500),]

wordcloud(names(m.tdm.v), m.tdm.v, min.freq = 1000, max.words=200, random.color = T, colors = rainbow(10),
          fixed.asp = T, rot.per = 0.30, random.order=F)


##### EMOTION CLASSIFICATION #####
m.nrc <- get_nrc_sentiment(moder.original$content, language="english") 
m.nrc.df <- data.frame(t(m.nrc))
m.nrc.df[,2:5]
m.nrc.new <- rowSums(m.nrc.df)
m.nrc.new <- as.data.frame(m.nrc.new)
m.nrc.def <- cbind(rownames(m.nrc.new), as.numeric(m.nrc.new[,1]))
m.nrc.def <- as.data.frame(m.nrc.def)
colnames(m.nrc.def) <- c("sentiment", "tot")

which.max(m.nrc.def$tot[1:8])

m.nrc.graf <- m.nrc.def[1:8,] 
quickplot(sentiment, data=m.nrc.graf, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Survey sentiments")

barplot(
  sort(colSums(prop.table(m.nrc[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",
  xlim=c(0, 0.20), col = palette("Pastel 1")[1:8]
)

# anger      anticipation    disgust         fear         joy       sadness     surprise        trust 
# 0.11241838   0.15095153   0.05335282   0.15017529   0.10446822   0.13375748   0.10696438   0.18791190  


#grafico temporale
m.sent <- as.numeric((m.nrc[,10])-(m.nrc[,9]))
m.text <- c()
for(i in 1:n.m) {m.text <- c(m.text, moder.corpus.pulito[[i]]$content)}

moder.tibble <- as_tibble(cbind(date=moder$date,  m.sent))#m.text,
head(moder.tibble)
moder.tibble[,1] <- as.Date(moder.tibble[,1]$date)
moder.tibble[,3] <- m.sent

m.sintesi <- moder.tibble [,-2]%>%
  group_by(date) %>%
  summarise(sentiment=sum(m.sent))
m.sintesi <- m.sintesi[-1,]
barplot(m.sintesi$sentiment)

m.time.pl <- ggplot(data = m.sintesi, mapping = aes(date,sentiment))+
  geom_col(fill="plum1", color="black")+ 
  geom_smooth(col= "red2", se=F) +
  scale_x_date(date_breaks = "2 weeks")+
  theme(axis.text.x = element_blank(), # text(angle=45,hjust = 1)) #+
        axis.title.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = 'gray95')) #+
  #labs(title="#moderna")

m.time.pl

rm(list = c("covidvac", "mod.tibble", "p.tibble", "a.tibble", "tot.tibble"))
mod.tibble <- moder.tibble[-(which(moder.tibble$date=="2021/06/25")),]#[,-2]
mod.tibble <- cbind(mod.tibble, vaccine=rep("moderna", 59981))
colnames(mod.tibble)[2] <- "sentiment"
p.tibble <- cbind(pfizer.tibble, vaccine=rep("pfizer", 110820))  #group_by(pfizer.tibble[,-2], date)
colnames(p.tibble)[2] <- "sentiment"
a.tibble <- cbind(astra.tibble, vaccine=rep("astrazeneca", 120685))#group_by(astra.tibble[,-2], date)
colnames(a.tibble)[2] <- "sentiment"
tot.tibble <- rbind(p.tibble, mod.tibble, a.tibble)
tot.tibble <- tot.tibble %>% group_by(vaccine, date) %>% summarise(tot=sum(sentiment))

colnames(p.tibble)fill=c("darkolivegreen","plum2", "cadetblue2"),fill=vaccine

ggplot(tot.tibble, aes(date, tot, group=date)) + 
  geom_col(aes(fill=vaccine) , color="black")+
  geom_smooth(aes(date, tot))+
  facet_wrap(vaccine~., nrow=3, ncol=1, strip.position = "top", scales = "free_y")+
  scale_x_date(date_breaks = "2 weeks")+#, element_text(angle=45,hjust = 1))+
  theme(panel.background = element_rect(color = "grey96"),
        axis.text.x=element_text(angle=45, hjust=1),
        legend.position = "none")+
  ylab("sentiment polarity")

p.time.pl + m.time.pl
library(gridExtra)
grid.arrange(grobs= plot.list)

plot.list <- list(p=p.time.pl, m=m.time.pl, a=a.time.pl)
arrangeGrob(grobs=plot.list, left =  )
m.sintesi <- m.sintesi[-206,]
tot.sintesi <- cbind(as.data.frame(p.sintesi), as.data.frame(m.sintesi$sentiment),
                     as.data.frame(a.sintesi$sentiment))
colnames(tot.sintesi) <- c("date", "pfizer", "moderna", "astrazeneca")
ggplot(tot.sintesi, aes(data ))+
  geom_col()+facet_wrap(date, scales = "free_y")

#####  CLUSTERING #####
m.tdm.clus.orig <- t(as.matrix(m.tdm.def))
#p.tdm.clus <- p.tdm.clus.orig

m.elim <- sample(1:n.m, 350000)
m.tdm.clus <- m.tdm.clus.orig#[-m.elim,]
dim(m.tdm.clus)


library(proxy)
library(cluster)
library(lsa)
m.dis.c <- dist((m.tdm.clus), method = "cosine")
m.dis.c <- cosine(m.tdm.clus)

#m.dis.e <- dist(m.tdm.clus)

rm(list="m.wh.vec")
dim(m.dis.c)

m.test =c(1:10)
m.wh.vec <- c()
for(i in 1:length(m.test)){km <- kmeans(m.tdm.clus, centers = m.test[i], nstart=50, iter.max = 25)
                            wh <- km$tot.withinss
                          m.wh.vec <- c(m.wh.vec, wh) }

m.whplot <- plot(1:length(m.test), m.wh.vec,
                 type="b", pch = 19, col="mediumpurple"  , cex=1.2 , frame = FALSE, 
                 xlab="Number of clusters K",
                 ylab="Total within-clusters sum of squares", main="Elbow-method")

m.km2 <- kmeans(m.tdm.clus, centers = 2, nstart=50, iter.max = 50)
m.cluplot <- clusplot(m.tdm.clus, m.km$cluster, color=T, shade=T, labels=F, lines=0, col.clus =c(1:3),
                       col.p = "black", cex =0.5, main = "Clusplot Term-Document Matrix")
m.km2$cluster #4clust
m.cluplot
m.cluplot3

m.clus1 <- moder.original$content[which(m.km2$cluster==1)]
m.clus2 <- moder.original$content[which(m.km2$cluster==2)]
m.clus3 <- moder.original$content[which(m.km3$cluster==3)]

m.nrc.cl1 <- get_nrc_sentiment(m.clus1, language="english") 
m.nrc.df.cl1 <- data.frame(t(m.nrc.cl1))
m.nrc.new1 <- rowSums(m.nrc.df.cl1)
m.nrc.new1 <- as.data.frame(m.nrc.new1)
m.nrc.def1 <- cbind(rownames(m.nrc.new1), as.numeric(m.nrc.new1[,1]))
m.nrc.def1 <- as.data.frame(m.nrc.def1)
colnames(m.nrc.def1) <- c("sentiment", "tot")
m.nrc.graf1 <- m.nrc.def1[1:8,] 
quickplot(sentiment, data=m.nrc.graf1, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 1")
colSums(prop.table(m.nrc.cl1[, 1:8]))
length(m.clus1)

#con 3 clusters
# anger     anticipation      disgust         fear          joy      sadness     surprise        trust 
#0.09325294   0.16165116   0.05025510   0.14721851   0.10734456   0.12341418   0.09724988   0.21961367 
# n= 23.126

#con solo 2 clusters
#    anger    anticipation    disgust       fear          joy      sadness     surprise        trust 
# 0.11572546   0.14708050   0.05234429   0.15337787   0.10103958   0.13553432   0.10944606   0.18545193 
# n=50.038

m.nrc.cl2 <- get_nrc_sentiment(m.clus2, language="english") 
m.nrc.df.cl2 <- data.frame(t(m.nrc.cl2))
m.nrc.new2 <- rowSums(m.nrc.df.cl2)
m.nrc.new2 <- as.data.frame(m.nrc.new2)
m.nrc.def2 <- cbind(rownames(m.nrc.new2), as.numeric(m.nrc.new2[,1]))
m.nrc.def2 <- as.data.frame(m.nrc.def2)
colnames(m.nrc.def2) <- c("sentiment", "tot")
m.nrc.graf2 <- m.nrc.def2[1:8,] 
quickplot(sentiment, data=m.nrc.graf2, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 2")
colSums(prop.table(m.nrc.cl2[, 1:8]))
length(m.clus2)

#con 3 clusters
#  anger       anticipation      disgust      fear       joy          sadness     surprise        trust 
#  0.13182825   0.13705895   0.05362769   0.15725383   0.09679399   0.14395350   0.11849672   0.16098707 
# n=27.677

#con 2 clusters
#    anger     anticipation   disgust     fear          joy          sadness     surprise        trust 
#  0.09414725   0.17233845   0.05892479   0.13248146   0.12341102   0.12394068   0.09325344   0.20150291 
# n= 10.030


m.nrc.cl3 <- get_nrc_sentiment(m.clus3, language="english") 
m.nrc.df.cl3 <- data.frame(t(m.nrc.cl3))
m.nrc.new3 <- rowSums(m.nrc.df.cl3)
m.nrc.new3 <- as.data.frame(m.nrc.new3)
m.nrc.def3 <- cbind(rownames(m.nrc.new3), as.numeric(m.nrc.new3[,1]))
m.nrc.def3 <- as.data.frame(m.nrc.def3)
colnames(m.nrc.def3) <- c("sentiment", "tot")
m.nrc.graf3 <- m.nrc.def3[1:8,] 
quickplot(sentiment, data=m.nrc.graf3, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 3")
colSums(prop.table(m.nrc.cl3[, 1:8]))
length(m.clus3)

#con 3 clusters
# anger       anticipation      disgust      fear          joy      sadness     surprise        trust 
# 0.09571542   0.17091264   0.06061374   0.13338641   0.12354346   0.12571470   0.09260331   0.19751031 
# n=9265


library(factoextra)
fviz_cluster(m.km2, data = scale(m.tdm.clus),
             palette = c("#2E9FDF", "red"),# "#E7B800",  "#FED9A6" ), 
             geom = "point", pointsize = 0.8,
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


palette()

#_______________________________________________________________________________________________

##### ASTRAZENECA ######
astra <- read_xlsx("astrazeneca_definitivo.xlsx", col_types = c("date", "text", "text"))
astra <- astra[,-3]
astra.original <- astra
#covidvac <- covidvac.original

n.a <-nrow(astra.original)

##### CREAZIONE DEL CORPUS ####
astra <- as_tibble(astra)
astra <- separate(astra, date, into = c("date", "time"), sep=" ")
astra <- as.data.frame(astra[,-2])
astra <- cbind(astra, astra[,1])[,-1]
astra <- cbind(seq(1:nrow(astra)), astra)
colnames(astra) <- c("doc_id", "text", "date")

astra.corpus <- Corpus(DataframeSource(astra))
astra.corpus.original <- astra.corpus
#moder.corpus <- moder.corpus.original

writeLines(as.character(astra.corpus[15]))


#####  PULIZIA DEL CORPUS  #####
astra.corpus <- tm_map(astra.corpus, content_transformer(tolower))
astra.corpus <- tm_map(astra.corpus, content_transformer(removeURL))
astra.corpus <- tm_map(astra.corpus, content_transformer(removeTAG))
astra.corpus <- tm_map(astra.corpus, content_transformer(removeHASHTAG))

as.stopwords <- c(stopwords("english"), "covidvaccine", "cant", "get", "got", "will", "still", "amp", "can",
                  "just", "jab", "india", "need", "via", "given", "give", "tell", "told", "every", "since",
                  "try", "yet", "also", "even", "let", "next", "use", "want", "getting", "come", "see", 
                  "say", "don't", "can't", "jab", "astrazeneca")
astra.corpus <- tm_map(astra.corpus, content_transformer(removeWords), as.stopwords)

astra.corpus <- tm_map(astra.corpus, toSpace, "â")
astra.corpus <- tm_map(astra.corpus, toSpace, "€")
astra.corpus <- tm_map(astra.corpus, toSpace, "™")
astra.corpus <- tm_map(astra.corpus, toSpace, "ð")
astra.corpus <- tm_map(astra.corpus, toSpace, "ÿ")
astra.corpus <- tm_map(astra.corpus, toSpace, "‘")
astra.corpus <- tm_map(astra.corpus, toSpace, "‡")
astra.corpus <- tm_map(astra.corpus, toSpace, "…")
astra.corpus <- tm_map(astra.corpus, toSpace, "¦")
astra.corpus <- tm_map(astra.corpus, toSpace, "ª")
astra.corpus <- tm_map(astra.corpus, toSpace, "¤")
astra.corpus <- tm_map(astra.corpus, toSpace, "½")
astra.corpus <- tm_map(astra.corpus, toSpace, "ï")
astra.corpus <- tm_map(astra.corpus, toSpace, "\u008f")
astra.corpus <- tm_map(astra.corpus, toSpace, "‰")
astra.corpus <- tm_map(astra.corpus, toSpace, "¼")
astra.corpus <- tm_map(astra.corpus, toSpace, "’")
astra.corpus <- tm_map(astra.corpus, toSpace, "˜")
astra.corpus <- tm_map(astra.corpus, toSpace, "‚")
astra.corpus <- tm_map(astra.corpus, toSpace, "“")
astra.corpus <- tm_map(astra.corpus, content_transformer(removeNumbers))
astra.corpus <- tm_map(astra.corpus, content_transformer(removePunctuation))


astra.corpus <- tm_map(astra.corpus, content_transformer(stripWhitespace))
astra.corpus.pulito <- astra.corpus
#moder.corpus <- moder.corpus.pulito

astra.corpus <- tm_map(astra.corpus, stemDocument, language="english")


#####  ANALISI INIZIALI #####

a.tdm <- TermDocumentMatrix(astra.corpus)#, control=list(wordLengths=c(3,30)) ) #, bounds=list(global = c(80, 840)))
dim(a.tdm)
a.tdm.nos <- removeSparseTerms(a.tdm, 0.996)
a.tdm.def <- as.TermDocumentMatrix(a.tdm.nos)
dim(a.tdm.def) #526 term

dimnames(a.tdm.def)$Terms 
rownames(a.tdm.def) <- tm_map(dimnames(a.tdm.def)$Terms, stemCompletion, moder.corpus.pulito)


a.tdm.v <- sort(rowSums(as.matrix(a.tdm.def)), decreasing=T)

a.df <- data.frame(term=names(a.tdm.v), occurrences=a.tdm.v)
a.pl <- ggplot(subset(a.df, occurrences>2600) , aes(term, occurrences)) + 
  geom_bar(stat="identity", fill="lightblue", col="lightsteelblue")+ 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size=15)) + ggtitle("Most frequent terms")
a.pl    

a.df[which(a.df$occurrences>7500),]

wordcloud(names(a.tdm.v), a.tdm.v, min.freq = 2000, max.words=200, random.color = T, colors = rainbow(10),
          fixed.asp = T, rot.per = 0.30, random.order=F)


##### EMOTION CLASSIFICATION #####
a.nrc <- get_nrc_sentiment(astra.original$content, language="english") 
a.nrc.df <- data.frame(t(a.nrc))
a.nrc.df[,2:5]
a.nrc.new <- rowSums(a.nrc.df)
a.nrc.new <- as.data.frame(a.nrc.new)
a.nrc.def <- cbind(rownames(a.nrc.new), as.numeric(a.nrc.new[,1]))
a.nrc.def <- as.data.frame(a.nrc.def)
colnames(a.nrc.def) <- c("sentiment", "tot")

which.max(a.nrc.def$tot[1:8])

a.nrc.graf <- a.nrc.def[1:8,] 
quickplot(sentiment, data=a.nrc.graf, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Survey sentiments")

barplot(
  sort(colSums(prop.table(a.nrc[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab="Percentage",
  xlim=c(0, 0.25), col = palette("Pastel 1")[1:8]
)

colSums(prop.table(a.nrc[, 1:8]))
#   anger      anticipation   disgust         fear          joy      sadness     surprise        trust 
#  0.11681563   0.16326230   0.06094338   0.15659164   0.09961306   0.11785928   0.07888168   0.20603303 


#grafico temporale
a.sent <- as.numeric((a.nrc[,10])-(a.nrc[,9]))
a.text <- c()
for(i in 1:n.a) {a.text <- c(a.text, astra.corpus.pulito[[i]]$content)}

astra.tibble <- as_tibble(cbind(date=astra$date,  a.sent))#a.text,
head(astra.tibble)
astra.tibble[,1] <- as.Date(astra.tibble[,1]$date)
astra.tibble[,3] <- a.sent

a.sintesi <- astra.tibble[,-2] %>%
  group_by(date) %>%
  summarise(sentiment=sum(a.sent))
a.sintesi <- a.sintesi[-1,]
barplot(a.sintesi$sentiment)

a.time.pl <- ggplot(data = a.sintesi, mapping = aes(date, sentiment))+
  geom_col(fill="palegreen3", color="black")+  
  geom_smooth(col= "red2", se=F) +
  scale_x_date(date_breaks = "2 weeks")+
  theme(axis.text.x = element_blank(), # text(angle=45,hjust = 1)) #+
        axis.title.x = element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = 'gray95'))
  #labs(title="#astrazeneca")

a.time.pl

a.sintesi[which.max(a.sintesi$sentiment),]
a.sintesi[which(a.sintesi$date=="2021-04-07"),]

# 16-03-21 picco max: stop al vaccino astrazeneca
# 07/04/21 picco a dx: EMA trova possibile correlazione tra vaccini astazeneca e casi di trombosi
# 30/12/20 primo picco da sx: Gran Bretagna approva astrazeneca
# 29/01/21: secondo picco da sx: EMA approva astrazeneca
# 08/02/21: terzo picco da sx: nuove disposizioni per la somministrazione, solo fino a 65 anni.

p.sintesi[which(p.sintesi$date=="2020-12-02"),]

#####  CLUSTERING #####
a.tdm.clus.orig <- t(as.matrix(a.tdm.def))
#p.tdm.clus <- p.tdm.clus.orig

a.elim <- sample(1:n.a, 350000)
a.tdm.clus <- a.tdm.clus.orig#[-a.elim,]
dim(a.tdm.clus)


library(proxy)
library(cluster)
library(lsa)
a.dis.c <- dist((a.tdm.clus), method = "cosine")
a.dis.c <- cosine(a.tdm.clus)

#a.dis.e <- dist(a.tdm.clus)

rm(list="a.wh.vec")
dim(a.dis.c)

a.test =c(1:10)
a.wh.vec <- c()
for(i in 1:length(a.test)){km <- kmeans(a.tdm.clus, centers = a.test[i], nstart=50, iter.max = 25)
                          wh <- km$tot.withinss
                          a.wh.vec <- c(a.wh.vec, wh) }

a.whplot <- plot(1:length(a.test), a.wh.vec,
                 type="b", pch = 19, col="mediumpurple"  , cex=1.2 , frame = FALSE, 
                 xlab="Number of clusters K",
                 ylab="Total within-clusters sum of squares", main="Elbow-method #astrazeneca")

a.km2 <- kmeans(a.tdm.clus, centers = 2, nstart=50, iter.max = 50)
a.cluplot3 <- clusplot(a.tdm.clus, a.km3$cluster, color=T, shade=T, labels=F, lines=0, col.clus =c(1:3),
                                 col.p = "black", cex =0.5, main = "Clusplot Term-Document Matrix")
a.km2$cluster #4clust
a.cluplot    #2 clust
a.cluplot3   #3 clust

a.clus1 <- astra.original$content[which(a.km2$cluster==1)]
a.clus2 <- astra.original$content[which(a.km2$cluster==2)]
a.clus3 <- astra.original$content[which(a.km2$cluster==3)]

a.nrc.cl1 <- get_nrc_sentiment(a.clus1, language="english") 
a.nrc.df.cl1 <- data.frame(t(a.nrc.cl1))
a.nrc.new1 <- rowSums(a.nrc.df.cl1)
a.nrc.new1 <- as.data.frame(a.nrc.new1)
a.nrc.def1 <- cbind(rownames(a.nrc.new1), as.numeric(a.nrc.new1[,1]))
a.nrc.def1 <- as.data.frame(a.nrc.def1)
colnames(a.nrc.def1) <- c("sentiment", "tot")
a.nrc.graf1 <- a.nrc.def1[1:8,] 
quickplot(sentiment, data=a.nrc.graf1, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 1")
colSums(prop.table(a.nrc.cl1[, 1:8]))
length(a.clus1)

#con 3 clusters

#con solo 2 clusters
#   anger    anticipation      disgust         fear      joy      sadness       surprise        trust 
# 0.12840385   0.15935226   0.06288922   0.15330855   0.09923023   0.12017055   0.08338322   0.19326213 
#  n= 64.361

a.nrc.cl2 <- get_nrc_sentiment(a.clus2, language="english") 
a.nrc.df.cl2 <- data.frame(t(a.nrc.cl2))
a.nrc.new2 <- rowSums(a.nrc.df.cl2)
a.nrc.new2 <- as.data.frame(a.nrc.new2)
a.nrc.def2 <- cbind(rownames(a.nrc.new2), as.numeric(a.nrc.new2[,1]))
a.nrc.def2 <- as.data.frame(a.nrc.def2)
colnames(a.nrc.def2) <- c("sentiment", "tot")
a.nrc.graf2 <- a.nrc.def2[1:8,] 
quickplot(sentiment, data=a.nrc.graf2, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 2")
colSums(prop.table(a.nrc.cl2[, 1:8]))
length(a.clus2)

#con 3 clusters

#con 2 clusters
# anger        anticipation     disgust     fear          joy        sadness     surprise        trust 
# 0.10431418   0.16748049     0.05884419   0.16013346   0.10002606   0.11536588   0.07402540   0.21981035 
# n=56.324


a.nrc.cl3 <- get_nrc_sentiment(a.clus3, language="english") 
a.nrc.df.cl3 <- data.frame(t(a.nrc.cl3))
a.nrc.new3 <- rowSums(a.nrc.df.cl3)
a.nrc.new3 <- as.data.frame(a.nrc.new3)
a.nrc.def3 <- cbind(rownames(a.nrc.new3), as.numeric(a.nrc.new3[,1]))
a.nrc.def3 <- as.data.frame(a.nrc.def3)
colnames(a.nrc.def3) <- c("sentiment", "tot")
a.nrc.graf3 <- a.nrc.def3[1:8,] 
quickplot(sentiment, data=a.nrc.graf3, weight=as.numeric(tot), geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Cluster 3")
colSums(prop.table(a.nrc.cl3[, 1:8]))
length(a.clus3)

#con 3 clusters

rm(list = c("covidvac", "m.clus1", "moder.corpus", "moder", "m.tdm", "m.clus2", "mo.stopwords",
            "m.km2", "km", "m.tdm.clus", "astra"))

library(factoextra)
fviz_cluster(a.km2, data = scale(a.tdm.clus),
             palette = c("darkorange1", "dodgerblue3"),# "#E7B800",  "#FED9A6" ), 
             geom = "point", pointsize = 0.8,
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


palette()



