library(tm)
library(wordcloud)
library(cluster)
library(ggplot2)
library(SnowballC)
library(fpc)
library(car)

#___________________________________________________________________________________________________
#                                    CARICAMENTO DEL CORPUS
#___________________________________________________________________________________________________

corpus <- Corpus(DirSource("DonaldTrump", encoding = "UTF-8"), readerControl = list(language = "en"))
summary(corpus.original)
corpus.original

writeLines(as.character(corpus[[5]]))

corpus.original <- corpus
corpus <- corpus.original
#__________________________________________________________________________________________________
#                                       PRE - PROCESSING
#__________________________________________________________________________________________________
writeLines(as.character(corpus[[5]]))

#trasformo simboli in spazi per poterli rimuovere
toSpace <- function(x, symbol) {gsub(symbol, " ", x)}
corpus <- tm_map(corpus, toSpace, "$")
corpus <- tm_map(corpus, toSpace, "-")
corpus <- tm_map(corpus, toSpace, "\\.")
corpus <- tm_map(corpus, toSpace, "'re")
corpus <- tm_map(corpus, toSpace, "'ve")
corpus <- tm_map(corpus, toSpace, "'ll")
corpus <- tm_map(corpus, toSpace, "'s")
#corpus <- tm_map(corpus, toSpace, ")")
#corpus <- tm_map(corpus, toSpace, "(")
corpus <- tm_map(corpus, toSpace, "-")

# trasformo maiuscole in minuscole
corpus <- tm_map(corpus, tolower)

# rimozione punteggiatura
corpus <- tm_map(corpus, removePunctuation)

# rimozione dei numeri
corpus <- tm_map(corpus, removeNumbers)

# rimozione delle stop words
mystopwords <- c("one", "can", "also", "get", "say", "dont", "use", "put", "way", "run", "even", "applause",
                 "ive", "cant", "come", "ill", "applauseour", "applauseaudi", "didnt", "doesnt", "not",
                 "yeah", "know", "ran", "zero", "two", "hey", "may", "might", "yes", "no", "wouldnt", 
                 "say", "first", "day", "live", "many", "keep", "since", "mean", "made", "do", "done", 
                 "just", "only", "make", "made", "every", "look", "time", "ever", "got", "big", 
                 "doesn", "long", "give", "well", "tell", "think", "take", "another", "don", "like", 
                 "said", "ask", "run", "really", "let", "word", "easy", "number", "inner", "see", "saw",
                 "running", "part", "somebody", "anybody", "anyone", "someone", "u", "s", "thing",
                 "happening", "happens", "bring", "need", "call", "much", "never", "happen", "maybe", 
                 "will", "must", "going", "good", "people", "nice", "america", "country", "countries",
                 "american", "tom", "state", "states", "great", "want", "trump", "nation", "nations", 
                 "start", "stop", "new", "thing", "back")
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), mystopwords) )

# rimozione spazi bianchi
corpus <- tm_map(corpus, stripWhitespace)

corpus.precomplet <- corpus
corpus.stem<- tm_map(corpus, stemDocument)

#_____________________________________________________________________________________________________
#                                             ASSOCIAZIONI
#_____________________________________________________________________________________________________

#matrice term-document senza stemCompletion altrimenti non funzionano delle funzioni
tdm3 <- TermDocumentMatrix(corpus.stem, control = list(wordLengths=c(3,27)))
freq3 <- rowSums(as.matrix(tdm3))

# individuazione delle associazioni
findAssocs(tdm3, "obama", corlimit=0.60)
# Il nome di obama è associato per la maggior parte a termini negativi: enemy, worse, aggress, weak
findAssocs(tdm3, "china", corlimit = 0.7 )
#la cina è associata a parole come: dog, loser, trump,  hell, sad, buy
findAssocs(tdm3, "tax", corlimit = 0.6 )
# property, overregulation, shut, growth
findAssocs(tdm3, "clinton", corlimit = 0.6)
#evil, communism, mistake, ideology, terror, enemy, libya
findAssocs(tdm3, "hillari", corlimit = 0.5)
#clinton delete rig server
findAssocs(tdm3, "job", corlimit=0.5)
#trade, regul, million, manufactur, model, currenc, repeal, tax
findAssocs(tdm3, "immigr", corlimit=0.6)
#terrorist, killer, radic, bomber, asylum, taliban
findAssocs(tdm3, "school", corlimit=0.6)
#choice, educ, charterm, host, quibbl, stood, disadvantag
findAssocs(tdm3, "border", corlimit=0.5)
#illeg, open, deport, sanctuari, cheat, crimin, gang
findAssocs(tdm3, "polici", corlimit=0.6)
#ally, vision, war, aimless, atrophy, awake, blaze, caution, chaotic, cheapest

#__________________________________________________________________________________________________
#                                     RAPPRESENTAZIONI GRAFICHE
#__________________________________________________________________________________________________

#grafico a barre
df <- data.frame(term=names(freq3), occurrences=freq3)   #senza stemCompletion
pl <- ggplot(subset(df, freq3> 150), aes(term, occurrences)) + 
             geom_bar(stat="identity", fill="lightsteelblue", col="navyblue")+ 
             theme(axis.text.x=element_text(angle = 45, hjust = 1, size=15 ))
pl


#wordcloud
set.seed(122)      #senza stemCompletion
wordcloud(names(freq3), freq3, min.freq = 110, random.color = T, colors = rainbow(10), fixed.asp = T)


#_________________________________________________________________________________________________________
#                                       STEM COMPLETION (non utilizzata)
#_________________________________________________________________________________________________________

#funzione unica
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
corpus <- removeNumPunct(corpus.precomplet)
#l'unico modo per far funzionare la funzione stemCompletion è usare questa funzione che restituisce
#il corpus in una forma accettata dalla funzione stemCompletion
corpus <- PlainTextDocument(corpus)

# stemming
mydic <- corpus
corpus <- mydic
corpus <- stemDocument(corpus)

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

corpus <- stemCompletion2(corpus, mydic)

corpus.complet <- corpus
inspect(corpus.complet)
#corpus <- corpus.complet

#stemming manuale
writeLines(as.character(corpus))[[1]]
corpus <- gsub(pattern="worst", replace="worse", corpus)
corpus <- gsub(pattern="obamacar", replace="obamacare", corpus)
corpus <- gsub(pattern="years", replace="year", corpus)
corpus <- gsub(pattern="isi", replace="isis", corpus)
corpus <- gsub(pattern="dollars", replace="money", corpus)
corpus <- gsub(pattern="jobs", replace="job", corpus)
corpus <- gsub(pattern="working", replace="work", corpus)
corpus <- gsub(pattern="workers", replace="work", corpus)
corpus <- gsub(pattern="schools", replace="school", corpus)
corpus <- gsub(pattern="policy", replace="policies", corpus)
corpus <- gsub(pattern="millions", replace="million", corpus)
corpus <- gsub(pattern="deals", replace="deal", corpus)


corpus.postcleaninig <- corpus
corpus <- corpus.postcleaninig

#__________________________________________________________________________________________________
#                             ANALISI QUANTITATIVE, TERM-DOCUMENT MATRIX
#__________________________________________________________________________________________________
corpus2 <- Corpus(VectorSource(corpus.stem))  #serve questo comando per trasformare di nuovo il corpus 
                                              #che è un plainTextDocument in un oggetto corpus

corpus <- corpus.stem  #la funzione stemCompletion2 crea problemi, i termini si moltiplicano e alcuni
                       #arrivano ad avere frequenza di 40.000 

tdm <- TermDocumentMatrix(corpus)
freq <- rowSums(as.matrix(tdm))
ord <- order(freq, decreasing = T)
freq[ord[1:100]]
length(freq)

tdm2 <- TermDocumentMatrix(corpus, control=list(wordLengths=c(3,27)))
freq2 <- rowSums(as.matrix(tdm2))
ord2 <- order(freq2, decreasing = T)
freq2[ord2[1:50]]
length(freq2)

findFreqTerms(tdm2, lowfreq = 100)


#__________________________________________________________________________________________________
#                                             CLUSTERING
#__________________________________________________________________________________________________
str(tdm2)
#terms <- tdm2$dimnames["Terms"]
#out <- c("job", "clinton", "hill")
#for(i in 1:3) {terms <- terms[which(terms!=out[i])]}
#tdm2$dimnames["Terms"] <- terms
remove(mat)
remove(d)
remove(gruppi)
mat <- as.matrix(tdm2)
mat <- subset(mat, subset=freq2>110)
#head(mat)

#clustering gerarchico
remove()
d <- dist(scale(mat))
gruppi <- hclust(d, method="ward.D")
plot(gruppi, main="Terms clustering with Ward linkage", xlab = "Terms", hang = -1 )
rect.hclust(gruppi, k = 9)

pamResult1 <- pamk(tdm2, krange=4:10, metric = "manhattan" )
k1 <- pamResult1$nc
k1

#wordclouds
idx <- cutree(gruppi, 9) 
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3))    
for(k in 1:9) {cl <- which( idx ==k ) 
               m <- as.matrix(cl)
               wordcloud(rownames(m), m, random.color=TRUE, colors=rainbow(9))
               } 
layout(matrix(1)) 

#clustering partizionale
kmean <- kmeans(t(mat2), 9)
k=9
for (i in 1:k) {cat(paste("cluster ", i, ": ", sep = "")) 
  s <- sort(kmean$centers[i, ], decreasing = T) 
  cat(names(s)[1:10], "\n")}

#__________________________________________________________________________________________________
#                               DOCUMENT-TERM MATRIX E CLUSTERING
#__________________________________________________________________________________________________

corpus2 <- corpus.postcleaninig   #non funziona perchè non è stata utilizzata la funzione stemCompletion
corpus2 <- corpus.stem

remove(dtm)

dtm <- DocumentTermMatrix(corpus2)
corpus.nopunct <- tm_map(corpus.original, removePunctuation)
corpus.nopunct <- tm_map(corpus.nopunct, stripWhitespace)

# funzione per sostituire nome stati

states <- c("New","West", "North", "South", "Alabama", "Alaska", "Arizona", "Arkansas", "California", 
            "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
            "Hampshire", "Jersey", "Mexico", "York", "Carolina", "Dakota", "Ohio", "Oklahoma", "Oregon",
            "Pennsylvania", "Rohde", "Island", "Tennessee", "Texas", "Utah", "Vermont",
            "Virginia", "Washington", "Wisconsin", "Wyoming")
docnames <- rownames(dtm)
docs <- c()
state <- c()
for (i in 1:56) { name <- docnames[i]
                 speech <- corpus.nopunct[[name]]
                 riga1 <- unlist(strsplit(as.character(speech), " "))[3:23]
                 state <- c()
                 for (j in states) { add <- ifelse(riga1==j, j, "")
                                    state <- c(state, add ) 
                        }
                 state <- state[state!=""]
                 state <- unique(state)
                 st <- paste(state, sep = "", collapse = " ")
                 docs <- c(docs, st[st!=""])
             }
docs
docs[3]<-"Wisconsin"
docs[44] <- "Ohio"
docs[40]<- "Florida"
docs[42] <- "New Hampshire"
rownames(dtm) <- docs


#clustering gerarchico
dtm2 <- DocumentTermMatrix(corpus2, control=list(wordLengths=c(3,27), bounds=list(global=c(5,52))))
rownames(dtm2) <- docs
freq.dtm <- colSums(as.matrix(dtm))
mat2 <- subset(as.matrix(dtm), select=freq.dtm>100)

remove(mat2)
d2 <- dist(scale(mat2))
clus <- hclust(d2, method = "ward.D")
plot(clus, main="Speech clustering with Ward linkage", xlab = "State", hang = -1)
rect.hclust(clus, 10)

pamResult2 <- pamk(dtm2, krange=2:10, metric = "manhattan" )
k2 <- pamResult2$nc
k2

#contenuto dei gruppi con clustering partizionale
kmeansResult <- kmeans(mat2, k2)
cluster <- list()
for (i in 1:k2) {cat(paste("cluster ", i, ": ", sep = "")) 
                s <- sort(kmeansResult$centers[i, ], decreasing = T)
                cat(names(s)[1:10], "\n")
}
