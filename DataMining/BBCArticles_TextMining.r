library(tm)
library(fpc)
library(dplyr)
library(SnowballC)
library(corpus)
library(ggplot2)
library(wordcloud)
library(cluster)
#____________________________________________________________________________________________________________
#                                             CREAZIONE CORPUS
#____________________________________________________________________________________________________________

articles <- Corpus(DirSource("bbc", encoding = "UTF-8", ), readerControl = list(language = "en"))
summary(articles)
articles
#<<SimpleCorpus>>
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 928

#writeLines(as.character(articles[1]))
articles.original <- articles
#articles <- articles.original
#__________________________________________________________________________________________________________
#                                             PRE-PROCESSING
#__________________________________________________________________________________________________________

#trasformo simboli in spazi con la funzione toSpace
toSpace <- function(x, symbol) {gsub(symbol, " ", x)}
articles <- tm_map(articles, toSpace, "£")
articles <- tm_map(articles, toSpace, "-")
articles <- tm_map(articles, toSpace, "\\.")
articles <- tm_map(articles, toSpace, "'re")
articles <- tm_map(articles, toSpace, "'ve")
articles <- tm_map(articles, toSpace, "'ll")
articles <- tm_map(articles, toSpace, "'s")
articles <- tm_map(articles, toSpace, "`")



# trasformo maiuscole in minuscole
articles <- tm_map(articles, tolower)

# rimozione punteggiatura
articles <- tm_map(articles, removePunctuation)

# rimozione dei numeri
articles <- tm_map(articles, removeNumbers)

articles.prestopw <- articles
# rimozione delle stopwords
mystopwords <- c("also", "said", "say", "one", "two", "three", "get", "told", "use", "put", 
                 "think", "just", "like", "can", "want", "see", "come", "came", "will", "take",
                 "took", "week", "year", "lot", "still", "went", "didnt", "within", "sure", 
                 "despite", "wether", "without", "thought", "given", "always", "mean", "even", 
                 "dont", "new", "now", "got", "try", "meet", "good", "call", "day", "look",
                 "made", "make", "need", "old", "time", "well", "yearly", "start", "four", 
                 "five", "give", "know", "since", "month", "yet", "ever")
articles <- tm_map(articles, removeWords, c(stopwords("english"), mystopwords) )
#new.articles <- tm_map(new.articles, removeWords, c(stopwords("english"), mystopwords) )

# rimozione spazi bianchi
articles <- tm_map(articles, stripWhitespace)


articles.prestem <- articles
#articles <- articles.prestem


# stemming e stem completion
mydic <- articles.prestem
new.articles <- articles.prestem

art <- text_tokens(articles) 
for (i in 1:length(new.articles)) {art[[i]] <- stemDocument(art[[i]])
                                   art[[i]] <- art[[i]] [art[[i]] != ""]
                                   art[[i]] <- stemCompletion(art[[i]], mydic)
                                   art[[i]] <- paste(art[[i]], sep = "", collapse = " " )
                                   # art[[i]] <- PlainTextDocument(c[[i]])
                                  }

for (i in 1:length(new.articles)) {new.articles[[i]] <- art[[i]]}

articles.postcomplet <- new.articles
#new.articles <- articles.postcomplet
#writeLines(as.character(new.articles[[1]]))    # corpus con parole completate


#sport
new.articles <- tm_map(new.articles, gsub, pattern="welsh students rugby", replace="welsh_students_rugby")
new.articles <- tm_map(new.articles, gsub, pattern="welsh students", replace="welsh_students_rugby")
new.articles <- tm_map(new.articles, gsub, pattern="international association of athletics aederations", 
                       replace="IAAF")
new.articles <- tm_map(new.articles, gsub, pattern="olympic games", replace="olympics")
new.articles <- tm_map(new.articles, gsub, pattern="athens olympics", replace="olympics")
new.articles <- tm_map(new.articles, gsub, pattern="sydney games", replace="olympics")
new.articles <- tm_map(new.articles, gsub, pattern="sydney games", replace="olympics")
new.articles <- tm_map(new.articles, gsub, pattern="robinson", replace="Robinson")
new.articles <- tm_map(new.articles, gsub, pattern="jason robinson", replace="Robinson")
new.articles <- tm_map(new.articles, gsub, pattern="wilkinson", replace="Wilkinson")
new.articles <- tm_map(new.articles, gsub, pattern="jonny wilkinson", replace="Wilkinson")
new.articles <- tm_map(new.articles, gsub, pattern="tindall", replace="Tindall")
new.articles <- tm_map(new.articles, gsub, pattern="mike tindall", replace="Tindall")
new.articles <- tm_map(new.articles, gsub, pattern="greenwood", replace="Greenwood")
new.articles <- tm_map(new.articles, gsub, pattern="will greenwood", replace="Greenwood")
new.articles <- tm_map(new.articles, gsub, pattern="white", replace="White")
new.articles <- tm_map(new.articles, gsub, pattern="julian white", replace="White")
new.articles <- tm_map(new.articles, gsub, pattern="vicker", replace="Vicker")
new.articles <- tm_map(new.articles, gsub, pattern="phil vicker", replace="Vicker")
new.articles <- tm_map(new.articles, gsub, pattern="guscott", replace="Guscott")
new.articles <- tm_map(new.articles, gsub, pattern="jeremy guscott", replace="Guscott")
new.articles <- tm_map(new.articles, gsub, pattern="dawson", replace="Dawson")
new.articles <- tm_map(new.articles, gsub, pattern="matt dawson", replace="Dawson")
new.articles <- tm_map(new.articles, gsub, pattern="corry", replace="Corry")
new.articles <- tm_map(new.articles, gsub, pattern="martin corry", replace="Corry")
new.articles <- tm_map(new.articles, gsub, pattern="mourinho", replace="Mourinho")
new.articles <- tm_map(new.articles, gsub, pattern="jose mourinho", replace="Mourinho")
new.articles <- tm_map(new.articles, gsub, pattern="terry", replace="Terry")
new.articles <- tm_map(new.articles, gsub, pattern="john terry", replace="Terry")
new.articles <- tm_map(new.articles, gsub, pattern="torres", replace="Torres")
new.articles <- tm_map(new.articles, gsub, pattern="fernando torres", replace="Torres")
new.articles <- tm_map(new.articles, gsub, pattern="beckam", replace="Beckam")
new.articles <- tm_map(new.articles, gsub, pattern="david beckam", replace="Beckam")
new.articles <- tm_map(new.articles, gsub, pattern="zidane", replace="Zidane")
new.articles <- tm_map(new.articles, gsub, pattern="zinedine zidane", replace="Zidane")
new.articles <- tm_map(new.articles, gsub, pattern="gravesen", replace="Gravesen")
new.articles <- tm_map(new.articles, gsub, pattern="thomas gravesen", replace="Gravesen")
new.articles <- tm_map(new.articles, gsub, pattern="owen", replace="Owen")
new.articles <- tm_map(new.articles, gsub, pattern="michael owen", replace="Owen")
new.articles <- tm_map(new.articles, gsub, pattern="collins", replace="Collins")
new.articles <- tm_map(new.articles, gsub, pattern="kim collins", replace="Collins")
new.articles <- tm_map(new.articles, gsub, pattern="gardener", replace="Gardener")
new.articles <- tm_map(new.articles, gsub, pattern="jason gardener", replace="Gardener")
new.articles <- tm_map(new.articles, gsub, pattern="shevchenko", replace="Shevchenko")
new.articles <- tm_map(new.articles, gsub, pattern="irina shevchenko", replace="Shevchenko")
new.articles <- tm_map(new.articles, gsub, pattern="sarah Claxton", replace="Claxton")
new.articles <- tm_map(new.articles, gsub, pattern="sarah claxton", replace="Claxton")
new.articles <- tm_map(new.articles, gsub, pattern="thie", replace="Thie")
new.articles <- tm_map(new.articles, gsub, pattern="james thie", replace="Thie")
new.articles <- tm_map(new.articles, gsub, pattern="abraham", replace="Abraham")
new.articles <- tm_map(new.articles, gsub, pattern="alexis abraham", replace="Abraham")
new.articles <- tm_map(new.articles, gsub, pattern="lambert", replace="Lambert")
new.articles <- tm_map(new.articles, gsub, pattern="chris lambert", replace="Lambert")
new.articles <- tm_map(new.articles, gsub, pattern="meadows", replace="Meadows")
new.articles <- tm_map(new.articles, gsub, pattern="jenny meadows", replace="Meadows")
new.articles <- tm_map(new.articles, gsub, pattern="djhone", replace="Djhone")
new.articles <- tm_map(new.articles, gsub, pattern="leslie djhone", replace="Djhone")
new.articles <- tm_map(new.articles, gsub, pattern="buckfield", replace="Buckfield")
new.articles <- tm_map(new.articles, gsub, pattern="nick buckfield", replace="Buckfield")
new.articles <- tm_map(new.articles, gsub, pattern="murphy", replace="Murphy")
new.articles <- tm_map(new.articles, gsub, pattern="catherine murphy", replace="Murphy")
new.articles <- tm_map(new.articles, gsub, pattern="fedorova", replace="Fedorova")
new.articles <- tm_map(new.articles, gsub, pattern="olga fedorova", replace="Fedorova")
new.articles <- tm_map(new.articles, gsub, pattern="verdecchia", replace="Verdecchia")
new.articles <- tm_map(new.articles, gsub, pattern="luca verdecchia", replace="Verdecchia")
new.articles <- tm_map(new.articles, gsub, pattern="jones", replace="Jones")
new.articles <- tm_map(new.articles, gsub, pattern="robert jones", replace="Jones")
new.articles <- tm_map(new.articles, gsub, pattern="howley", replace="Howley")
new.articles <- tm_map(new.articles, gsub, pattern="rob howley", replace="Howley")
new.articles <- tm_map(new.articles, gsub, pattern="humphrey", replace="Humphreys")
new.articles <- tm_map(new.articles, gsub, pattern="jon humphrey", replace="Humphreys")
new.articles <- tm_map(new.articles, gsub, pattern="morris", replace="Morris")
new.articles <- tm_map(new.articles, gsub, pattern="williams", replace="Williams")
new.articles <- tm_map(new.articles, gsub, pattern="martyn williams", replace="Williams")
new.articles <- tm_map(new.articles, gsub, pattern="sweeney", replace="Sweeney")
new.articles <- tm_map(new.articles, gsub, pattern="ceri sweeney", replace="Sweeney")
new.articles <- tm_map(new.articles, gsub, pattern="welsh students", replace="welsh_students")
new.articles <- tm_map(new.articles, gsub, pattern="new zealand", replace="new_zealand")
new.articles <- tm_map(new.articles, gsub, pattern="rush", replace="Rush")
new.articles <- tm_map(new.articles, gsub, pattern="xavier rush", replace="Rush")
new.articles <- tm_map(new.articles, gsub, pattern="all blacks", replace="all_blacks")
new.articles <- tm_map(new.articles, gsub, pattern="lo cicero", replace="LoCicero")
new.articles <- tm_map(new.articles, gsub, pattern="andrea lo cicero", replace="LoCicero")
new.articles <- tm_map(new.articles, gsub, pattern="gear", replace="Gear")
new.articles <- tm_map(new.articles, gsub, pattern="rico gear", replace="Gear")
new.articles <- tm_map(new.articles, gsub, pattern="latham", replace="Latham")
new.articles <- tm_map(new.articles, gsub, pattern="chris latham", replace="Latham")
new.articles <- tm_map(new.articles, gsub, pattern="cowan", replace="Cowan")
new.articles <- tm_map(new.articles, gsub, pattern="jimmy cowan", replace="Cowan")
new.articles <- tm_map(new.articles, gsub, pattern="schalk burger", replace="schalk_burger")
new.articles <- tm_map(new.articles, gsub, pattern="international rugby board", replace="international_rugby_board")
new.articles <- tm_map(new.articles, gsub, pattern="dwyer", replace="Dwyer")
new.articles <- tm_map(new.articles, gsub, pattern="bob dwyer", replace="Dwyer")
new.articles <- tm_map(new.articles, gsub, pattern="greene", replace="Greene")
new.articles <- tm_map(new.articles, gsub, pattern="maurice greene", replace="Greene")
new.articles <- tm_map(new.articles, gsub, pattern="world championship", replace="world_championship")
new.articles <- tm_map(new.articles, gsub, pattern="gatlin", replace="Gatlin")
new.articles <- tm_map(new.articles, gsub, pattern="justin gatlin", replace="Gatlin")
new.articles <- tm_map(new.articles, gsub, pattern="obikwelu", replace="Obikwelu")
new.articles <- tm_map(new.articles, gsub, pattern="francis obikwelu", replace="Obikwelu")
new.articles <- tm_map(new.articles, gsub, pattern="edwards", replace="Edwards")
new.articles <- tm_map(new.articles, gsub, pattern="jonathan edwards", replace="Edwards")
new.articles <- tm_map(new.articles, gsub, pattern="manchester games", replace="manchester_games")
new.articles <- tm_map(new.articles, gsub, pattern="idowu", replace="Idowu")
new.articles <- tm_map(new.articles, gsub, pattern="phillips idowu", replace="Idowu")
new.articles <- tm_map(new.articles, gsub, pattern="chepkemei", replace="Chepkemei")
new.articles <- tm_map(new.articles, gsub, pattern="susan chepkemei", replace="Chepkemei")
new.articles <- tm_map(new.articles, gsub, pattern="big apple", replace="big_apple")
new.articles <- tm_map(new.articles, gsub, pattern="dibaba", replace="Dibaba")
new.articles <- tm_map(new.articles, gsub, pattern="tirunesh dibaba", replace="Dibaba")
new.articles <- tm_map(new.articles, gsub, pattern="kosenkow", replace="Kosenkow")
new.articles <- tm_map(new.articles, gsub, pattern="alexander kosenkow", replace="Kosenkow")
new.articles <- tm_map(new.articles, gsub, pattern="van balkom", replace="VanBalkom")
new.articles <- tm_map(new.articles, gsub, pattern="patrick van balkom", replace="VanBalkom")
new.articles <- tm_map(new.articles, gsub, pattern="blume", replace="Blume")
new.articles <- tm_map(new.articles, gsub, pattern="marc blume", replace="Blume")
new.articles <- tm_map(new.articles, gsub, pattern="gavaert", replace="Gavaert")
new.articles <- tm_map(new.articles, gsub, pattern="kim gavaert", replace="Gavaert")
new.articles <- tm_map(new.articles, gsub, pattern="great britain", replace="great_britain")
new.articles <- tm_map(new.articles, gsub, pattern="radcliffe", replace="Radcliffe")
new.articles <- tm_map(new.articles, gsub, pattern="paula radcliffe", replace="Radcliffe")
new.articles <- tm_map(new.articles, gsub, pattern="hyde peters", replace="HydePeters")
new.articles <- tm_map(new.articles, gsub, pattern="zara hyde peters", replace="HydePeters")
new.articles <- tm_map(new.articles, gsub, pattern="uk athletics", replace="uk_athletics")
new.articles <- tm_map(new.articles, gsub, pattern="moorcroft", replace="Moorcroft")
new.articles <- tm_map(new.articles, gsub, pattern="david moorcroft", replace="Moorcroft")
new.articles <- tm_map(new.articles, gsub, pattern="tel aviv", replace="tel_aviv")
new.articles <- tm_map(new.articles, gsub, pattern="kenteris", replace="Kenteris")
new.articles <- tm_map(new.articles, gsub, pattern="costas kenteris", replace="Kenteris")
new.articles <- tm_map(new.articles, gsub, pattern="thanou", replace="Thanou")
new.articles <- tm_map(new.articles, gsub, pattern="katerina thanou", replace="Thanou")
new.articles <- tm_map(new.articles, gsub, pattern="merritt", replace="Merritt")
new.articles <- tm_map(new.articles, gsub, pattern="lashawn merritt", replace="Merritt")
new.articles <- tm_map(new.articles, gsub, pattern="carroll", replace="Carroll")
new.articles <- tm_map(new.articles, gsub, pattern="mark carroll", replace="Carroll")
new.articles <- tm_map(new.articles, gsub, pattern="european indoor championships", replace="european_indoor_championships")
new.articles <- tm_map(new.articles, gsub, pattern="bekele", replace="Bekele")
new.articles <- tm_map(new.articles, gsub, pattern="kenenisa bekele", replace="Bekele")
new.articles <- tm_map(new.articles, gsub, pattern="geneti", replace="Geneti")
new.articles <- tm_map(new.articles, gsub, pattern="markos geneti", replace="Geneti")
new.articles <- tm_map(new.articles, gsub, pattern="coghlan", replace="Coghlan")
new.articles <- tm_map(new.articles, gsub, pattern="eamonn coghlan", replace="Coghlan")
new.articles <- tm_map(new.articles, gsub, pattern="holmes", replace="Holmes")
new.articles <- tm_map(new.articles, gsub, pattern="kelly holmes", replace="Holmes")
new.articles <- tm_map(new.articles, gsub, pattern="cherkasova", replace="Cherkasova")
new.articles <- tm_map(new.articles, gsub, pattern="svetlana cherkasova", replace="Cherkasova")
new.articles <- tm_map(new.articles, gsub, pattern="henson", replace="Henson")
new.articles <- tm_map(new.articles, gsub, pattern="gavin henson", replace="Henson")
new.articles <- tm_map(new.articles, gsub, pattern="hodgson", replace="Hodgson")
new.articles <- tm_map(new.articles, gsub, pattern="charlie hodgson", replace="Hodgson")
new.articles <- tm_map(new.articles, gsub, pattern="henman", replace="Henman")
new.articles <- tm_map(new.articles, gsub, pattern="tim henman", replace="Henman")
new.articles <- tm_map(new.articles, gsub, pattern="rusedski", replace="Rusedski")
new.articles <- tm_map(new.articles, gsub, pattern="greg rusedski", replace="Rusedski")
new.articles <- tm_map(new.articles, gsub, pattern="andreev", replace="Andreev")
new.articles <- tm_map(new.articles, gsub, pattern="igor andreev", replace="Andreev")

#politics
new.articles <- tm_map(new.articles, gsub, pattern="trade center", replace="trade_center")
new.articles <- tm_map(new.articles, gsub, pattern="blunkett", replace="Blunkett")
new.articles <- tm_map(new.articles, gsub, pattern="david blunkett", replace="Blunkett")
new.articles <- tm_map(new.articles, gsub, pattern="national executive", replace="national_executive")
new.articles <- tm_map(new.articles, gsub, pattern="blair", replace="Blair")
new.articles <- tm_map(new.articles, gsub, pattern="tony blair", replace="Blair")
new.articles <- tm_map(new.articles, gsub, pattern="labour party", replace="labour_party")
new.articles <- tm_map(new.articles, gsub, pattern="general election", replace="general_election")
new.articles <- tm_map(new.articles, gsub, pattern="liberal democrat", replace="liberal_democrat")
new.articles <- tm_map(new.articles, gsub, pattern="liberal democrats", replace="liberal_democrat")
new.articles <- tm_map(new.articles, gsub, pattern="lib dem", replace="liberal_democrat")
new.articles <- tm_map(new.articles, gsub, pattern="lib dems", replace="liberal_democrat")
new.articles <- tm_map(new.articles, gsub, pattern="chambers of commerce", replace="chambers_of_commerce")
new.articles <- tm_map(new.articles, gsub, pattern="frost", replace="Frost")
new.articles <- tm_map(new.articles, gsub, pattern="david frost", replace="Frost")
new.articles <- tm_map(new.articles, gsub, pattern="national party", replace="national_party")
new.articles <- tm_map(new.articles, gsub, pattern="caruana", replace="Caruana")
new.articles <- tm_map(new.articles, gsub, pattern="peter caruana", replace="Caruana")
new.articles <- tm_map(new.articles, gsub, pattern="straw", replace="Straw")
new.articles <- tm_map(new.articles, gsub, pattern="jack straw", replace="Straw")
new.articles <- tm_map(new.articles, gsub, pattern="roche", replace="Roche")
new.articles <- tm_map(new.articles, gsub, pattern="barbara roche", replace="Roche")
new.articles <- tm_map(new.articles, gsub, pattern="green", replace="Green")
new.articles <- tm_map(new.articles, gsub, pattern="andrew green", replace="Green")
new.articles <- tm_map(new.articles, gsub, pattern="world war", replace="world_war")
new.articles <- tm_map(new.articles, gsub, pattern="prime minister", replace="prime_minister")
new.articles <- tm_map(new.articles, gsub, pattern="downing street", replace="downing_street")
new.articles <- tm_map(new.articles, gsub, pattern="berlusconi", replace="Berlusconi")
new.articles <- tm_map(new.articles, gsub, pattern="silvio berlusconi", replace="Berlusconi")
new.articles <- tm_map(new.articles, gsub, pattern="schroeder", replace="Schroeder")
new.articles <- tm_map(new.articles, gsub, pattern="gerhard schroeder", replace="Schroeder")
new.articles <- tm_map(new.articles, gsub, pattern="bbc news", replace="bbc_news")
new.articles <- tm_map(new.articles, gsub, pattern="office of national statistics", replace="office_of_national_statistics")
new.articles <- tm_map(new.articles, gsub, pattern="house of lords", replace="house_of_lords")
new.articles <- tm_map(new.articles, gsub, pattern="high court", replace="high_court")
new.articles <- tm_map(new.articles, gsub, pattern="house of commons", replace="house_of_commons")
new.articles <- tm_map(new.articles, gsub, pattern="countryside alliance", replace="countryside_alliance")
new.articles <- tm_map(new.articles, gsub, pattern="howard", replace="Howard")
new.articles <- tm_map(new.articles, gsub, pattern="michael howard", replace="Howard")
new.articles <- tm_map(new.articles, gsub, pattern="kennedy", replace="Kennedy")
new.articles <- tm_map(new.articles, gsub, pattern="charles kennedy", replace="Kennedy")
new.articles <- tm_map(new.articles, gsub, pattern="brown", replace="Brown")
new.articles <- tm_map(new.articles, gsub, pattern="gordon brown", replace="Brown")
new.articles <- tm_map(new.articles, gsub, pattern="fox", replace="Fox")
new.articles <- tm_map(new.articles, gsub, pattern="liam fox", replace="Fox")
new.articles <- tm_map(new.articles, gsub, pattern="shadjareh", replace="Shadjareh")
new.articles <- tm_map(new.articles, gsub, pattern="massoud shadjareh", replace="Shadjareh")
new.articles <- tm_map(new.articles, gsub, pattern="islamic human rights commission", replace="IHRC")
new.articles <- tm_map(new.articles, gsub, pattern="blears", replace="Blears")
new.articles <- tm_map(new.articles, gsub, pattern="hazel blears", replace="Blears")
new.articles <- tm_map(new.articles, gsub, pattern="environment protection agency", replace="environment_protection_agency")
new.articles <- tm_map(new.articles, gsub, pattern="report", replace="Report")
new.articles <- tm_map(new.articles, gsub, pattern="gershon report", replace="Report")
new.articles <- tm_map(new.articles, gsub, pattern="agricultural wages committees", replace="agricultural_wages_committees")
new.articles <- tm_map(new.articles, gsub, pattern="redwood", replace="Redwood")
new.articles <- tm_map(new.articles, gsub, pattern="john redwood", replace="Redwood")
new.articles <- tm_map(new.articles, gsub, pattern="football licensing authority", replace="football_licensing_authority")
new.articles <- tm_map(new.articles, gsub, pattern="bbc radio", replace="bbc_radio")
new.articles <- tm_map(new.articles, gsub, pattern="balls", replace="Balls")
new.articles <- tm_map(new.articles, gsub, pattern="ed balls", replace="Balls")
new.articles <- tm_map(new.articles, gsub, pattern="daily telegraph", replace="daily_telegraph")
new.articles <- tm_map(new.articles, gsub, pattern="won", replace="win")

#writeLines(as.character(articles.original[["p (41).txt"]]))


#_________________________________________________________________________________________________________
#                                                ANALISI QUANTITATIVE INIZIALI
#_________________________________________________________________________________________________________
set.seed(123)
dtm <- DocumentTermMatrix(new.articles, control=list(wordLengths=c(3,30), bounds=list(global = c(80, 840))))
freq <- colSums(as.matrix(dtm))
ord <- order(freq, decreasing = T)
dim(dtm)     # 928 320
freq[ord[1:100]]     #win, govern, labour, partial, elect, people
findFreqTerms(dtm, lowfreq = 80) #"aim", "allow", "announce", "back", "blair", "british"  

freq <- freq[ord]   #"aim", "allow", "announce", "back", "blair", "british"  

# grafico a barre
df <- data.frame(term=names(freq), occurrences=freq)
pl <- ggplot(subset(df, freq>250) , aes(term, occurrences)) + 
  geom_bar(stat="identity", fill="lightblue", col="lightsteelblue")+ 
  theme(axis.text.x=element_text(angle = 45, hjust = 1, size=15)) + ggtitle("Most frequent terms")
pl    

length(subset(df, freq>250)$term) #80
# 

# wordcloud
#set.seed(122)    
wordcloud(names(freq), freq, min.freq = 300, random.color = T, colors = rainbow(10), fixed.asp = T)

#head(new.articles)
#___________________________________________________________________________________________________________
#                                               ASSOCIAZIONI
#__________________________________________________________________________________________________________

lista_ass <- list()
termini <- names(freq)
for (i in 1:length(names(freq))) {x <- termini[i]
                                   ass <- findAssocs(dtm, x, corlimit=0.5)
                                  lista_ass[[x]] <- (ass[[1]])
}
lista_ass
length(lista_ass)  #320

#vanno verificate prima delle funzioni tmmap dopo lo stemming
findAssocs(dtm, "blair", corlimit=0.2)
findAssocs(dtm, "democrat", corlimit=0.2)
findAssocs(dtm, "brown", corlimit=0.2)
findAssocs(dtm, "minister", corlimit=0.2)

t <- c("blair", "prime_minister", "govern", "agree","liberal_democrat", "general", "elect", "people", 
       "olympiad", "champion", "title", "person", "final", "second", 
       "labour", "first", "win", "set", "match", "game" )
plot(dtm, terms = t, corThreshold = 0.2, attrs=list(node=list(fontsize=55, shape="rectangle")), 
     main="Associations whit cor>0.2")



#___________________________________________________________________________________________________________
#                                        CLUSERING DEI DOCUMENTI
#__________________________________________________________________________________________________________
mat <- as.matrix(dtm)
#type <- c(rep("p", length(grep("p", rownames(mat)))), rep("s", length(grep("s", rownames(mat)))))

# verifica della presenza di termini in comune tra articoli di politica e di sport
dtm.p <- dtm[1:417,]     # 417 320
dtm.s <- dtm[418:928,]   # 511 320
freq.p <- colSums(as.matrix(dtm.p))
freq.s <- colSums(as.matrix(dtm.s))

f.p <- findFreqTerms(dtm.p, lowfreq = 100)
f.s <- findFreqTerms(dtm.s, lowfreq = 100)
f.p   #"allow" "announce" "back" "blair" "british" "brown"  ....
f.s   #"back" "david" "end"  "first"  "follow" "home" "move"...

common <- c()
for(i in 1:length(f.p)) {tmp <- c()
                        for (j in 1:length(f.s)) {tmp <- c(tmp, ifelse(f.p[i]==f.s[j], T, F))}
                        common[i]<-ifelse(sum(tmp==T)==1, T, F)
                        }
f.p[common]
#  [1] "back"     "david"    "end"      "first"    "follow"   "home"     "move"     "next"     "right"    "taken"   
#[11] "win"      "work"     "believe"  "decision" "expect"   "nation"   "number"   "set"      "claim"    "hope"    
#[21] "last"     "sayeed"   "wantage"  "yearned"  "face"     "wales"    "much"     "former"   "place"    "run"     
#[31] "world"    "way"      "european"

#set.seed(123)
par(mfrow=c(1,2))
wordcloud(names(freq.p), freq.p, min.freq = 150, random.color=TRUE, colors=rainbow(7), fixed.asp=T) #politica
wordcloud(names(freq.s), freq.s, min.freq = 150, random.color=TRUE, colors=rainbow(7), fixed.asp=T) #sport
par(mfrow=c(1,1))

# rimozione delle parole in comune tra gli articoli di politica e di sport
common.words <- f.p[common]    
new.articles <- tm_map(new.articles, removeWords, c(stopwords("english"), common.words) ) 
dtm <- DocumentTermMatrix(new.articles, control=list(wordLengths=c(3,25), bounds=list(global = c(80, 840))))

dtm.class <- dtm   #dtm usata per la classificazione

eliminaRighe <- function(a, r){tmp <- c()
                              el <- rep(NA ,length = length(a) )
                              for(i in 1:length(a)) {
                                                    for (j in 1:length(r)){
                                                    tmp[j] <- ifelse(a[i]==r[j], T, F) 
                                                    #T=nome riga = rem[i], F=nome riga != rem[i]
                                                    }
                              el[i] <- ifelse( sum(tmp==T)==1 , F, T)
                              # T=riga da mantenere, F=riga da eliminare
                              tmp <- c()
                              }
                              return(el)}







# ~ CLUSTERING GERARCHICO ~

mat <- as.matrix(dtm)

mysilhouette <- function(g, d, k) {s <- c()
                                k <- c(2:k)
                                for (i in 1:length(k)){c <- cutree(g, k=k[i])
                                                       si <- silhouette(c, dist=d)
                                                       s <- c(s, sum(si[,3])/length(c))}
                                return(s)
}


# 1) distanza euclidea - linkage ward

d.e <- dist(scale(mat), method="euclidean")
groups.e <- hclust(d.e, method = "ward.D")
sil.e <- mysilhouette(groups.e, d.e, 6)
sil.e
#  0.12796583   2cluster *
#  0.07128152   3cluster
#  0.07504298   4cluster
# -0.03224627   5cluster 
# -0.02885764   6cluster
k.e = which.max(sil.e)+1
k.e
plot(groups.e, hang = -1, main="Articles clustering with ward linkage")
rect.hclust(groups.e, k=k.e)

#outliers? No dal dendogramma, nel clusplot si possibili outliers
clusplot(mat, cutree(groups.e, k=2), color=T, shade=T, labels=2, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1  )

#rimozione outliers
out <- c("p (290).txt","p (293).txt", "p (380).txt")
articoli <- rownames(mat)
mat.no.out <- mat[eliminaRighe(articoli, out),]
dim(mat.no.out) #925 287
d.e <- dist(scale(mat.no.out), method="euclidean")
groups.e <- hclust(d.e, method = "ward.D")
sil.e <- mysilhouette(groups.e, d.e, 6)
sil.e 
# 0.0449114086  0.0443274164  0.0351380435 -0.0001978756 -0.0599437373

clusplot(mat.no.out, cutree(groups.e, k=2) , color=T, shade=T, labels=F, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1 )
plot(groups.e, hang = -1, main="Articles clustering with ward linkage")
rect.hclust(groups.e, k=k.e)


# 2) distanza euclidea - linkage complete

d.e <- dist(scale(mat), method="euclidean")
groups.e <- hclust(d.e, method = "complete")
sil.e <- mysilhouette(groups.e, d.e, 6)
sil.e
#  CON OUTLIERS                      
#  0.7189926   2cluster *               
#  0.6287788   3cluster         
#  0.5937252   4cluster               
#  0.5926523   5cluster               
#  0.4070877   6cluster                
k.e = which.max(sil.e)+1
k.e
plot(groups.e, hang = -1, main="Articles clustering with complete linkage")
rect.hclust(groups.e, k=k.e)     #non è possibile rappresentare i due rettangoli
                                #in modo che distinguano i due gruppi
#outliers? Si
clusplot(mat, cutree(groups.e, k=2) , color=T, shade=T, labels=2, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1 )

#rimozione outliers
out <- c("p (290).txt","p (293).txt", "p (380).txt", "s (491).txt", "s (371).txt",
         "s (58).txt", "s (69).txt", "s (476).txt", "s (61).txt")
articoli <- rownames(mat)
mat.no.out <- mat[eliminaRighe(articoli, out),]
dim(mat.no.out) #919 287
d.e <- dist(scale(mat.no.out), method="euclidean")
groups.e <- hclust(d.e, method = "complete")
sil.e <- mysilhouette(groups.e, d.e, 6)
sil.e
# 0.4021295 0.3485788 0.3358471 0.3332978 0.1185285
clusplot(mat.no.out, cutree(groups.e, k=2) , color=T, shade=T, labels=2, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1 )

# SENZA OUTLIERS INDICATI SOPRA (OUT) 
#  0.4021295     2clusters
#  0.3485788     3clusters
#  0.3358471     4clusters
#  0.3332978     5clusters
#  0.1185285     6clusters


# NB: nel clustering con distanza euclidea e legame completo non è possibile individuare due clustering
#     in modo chiaro perchè anche eliminando gli outliers, continuano ed evidenziarsene altri. Questo
#     impedisce l'utlizzo del legame completo





# 3) distanza di manhattan - ward linkage

d.m <- dist(scale(mat), method="manhattan")
groups.m <- hclust(d.m, method = "ward.D")
sil.m <- mysilhouette(groups.m, d.m, 6)
sil.m
#  0.08040271   2cluster *
#  0.05174100   3cluster
#  0.05799654   4cluster
#  0.01546301   5cluster
#  0.01889040   6cluster
k.m = which.max(sil.m)+1
k.m
plot(groups.m, hang=-1, main ="Articles clustering with ward likage", xlab = "distanza di manhattan")
rect.hclust(groups.m, k=k.m)

#outliers? Si
clusplot(mat, cutree(groups.m, k=2), color=T, shade=T, labels=2, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1  )

#rimozione outliers
out <- c("p (290).txt","p (293).txt", "p (380).txt" )
articoli <- rownames(mat)
mat.no.out <- mat[eliminaRighe(articoli, out),]
d.m <- dist(scale(mat.no.out), method="manhattan")
groups.m <- hclust(d.m, method = "ward.D")
sil.m <- mysilhouette(groups.m, d.m, 6)
sil.m
# 0.133411990 -0.009276007 -0.004334398 -0.007383507 -0.003441197
clusplot(mat.no.out, cutree(groups.m, k=2) , color=T, shade=T, labels=F, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1, main = "Clustplot ward linkage" )
plot(groups.m, hang=-1, main ="Articles clustering with ward likage", xlab = "distanza di manhattan")
rect.hclust(groups.m, k=k.m)
groups.m$labels <- c(1:925)


#risultato migliore
c <- cutree(groups.m, k=2)
rownames(mat.no.out)[which(c==2)]   #88 su 597 sono p
rownames(mat.no.out)[which(c==1)]   #2 su 328 sono s


# 4) distanza di manhattan - complete linkage

d.m <- dist(scale(mat), method="manhattan")
groups.m <- hclust(d.m, method = "complete")
sil.m <- mysilhouette(groups.m, d.m, 6)
sil.m
#  0.8034007   2cluster *   
#  0.7178087   3cluster
#  0.7063131   4cluster
#  0.6005846   5cluster
#  0.5384110   6cluster
k.m = which.max(sil.m)+1
k.m
plot(groups.m, hang=-1, main ="Articles clustering with complete likage", xlab = "distanza di manhattan")
rect.hclust(groups.m, k=k.m)


#outliers? Si
clusplot(mat, cutree(groups.m, k=2) , color=T, shade=T, labels=2, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1, )

#rimozione outliers
out <- c("p (290).txt","p (293).txt", "p (380).txt" , "s (491).txt", "s (371).txt",
         "s (58).txt", "s (69).txt", "s (61).txt")
articoli <- rownames(mat)
mat.no.out <- mat[eliminaRighe(articoli, out),]
d.m <- dist(scale(mat.no.out), method="manhattan")
groups.m <- hclust(d.m, method = "complete")
sil.m <- mysilhouette(groups.m, d.m, 6)
sil.m  
#0.51920589 0.04254882 0.04272737 0.04217143 0.04072145
clusplot(mat.no.out, cutree(groups.m, k=2) , color=T, shade=T, labels=F, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1, main = "Clusplot complete linkage")
groups.m$labels <- c(1:920)
plot(groups.m, hang=-1, main ="Articles clustering with complete likage", xlab = "distanza di manhattan")
rect.hclust(groups.m, k=k.m)

#NB: nel clustering con distanza di manhattan e legame completo non è possibile individuare due clustering
#     in modo chiaro perchè anche eliminando gli outliers, continuano ed evidenziarsene altri. Questo
#     impedisce l'utlizzo del legame completo




# 5) distanza coeff. di pearson (dissimilarità) - ward linkage

d.p <- as.dist((1-cor(t(mat)))/2)
groups.p <- hclust(d.p, method="ward.D")
sil.p <- mysilhouette(groups.p, d.p, 6)
sil.p
#   0.12110649   2cluster *
#   0.10073578   3cluster
#   0.09074576   4cluster
#   0.07422913   5cluster
#   0.07907640   6cluster
k.p = which.max(sil.p)+1
k.p
plot(groups.p, hang=-1, main="Articles clustering with ward linkage" )
rect.hclust(groups.p, k=k.p)

#outliers? Si
clusplot(mat, cutree(groups.p, k=2) , color=T, shade=T, labels=2, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1, )

#rimozione degli outliers
out <- c("p (290).txt","p (293).txt", "p (380).txt" )
articoli <- rownames(mat)
mat.no.out <- mat[eliminaRighe(articoli, out),]
d.p <- as.dist((1-cor(t(mat.no.out)))/2)
groups.p <- hclust(d.p, method="ward.D")
sil.p <- mysilhouette(groups.p, d.p, 6)
sil.p
# 0.12094077 0.10116513 0.09162293 0.07503693 0.07992270
clusplot(mat.no.out, cutree(groups.p, k=2) , color=T, shade=T, labels=F, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1, )




# 6) distanza coeff. di pearson (dissimilarità) - complete linkage

d.p <- as.dist((1-cor(t(mat)))/2)
groups.p <- hclust(d.p, method="complete")
sil.p <- mysilhouette(groups.p, d.p, 6)
sil.p
#  0.11637443   2cluster *
#  0.09007173   3cluster
#  0.06583808   4cluster
#  0.06519244   5cluster
#  0.05185965   6cluster
k.p = which.max(sil.p)+1
k.p
plot(groups.p, hang=-1, main="Articles clustering with ward linkage" )
rect.hclust(groups.p, k=k.p)

#outliers? Si
clusplot(mat, cutree(groups.p, k=2) , color=T, shade=T, labels=2, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1, )

#rimozione degli outliers
out <- c("p (290).txt","p (293).txt", "p (380).txt" )
articoli <- rownames(mat)
mat.no.out <- mat[eliminaRighe(articoli, out),]
d.p <- as.dist((1-cor(t(mat.no.out)))/2)
groups.p <- hclust(d.p, method="complete")
sil.p <- mysilhouette(groups.p, d.p, 6)
sil.p
# 0.11774680 0.09160959 0.06345492 0.06184683 0.04628583
clusplot(mat.no.out, cutree(groups.p, k=2) , color=T, shade=T, labels=F, lines=0, col.clus =c("springgreen", "orange"),
         col.p = "black", cex =1, )






# ~ CLUSTERING PARTIZIONALE ~

#remove()
mat <- scale(mat)

# 1) k-means
#set.seed(123)
km <- kmeans(mat, centers=2, nstart = 50)
clusplot(mat, km$cluster, color=T, shade=T, labels=F, lines=0, col.clus =c("pink", "lightslateblue"),
         col.p = "black", cex =1, main = "Clusplot DocumentTermMatrix completa")
sil.km <- silhouette(km$cluster, dist = dist(mat))
sum(sil.km[,3])/length(km$cluster)
# 0.08788355  silhouette
# outliers: "p (290).txt","p (380).txt","p (293).txt","s (491).txt","s (371).txt",
#            "s (58).txt"




# rimozione degli outliers
rem <- c("p (290).txt","p (380).txt","p (293).txt","s (491).txt","s (371).txt",
         "s (58).txt" )
articoli <- rownames(mat)
mat2 <- mat
mat2<- mat2[eliminaRighe(articoli, rem),]
dim(mat2)


#clusplot senza outliers
#set.seed(123)
km2 <- kmeans(scale(mat2), centers=2, nstart = 50)
clusplot(mat2, km2$cluster, color=T, shade=T, labels=F, lines=0, col.clus =c("pink", "lightslateblue"),
         col.p = "black", cex =1, main = "Clusplot kmeans, no outliers")
sil.km2 <- silhouette(km2$cluster, dist = dist(mat2))
sum(sil.km2[,3])/length(km2$cluster)
#  0.07361279   euclidea



test <- c(2:5)
sil.vec <- c()
for(i in 1:length(test)){km <- kmeans(scale(mat2), centers = test[i], nstart=50)
                         sil <- silhouette(km$cluster, dist = dist(mat2))
                         sil.vec[i] <-sum(sil[,3])/length(km$cluster)}

test[which.max(sil.vec)]
#la silhouette massima si ha con 2 clusters

rownames(mat2)[which(km2$cluster==1)]   #3 s su 370
rownames(mat2)[which(km2$cluster==2)]   #47 sono p su 552
km$tot.withinss   #242931.6
km2$tot.withinss  #253899.8

km$betweenss    #21395.39
km2$betweenss   #10427.18




# 2) k-medoids
#set.seed(123)
pk <- pamk((mat), krange = 2:10)
k.pk <- pk$nc
k.pk  #2
clusplot(mat , pk$pamobject$clustering , color=T, shade=T, labels=2, lines=0, col.clus =c("lavenderblush3", "olivedrab2"),
         col.p = "black", main = "Clusplot k-medoids" )
pk$pamobject$silinfo$clus.avg.widths  #silhouette dei due gruppi
#-0.03827619  0.04986563
pk$pamobject$silinfo$avg.width  #silhouette generale,  è molto bassa
# 0.01842711

#rimuovo possibili outliers
r <- c("p (290).txt","p (380).txt","p (293).txt","s (491).txt","s (371).txt" )
articoli <- rownames(mat)
mat3 <- mat
mat3<- mat3[eliminaRighe(articoli, r),]
dim(mat3)

pk3 <- pamk(scale(mat3), krange = 2:10)
k.pk3 <- pk$nc
k.pk3  #2
clusplot(scale(mat3) , pk3$pamobject$clustering , color=T, shade=T, labels=F, lines=0, col.clus =c("lavenderblush4", "olivedrab2"),
         col.p = "black", main="Clusplot k-medoids, no outliers" )
pk3$pamobject$silinfo$clus.avg.widths  #silhouette dei due gruppi
# -0.02470694  0.03549412
pk3$pamobject$silinfo$avg.width  #silhouette generale,  è molto bassa
# 0.01429656

pk3$pamobject$clusinfo
rownames(mat3)[which(pk3$pamobject$clustering==2)]   #296 p su 598
rownames(mat3)[which(pk3$pamobject$clustering==1)]   #118 p su 325



#___________________________________________________________________________________________________________
#                                         CLUSTERING DEI TERMINI
#___________________________________________________________________________________________________________

tdm <- TermDocumentMatrix(new.articles, control=list(wordLengths=c(3,30), bounds=list(global = c(80, 840))))
mat.t <- as.matrix(tdm)
dim(mat.t) #`287 928`
freq.t <- rowSums(mat.t)

mat.t.rid <- subset(mat.t, subset = freq.t>180 ) #mat.t ridotta
freq.t2 <- rowSums(mat.t.rid) 
length(rownames(mat.t.rid))    #114
mat.t.rid <- scale(mat.t.rid)
dim(mat.t.rid)   #114 928



# ~ CLUSTERING GERARCHICO ~

# 1) distanza euclidea - ward linkage

dt.e <- dist(mat.t.rid, method = "euclidean")
groupst.e <- hclust(dt.e, method = "ward.D")
st.e <- mysilhouette(groupst.e, dt.e, 10)
st.e    
# 0.05030666   silhouette massima con 2 cluster ma comunque bassa
kt.e = which.max(st.e)+1
kt.e
plot(groupst.e, hang = -1, main = "Terms clustering with ward linkage", xlab = "distanza euclidea")
rect.hclust(groupst.e, k=kt.e)
ct <- cutree(groupst.e, k=2)
rownames(mat.t.rid)[which(ct==2)]
rownames(mat.t.rid)[which(ct==1)]



# 2) distanza euclidea - complete linkage
dt.e <- dist(mat.t.rid, method = "euclidean")
groupst.e <- hclust(dt.e, method = "complete")
st.e <- mysilhouette(groupst.e, dt.e, 10)
st.e    
# 0.1977596   silhouette massima con 3 cluster ma ci sono due outliers
kt.e = which.max(st.e)+1
kt.e
plot(groupst.e, hang = -1, main = "Terms clustering with complete linkage")
rect.hclust(groupst.e, k=kt.e)


#c'è un outliers "play", "game"
#rimozione outlier
rem.t <- c( "play", "game")
termini <- rownames(mat.t.rid)
el <- eliminaRighe(termini, rem.t)
mat.t.no.out <- mat.t.rid[el,]
dim(mat.t.no.out)
dt.e <- dist(mat.t.no.out, method = "euclidean")
groupst.e <- hclust(dt.e, method = "complete")
st.e <- mysilhouette(groupst.e, dt.e, 10)
st.e
# 0.2025723
kt.e = which.max(st.e)+1
kt.e
plot(groupst.e, hang = -1, main = "Terms clustering with complete linkage")
rect.hclust(groupst.e, k=kt.e)



# 3) distanza di manhattan - ward linkage
dt.m <- dist(mat.t.rid, method = "manhattan")
groupst.m <- hclust(dt.m, method = "ward.D")
st.m <- mysilhouette(groupst.m, dt.m, 10)
st.m
# 0.09755961  silhouette massima con 2 cluster ma comunque bassa
kt.m = which.max(st.m)+1
kt.m
plot(groupst.m, hang = -1, main = "Terms clustering with ward linkage", xlab = "distanza di manhattan")
rect.hclust(groupst.m, k=kt.m)



# 4) distanza di manhattan - complete linkage
dt.m <- dist(mat.t.rid, method = "manhattan")
groupst.m <- hclust(dt.m, method = "complete")
st.m <- mysilhouette(groupst.m, dt.m, 10)
st.m
#0.09947523  silhouette massima con 2 cluster ma comunque bassa
kt.m = which.max(st.m)+1
kt.m
plot(groupst.m, hang = -1, main = "Terms clustering with complete linkage", xlab="distanza di manhattan")
rect.hclust(groupst.m, k=kt.m)
ct <- cutree(groupst.m, k=2)
rownames(mat.t.rid)[which(ct==2)]
rownames(mat.t.rid)[which(ct==1)]



# 5) distanza coeff. di pearson - ward linkage
dt.p <- as.dist((1-cor(t(mat.t.rid)))/2)
groupst.p <- hclust(dt.p, method = "ward.D")
st.p <- mysilhouette(groupst.p, dt.p, 10)
st.p
# 0.09840733   silhouette massima con 2 cluster ma comunque bassa
kt.p = which.max(st.p)+1
kt.p
plot(groupst.p, hang=-1, main = "Terms clustering with ward linkage", xlab = "distanza con corr. di Pearson")
rect.hclust(groupst.p, k=kt.p)

ct <- cutree(groupst.p, k=2)
rownames(mat.t.rid)[which(ct==2)]
rownames(mat.t.rid)[which(ct==1)]



# 6) distanza coeff. di pearson - complete linkage
dt.p <- as.dist((1-cor(t(mat.t.rid)))/2)
groupst.p <- hclust(dt.p, method = "complete")
st.p <- mysilhouette(groupst.p, dt.p, 10)
st.p
# 0.079131415  silhouette massima con 2 cluster ma comunque bassa
kt.p = which.max(st.p)+1
kt.p
plot(groupst.p, hang=-1, main = "Terms clustering with complete linkage")
rect.hclust(groupst.p, k=kt.p)



 
 
# CLUSTERING PARTIZIONALE

kmeansTopK <- function(x, k){test <- c(2:k)
                             s <- c()
                             for(i in 1:length(test)){km <- kmeans(x, centers = test[i], nstart=50)
                                                      sil <- silhouette(km$cluster, dist=dist(x))
                                                      s <- c(s, sum(sil[,3])/length(km$cluster))}
                             return(c(test[which.max(s)]+1, s[which.max(s)]))
                        }


# 1) k-means
#remove(km.t)
mat.t <- scale(mat.t)

topk.t <- kmeansTopK(mat.t, 10)
topk.t
#4    sil=  0.3153342
#set.seed(123)
km.t <- kmeans(mat.t, centers = topk.t[1], nstart = 100)
clusplot(as.matrix(dist(mat.t)), km.t$cluster, color=T, shade=T, labels=2, lines=0, 
         col.clus =c("green", "orange", "blue", "red"), col.p = "black", cex =1, 
         main="Clusplot k-means")

topk.t[2]
#0.3153342 silhouette

#ci sono outliers: "game", "play", "england", "player"

# rimozione degli outliers
rem <- c("game", "play", "england", "player")
termini <- rownames(mat.t)
mat.t2 <- mat.t
mat.t2<- mat.t2[eliminaRighe(termini, rem),]
dim(mat.t2)

#set.seed(56)
topk.t2 <- kmeansTopK(mat.t2, 10)
topk.t2
# 3    0.371578

km.t2 <- kmeans(mat.t2, centers = topk.t2[1], nstart = 100)
clusplot(as.matrix(dist(mat.t2)), km.t2$cluster, color=T, shade=T, labels=2, lines=0, 
         col.clus =c("green", "orange", "blue", "red"), col.p = "black", cex =1, 
         main="Clusplot k-means, no outliers")


length(labels(km.t2$cluster)[which(km.t2$cluster==1)]) #52 termini
length(labels(km.t2$cluster)[which(km.t2$cluster==2)]) #12 termini
length(labels(km.t2$cluster)[which(km.t2$cluster==3)]) #219 termini
#labels(km.t$cluster)[which(km.t$cluster==4)] #12 termini



#___________________________________________________________________________________________________________
#                                       CLASSIFICAZIONE DEI DOCUMENTI
#___________________________________________________________________________________________________________
library(class)
library(caret)
#remove(accuracy_knn_models, testo)

#classificazione
matrice=as.matrix(dtm.class)
doc=as.data.frame(matrice)
type=c(rep(0,417),rep(1,511)) # 0=poilitca, 1=sport
doc=cbind(doc,type)
dim(doc) #928 x 288


#divido in training e test  
n = nrow(doc)

#set.seed(123)
idx = sample(c(rep("Tr",0.701*n), rep("Te",0.3*n)))
tr = doc[idx == "Tr",]    #650 documenti
dim(tr)
te = doc[idx == "Te",]    #278 documenti
dim(te)

prop.table(table(tr$type))  #0.4415385 0.5584615 
prop.table(table(te$type))  #0.4676259 0.5323741 



#testo k da 2 a 15(1 genera overfitting)
K = c(2:15)
accuracy_knn_models <- NULL

set.seed(123)
# Calcolo accuratezza per ogni valore di K - TRAINING
for (k in K){
  model = knn(tr[,1:287], tr[,1:287], cl=tr[, 288], k=k)
  accuracy =   confusionMatrix(factor(tr[, 288]), model)$overall["Accuracy"]
  accuracy_knn_models = c(accuracy_knn_models, accuracy)
}  

plot(K, accuracy_knn_models, type = "b")
(max(accuracy_knn_models))
#0.96
which((accuracy_knn_models)==max(accuracy_knn_models))+1
#2
knn.tr <- knn(tr[,1:287], tr[,1:287], cl=tr[, 288], k=2)
confusionMatrix(factor(tr[, 288]), knn.tr)
# Accuracy : 0.9631 
#Sensitivity : 1.0000          
#Specificity : 0.9380      



#migliore k=2 lo provo nel - TEST
#set.seed(76)
knn.test <- knn(tr[,1:287],te[,1:287],tr[,288],k=2,prob=T)
knn.test
length(knn.test)    #278
summary(knn.test) #101 177 
confusionMatrix(factor(te[, 288]), knn.test)
#Accuracy : 0.8957  
#Sensitivity : 1.0000         
#Specificity : 0.8362 



library(ROCR)
probs <- attributes(knn.test)$prob
prob.knn <- 2*ifelse(knn.test==0, 1-probs, probs)-1
roc.pred <- prediction(prob.knn, te[,288])
roc.perf <- performance(roc.pred, "tpr", "fpr")
auc <- performance(roc.pred, measure = "auc")@y.values
auc
#0.95
plot(roc.perf, colorize=T, main="KNN-2", lwd=2)


#__________________________________________________________________________________________________
#                                                FINE
#__________________________________________________________________________________________________
