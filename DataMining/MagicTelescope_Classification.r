magicTelescope <- read.csv("magic04.data", header = F)
x <- c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long","fM3Trans","fAlpha","fDist","class")
colnames(magicTelescope) <- x
View(magicTelescope)
str(magicTelescope)
summary(magicTelescope)

#plot(magicTelescope)
#_____________________________________________________________________________________________
#                                     SUDDIVISIONE DEL DATASET
#_____________________________________________________________________________________________

size <- floor(nrow(magicTelescope)*0.75)                              #14265
set.seed(121)
trn.val.index <- sample(seq_len(nrow(magicTelescope)), size = size)

trn.val.original <- magicTelescope[trn.val.index,]                      #training+validation set
test.original <- magicTelescope[-trn.val.index,]                        #test set

#suddivido ulteriormente il trn.val in training e validation
trn.size <- floor(nrow(trn.val.original)*0.65)                         
trn.index <- sample(seq_len(nrow(trn.val.original)), size = trn.size)  

training.original <- trn.val.original[trn.index,]             #9272   
validation.original <- trn.val.original[-trn.index,]          #4993

#mantengo questi set originali e ne creo una copia da modificare
training <- training.original
validation <- validation.original
test <- test.original

#verifico il bilanciamento delle classi nei set
round(prop.table(table(training$class)), 2)
round(prop.table(table(validation$class)), 2)
round(prop.table(table(test$class)), 2)
round(prop.table(table(magicTelescope$class)), 2)
#le classi sono bilanciate allo stesso modo circa quindi non devo effettuare bilanciamenti

#_______________________________________________________________________________________________
#                                 ANALISI PRELIMINARI DEI DATI
#_______________________________________________________________________________________________


# 1)___MISSING E VALORI ANOMALI____

any(is.na(magicTelescope))   #non ci sono missing
str(training)
summary(training[,-11])
#fLength: rappresenta la lunghezza in millimetri dell'asse maggiore di un'ellisse quindi deve 
#         avere valori maggiori o uguali a 0
#fWidth: presenta 0 come valore di minimo ma essendo una variabili espressa in millimetri e poichè 
#        rappresenta uno degli assi di un'ellisse potrebbe avere valore 0
#fSize: log in base 10 della somma del contenuto di tutti i pixel, può assumere solo valori 
#        positivi e diversi da 0
#fConc: rapporto della somma dei due pixel più alti / fSize, assume valori positivi
#fConc1: rapporto del più alto pixel / fSize, assume valori positivi
#fAsym: distanza del pixel più alto dal centro, proiettata sull'asse maggiore, assume valori reali
#       nonostante sia una misura in mm perchè il segno indica la direzione
#fM3Long: radice cubica del terzo momento lungo l'asse maggiore, assume valori reali nonostante 
#         sia in mm
#fM3Trans: radice terza del terzo momento lungo l'asse minore, assume valori reali nonostante 
#          sia in mm
#fAlpha: rappresenta l'angolo tra l'asse maggiore dell'ellisse e il vettore che collega l'origine
#        al punto, quindi può essumere valore 0
#fDist: distanza dall'origine al centro dell'ellisse


#salvo medie e mediane delle covariate per poterle sostituire in seguito se necessario
medianAll.trn <- matrix(NA, nrow = 1, ncol = ncol(training)-1)
colnames(medianAll.trn) <- colnames(training)[-11]
rownames(medianAll.trn) <- "Median" 
meanAll.trn <- matrix(NA, nrow = 1, ncol = ncol(training)-1)
colnames(meanAll.trn) <- colnames(training)[-11]
rownames(meanAll.trn) <- "Mean"

for (i in 1:(dim(training)[2]-1)){
  medianAll.trn[,i] <- median(training[,i])
  meanAll.trn[,i] <- mean(training[,i])
}


# 2) ____ANALISI DISTRIBUZIONE DELLE VARIABILI_____

training <- training.original
summary(training)
boxplot(training[,1:10])
library(car)


# ~ fLength ~
boxplot(training$fLength)
symbox(training$fLength)
hist(training$fLength)
488/9272*100              #5% di outliers
#la variabile fLength presenta un elevato numero di outliers e presenta un'evidente asimmetria
#positiva, la trasformata logaritmo risolve il problema:
par(mfrow=c(1,2))
boxplot(log(training$fLength))
hist(log(training$fLength))
par(mfrow=c(1,1))


# ~ fWidth ~
training$fWidth[1:10]
training[training$fWidth==0, "fWidth"] <- medianAll.trn[,"fWidth"]

boxplot(training$fWidth)
hist(training$fWidth)
symbox(training$fWidth, powers = c(0,0.5,1))
759/9272*100            #8% di outliers
#la variabile fWidth presenta una forte asimmetria positiva e un elevato numero di outliers, la
#trasformazione logaritmica in parte risolve il problema di asimmetria, ma resta comunque un 
#alto numero di outliers
par(mfrow=c(1,2))
boxplot(log(training$fWidth))
hist(log(training$fWidth))
par(mfrow=c(1,1))
#osservando i grafici trasformati probabilmente non è la trasformazione migliore


# ~ fSize ~
boxplot(training$fSize) 
hist(training$fSize)
symbox(training$fSize)   
192/9272*100           #2% di outliers
#la variabile fSize presenta abbastanza simmetria e un basso numero di outliers, la 
#trasformazione che sembra meglio risolvere il problema degli outliers è il reciproco
par(mfrow=c(1,2))
boxplot((training$fSize)^(-1))$out
hist(log(training$fSize)^(-1))
par(mfrow=c(1,1))
#è già abbastanza normale senza trasformarla


# ~ fConc ~
boxplot(training$fConc)        #0% outliers
hist(training$fConc)
symbox(training$fConc)
#la variabile fConc presenta leggera asimmetria positiva e non presenta outliers, non si applicano
#trasformazioni


# ~ fConc1 ~
boxplot(training$fConc1)
hist(training$fConc1)
symbox(training$fConc1, powers=c( 0.5, 1) )
72/9272*100        #0.78% di outliers
#la variabile fConc1 presenta lieve asimmetria positiva e outliers quasi assenti
par(mfrow=c(1,2))
boxplot((training$fConc1)^(0.5))
hist((training$fConc1)^(0.5))
par(mfrow=c(1,1))
#migliora anche la distribuzione come si vede dall'istogramma


# ~fAsym ~
boxplot(training$fAsym)
hist(training$fAsym)
symbox(training$fAsym, powers=c(0.5, 1, 3))
plot(dist(training$fAsym), add=T)
857/9272*100            #9% di outliers
#La variabile fAsym presenta un elevatissimo numero di outliers, la trasformazione logaritmica
#non è applicabile perchè la variabile presenta valori negativi


# ~ fM3Long ~
boxplot(training$fM3Long)
hist(training$fM3Long)
symbox(training$fM3Long, powers = c(0.5,1, 3))
575/9272*100           #6% di outliers
#La variabile fM3Long presenta un modesto numero di outliers ed è abbastanza simmetrica


# ~ fM3Trans ~
boxplot(training$fM3Trans)
hist(training$fM3Trans)
symbox(training$fM3Trans, powers = c(0.5,1,3))
410/9272*100      #4% di outliers
#La variabile fM3Long presenta un modesto numero di outliers ed è abbastanza simmetrica


# ~ fAlpha ~
#non sostituisco gli elementi con 0 perchè è solo 1 ed essendo un ampiezza in gradi potrebbe 
#essere giusto

boxplot(training$fAlpha)      #0% outliers
hist(training$fAlpha)
symbox(training$fAlpha)
#no outliers ma la trasformazione è asimmetrica
hist((training$fAlpha)^(0.5))
boxplot((training$fAlpha)^(0.5))
#non applico trasformazioni


# ~ fDist ~
boxplot(training$fDist)
hist(training$fDist)
symbox(training$fDist)
60/9272*100              #0.64% outliers
#se non fosse per gli outliers sarebbe simmetrica, inoltre l'istogramma ha una buona forma,
#non trasformo la variabile.


#applico le trasformazioni decise
training[, "fLength"] <- log(training$fLength)



# 2) ____ANALISI DELLA CORRELAZIONE TRA ESPLICATIVE_____

correl <- cor(training[,-11])
round(correl, 2)

#grafico
library(corrplot)
corrplot(correl, method="shade", type ="upper",diag = F, addCoef.col = "black", tl.col = "black")


# 4) ____SALVO MEDIE E SD____

indicators <- matrix(NA, nrow = 2, ncol = dim(training)[2]-1)
for (i in 1:(dim(training)[2]-1)){
                                   indicators[1,i] <- mean(training[,i])
                                   indicators[2,i] <- sd(training[,i])
                                  }
rownames(indicators) <- c("Mean", "StDev" )
colnames(indicators) <- colnames(training)[-11]
round(indicators, 2)                       #matrice di medie e devianze dopo le trasformazioni



#5) ____STANDARDIZZAZIONE DELLE VARIABILI_____

for (i in 1:(dim(training)[2]-1)){
  training[,i] <- (training[,i]-indicators["Mean",i])/indicators["StDev",i]
}


training <- training.original
#___________________________________________________________________________________________
#                             ANALISI DEI COMPONENTI PRINCIPALI
#___________________________________________________________________________________________

training1 <- training
#data l'elevata correlazione tra alcune variabili si valuta di ridurre la dimensionalità
#mantnenedo le iformazioni date da tutte le variabili
#valuto di unIre le prime 3 variabili e la 4 e 5a

# ~ unisco 1:3 variabili ~

pca <- princomp(x=training[,1:3])
plot(pca, type="lines") 
summary(pca)
#mi fermo alla prima componente perchè è quella che massimizza la varianza
training1[,1] <- training1[,1]*pca$loadings[1,1]+training1[,2]*pca$loadings[2,1]+
                training1[,3]*pca$loadings[3,1]
colnames(training1)[1] <- "fLenWidSiz"


# ~ unisco 4a e 5a variabili ~

pca2 <- princomp(x=training[,4:5])
plot(pca2, type="lines")
pca2$loadings
summary(pca2)
#mi fermo alla prima componente perchè massimizza la varianza
training1[,4] <- training1[,4]*pca$loadings[1,1] + training1[,5]*pca$loadings[2,1]
training1 <- training1[,-c(2,3,5)]

summary(training1)
 

# ~ rivaluto la correlazione ~

correl2 <- cor(training1[,-8])
round(correl2, 2)
corrplot(correl2, method="shade", type = "upper", diag = F, addCoef.col = "black", tl.col = "black")
#valuto la distribuzione
boxplot(training1$fLenWidSiz)
symbox(training1$fLenWidSiz)
hist(training1$fLenWidSiz)
boxplot(training1$fConc)
hist(training1$fConc)

summary(training)
____________________________________________________
#valuto una pca delle prime 5 variabili
training2 <- training
pca3 <- princomp(x=training[,1:5])
summary(pca3)
plot(pca3, type = "lines")
training2[,1] <- 
____________________________________________________
#tengo TRAINING1
training <- training1

#_______________________________________________________________________________________________
#                   VERIFICA DELLE ASSUNZIONI PER L'APPLICAZIONE DEI MODELLI
#_______________________________________________________________________________________________

summary(training)
# valuto la normalità delle distribuzioni e la presenza di omochedasticità o eteroschedasticità
# tra le classi


# a) ___VALUTAZIONE DELLA NORMALITA'____

# ~ qqplot ~
par(mfrow=c(1,2))
training$class <- ifelse(training$class==1, "g", "h")

#fLength
#qqnorm(training[training$class=="g", "fLength"])
#qqline(training[training$class=="g", "fLength"], col="red")
#qqnorm(training[training$class=="h", "fLength"])
#qqline(training[training$class=="h", "fLength"], col="red")
#sembrano esserci code pesanti in entrambe le classi
#ks.test(training[training$class=="g", "fLength"], "pnorm", 
#    mean(training[training$class=="g", "fLength"]), sd(training[training$class=="g", "fLength"]))
#ks.test(training[training$class=="h", "fLength"], "pnorm", 
#    mean(training[training$class=="h", "fLength"]), sd(training[training$class=="h", "fLength"]))
#rifiuto l'ipotesi di normalità


#fWidth
#qqnorm(training[training$class=="g", "fWidth"])
#qqline(training[training$class=="g", "fWidth"], col="red")
#qqnorm(training[training$class=="h", "fWidth"])
#qqline(training[training$class=="h", "fWidth"], col="red")
#non  c'è un buon fit per la normalità


#fSize
#qqnorm(training[training$class=="g", "fSize"])
#qqline(training[training$class=="g", "fSize"], col="red")
#qqnorm(training[training$class=="h", "fSize"])
#qqline(training[training$class=="h", "fSize"], col="red")
#se trasformassi con ^(-1) migliorerebbe
#ks.test(training[training$class=="g", "fSize"], "pnorm", 
#    mean(training[training$class=="g", "fSize"]), sd(training[training$class=="g", "fSize"]))
#ks.test(training[training$class=="h", "fSize"], "pnorm", 
#   mean(training[training$class=="h", "fSize"]), sd(training[training$class=="h", "fSize"]))
#rifiuto l'ipotesi di normalità per colpa delle code

#fLenWidSiz
qqnorm(training[training$class=="g", "fLenWidSiz"], main="fLenWidSiz - g")
qqline(training[training$class=="g", "fLenWidSiz"], col="red")
qqnorm(training[training$class=="h", "fLenWidSiz"], main="fLenWidSiz - h")
qqline(training[training$class=="h", "fLenWidSiz"], col="red")


#fConc
qqnorm(training[training$class=="g", "fConc"], main="fConc - g")
qqline(training[training$class=="g", "fConc"], col="red")
qqnorm(training[training$class=="h", "fConc"], main="fConc - h")
qqline(training[training$class=="h", "fConc"], col="red")
#ad eccezione delle code pesanti c'è un buon fit
ks.test(training[training$class=="g", "fConc"][], "pnorm", 
    mean(training[training$class=="g", "fConc"]), sd(training[training$class=="g", "fConc"]))
ks.test(training[training$class=="h", "fConc"], "pnorm", 
    mean(training[training$class=="h", "fConc"]), sd(training[training$class=="h", "fConc"]))


#fConc1
#qqnorm(training[training$class=="g", "fConc1"])
#qqline(training[training$class=="g", "fConc1"], col="red")
#qqnorm(training[training$class=="h", "fConc1"])
#qqline(training[training$class=="h", "fConc1"], col="red")
#ad eccezione delle code pesanti c'è un buon fit
#ks.test(training[training$class=="g", "fConc1"], "pnorm", 
#        mean(training[training$class=="g", "fConc1"]), sd(training[training$class=="g", "fConc1"]))
#ks.test(training[training$class=="h", "fConc1"], "pnorm", 
#        mean(training[training$class=="h", "fConc1"]), sd(training[training$class=="h", "fConc1"]))

#fAsym
qqnorm(training[training$class=="g", "fAsym"], main="fAsym")
qqline(training[training$class=="g", "fAsym"], col="red")
qqnorm(training[training$class=="h", "fAsym"], main="fAsym")
qqline(training[training$class=="h", "fAsym"], col="red")
#c'è un pessimo fit, non normale

#fM3Long
qqnorm(training[training$class=="g", "fM3Long"], main="fM3Long")
qqline(training[training$class=="g", "fM3Long"], col="red")
qqnorm(training[training$class=="h", "fM3Long"], main="fM3Long")
qqline(training[training$class=="h", "fM3Long"], col="red")
#per classe g fit quasi accettabile, per classe h pessimo fit

#fM3Trans
qqnorm(training[training$class=="g", "fM3Trans"], main="fM3Trans")
qqline(training[training$class=="g", "fM3Trans"], col="red")
qqnorm(training[training$class=="h", "fM3Trans"], main="fM3Trans")
qqline(training[training$class=="h", "fM3Trans"], col="red")
#pessimo fit, non normale

#fAlpha
qqnorm(training[training$class=="g", "fAlpha"], main="fAlpha")
qqline(training[training$class=="g", "fAlpha"], col="red")
qqnorm(training[training$class=="h", "fAlpha"], main="fAlpha")
qqline(training[training$class=="h", "fAlpha"], col="red")
#buon fit per la classe g ad eccezione delle code, pessimo fit per la classe h

#fDist
qqnorm(training[training$class=="g", "fDist"], main= "fDist - g")
qqline(training[training$class=="g", "fDist"], col="red")
qqnorm(training[training$class=="h", "fDist"], main= "fDist h")
qqline(training[training$class=="h", "fDist"], col="red")
#buon fit per entrambe le classi ed eccezione delle code un po' pesanti

par(mfrow=c(1,1))

# ~ linee di densità ~

library(ggplot2)
library(gridExtra)
density <- list()
variables <- colnames(training)[-8]
for (i in variables) {
  density[[i]] <- ggplot(training, aes_string(x=i, y="..density..", col="class") )+
    geom_density(y="..density..") + scale_color_manual(values = c("blue", "dark orange")) +
     theme(legend.position = "none", plot.title = element_text(i)) }
do.call(grid.arrange, c(density, nrow=3))

#si osserva che la distribuzione normale sembra essere rispettata da pochissime variabili
#fDist, fConc e fConc1 per la classe g e fSize, mentre per la classe h solo fSize e fDist.
#Assumiamo comunque una distribuzione normale sapendo che commetteremo un errore


# ~ shapiro test ~
samp <- sample(nrow(training), size=5000)
pval.shapiro <-  matrix(NA, nrow = ncol(training)-1, ncol = 2)
colnames(pval.shapiro) <- c("signal (g)", "background (h)")
rownames(pval.shapiro) <- variables
for(i in 1:(dim(training)[2]-1)) { tmp <- training[samp,]
                 pval.shapiro[i,1] <- shapiro.test(tmp[tmp[,"class"]=="g", i])$p.value
                 pval.shapiro[i,2] <- shapiro.test(tmp[tmp[,"class"]=="h", i])$p.value
}
round(pval.shapiro, 2)


# b) _____VALUTAZIONE DELL'OMOSCHEDASTICITA'______

pbox.all <- list()
variables

for (i in variables){
  pbox.all[[i]] <- ggplot(training, aes_string(x = "class", y = i, col = "class", fill = "class")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("blue", "orange")) 
  scale_fill_manual(values = c("blue", "orange"))
}
do.call(grid.arrange, c(pbox.all, nrow=2))
#le variabili non sembrano condividere una varianza in comune condizionata alle classi

library(heplots)
covEllipses(training[,1:(dim(training)[2]-1)], 
            factor(training$class), 
            fill = TRUE, 
            pooled = FALSE, 
            col = c("blue", "orange"), 
            variables = c(1:7, (dim(training)[2]-1)), 
            fill.alpha = 0.05)

#le variabili, ad eccezione di alcuni casi non sembrano avere varianza comune condizionate alle
#classi quindi si esclude l'ipotesi di varianza comune e di conseguenza il metodo di LDA


#________________________________________________________________________________________________
#                             STIMA DEI MODELLI NEL TRAINING SET
#_______________________________________________________________________________________________


# 1) ____REGRESSIONE LOGISTICA_____

training$class <- ifelse(training$class=="g", 0, 1)
mod0 <- glm(class~., family = "binomial", data=training)
summary(mod0)
#le variabili fM3Trans e fDist non sono significative e l'AIC è molto alto
#eseguo una selezione del modello basata sull'AIC

library(MASS)
step.mod0 <- stepAIC(mod0, direction="both")
#eliminando fM3Trans e fDist l'aic migliora di poco, analizzo gli outliers per vedere se incidono

anova(step.mod0, test="Chisq")
#le variabili sono tutte significative


influencePlot(step.mod0)
#provo a rimuovere queste osservazioni 
head(training)
r.eliminare <- c("3", "12518", "14070", "2645")
training3 <- training
for (i in 1:length(r.eliminare)) { r <- r.eliminare[i]
             training3 <- training3[rownames(training3)!=r, ]
}
dim(training3)


mod1 <- glm(class~., family = "binomial", data = training3)
summary(mod1)
#l'aic si è ridotto
step.mod1 <- stepAIC(mod1, direction = "both")
#l'aic è migliorato, il modello è migliore
anova(step.mod1, test="Chisq")
influencePlot(step.mod1)
#ci sarebbero altri outliers da togliere
summary(step.mod1)

# ~ verifica linearità ~
probabilities <- predict(step.mod1, type = "response")
predittori <- c("fLenWidSiz", "fConc", "fAsym", "fM3Long", "fAlpha")

summary(training)
library(dplyr)
library(tidyr)
supp <- training3[, c(1, 2, 3, 4, 6)]
supp <- supp %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(supp[,-8], aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#non sembra esserci linearità

#OSSERVAZIONI: il modello di regressione logistica presenta diversi problemi


# 2) ___ANALISI DISCRIMINANTE QUADRATICA_____

#La LDA non è applicabile perchè vengono violate entrambe le sue ipotesi di base: non c'è
#covarianza in comune condizionata alle classi e non sembra esserci normalità delle variabili

#Proviamo ad applicare la QDA forzando l'ipotesi di normalità e sapendo quindi che commetteremo
#un errore anche abbastanza significativo. Scelgo di usare le variabili che presentano una 
#distribuzione più simile a quella normale: fLenWidSiz, fConc, fDist

qda.mod <- qda(class~fLenWidSiz+fConc+fDist, data=training)
qda.mod


#______________________________________________________________________________________________
#                           VALUTAZIONE CAPACITA' - VALIDATION SET
#______________________________________________________________________________________________
validation <- validation.original
summary(validation[,-11])
str(validation)

#imputazione della mediana nella variabile Width
validation[validation$fWidth==0, "fWidth"] <- medianAll.trn["Median", "fWidth"]

#trasformazione logaritmica di fLength
validation$fLength <- log(validation$fLength)

#standardizzazione delle variabili (con media e sd del training)
for(i in 1:(dim(validation)[2]-1)) {
    validation[,i] <- (validation[,i]-indicators["Mean",i])/indicators["StDev",i]
}
summary(validation)

# _____ANALISI DELLE COMPONENTI PRINCIPALI_____

validation1 <- validation

# ~ unisco 1:3 variabili ~

pca <- princomp(x=validation[,1:3])
plot(pca, type="lines") 
summary(pca)
#mi fermo alla prima componente perchè è quella che massimizza la varianza
validation1[,1] <- validation1[,1]*pca$loadings[1,1]+validation1[,2]*pca$loadings[2,1]+
                   validation1[,3]*pca$loadings[3,1]
colnames(validation1)[1] <- "fLenWidSiz"


# ~ unisco 4a e 5a variabili ~

pca2 <- princomp(x=validation[,4:5])
plot(pca2, type="lines")
summary(pca2)
#mi fermo alla prima componente perchè massimizza la varianza
validation1[,4] <- validation1[,4]*pca$loadings[1,1] + validation1[,5]*pca$loadings[2,1]
validation1 <- validation1[,-c(2,3,5)]

summary(validation1)
validation <- validation1


validation$class <- ifelse(validation$class=="g", 0, 1)
library(caret)


# 1) _REGRESSIONE LOGISTICA_

pred.logit <- predict(step.mod1, validation[,-8], type = "response")
pred.logit.class <- ifelse(pred.logit > 0.5, 1, 0)
confusionMatrix(factor(validation[,8]), factor(as.vector(pred.logit.class)))

#ACCURACY -> è del 78.83% 
#SENSITIVITY -> è del 79.83%
#SPECIFICITY -> è del 76.18%



# 2) _ANALISI DISCRIMINANTE QUADRATICA_

pred.qda <- predict(qda.mod, validation[,-8], type="response")
confusionMatrix( factor(validation[,8]), pred.qda$class)

#ACCURACY -> è del 72.06%
#SENSITIVITY -> è del 71.59%
#SPECIFICITY -> 74.64%



# 3) _K-NEAREST NEIGHBOUR_

library(class)
K <- c(1:50, 175, 121, 195,235) 
accuracy.knn <- NULL

for(i in 1:length(K)){ k <- K[i]
     model <- knn(training[,-8], validation[,-8], cl=training$class, k=k)
     acc <- confusionMatrix(factor(validation[,8]), model )$overall["Accuracy"]
     accuracy.knn <- c(accuracy.knn, acc)
}
length(accuracy.knn)
plot(x=K, y=accuracy.knn, type = "l")
K[which.max(accuracy.knn)]                 #15

knn.mod <- knn(training[,-8], validation[,-8], cl=training$class, k=17, prob = T)
confusionMatrix(factor(validation[,8]), knn.mod)

#ACCURACY ->  81.81%
#SENSITIVITY -> 80.78%
#SPECIFICITY -> 84.94%



#______ROC CURVE E AUC_______
library(ROCR)

roc.pred.logit <- prediction(pred.logit, labels = validation[,8])
roc.perf.logit <- performance(roc.pred.logit, "tpr", "fpr")
auc.logit <- performance(roc.pred.logit, measure = "auc")@y.values
auc.logit
#0.8377

roc.pred.qda <- prediction(pred.qda$posterior[,2], validation[,8])
roc.perf.qda <- performance(roc.pred.qda, "tpr", "fpr")
auc.qda <- performance(roc.pred.qda, measure = "auc")@y.values
auc.qda
#0.6966

probs <- attributes(knn.mod)$prob
prob.knn <- 2*ifelse(knn.mod==0, 1-probs, probs)-1
roc.pred.knn <- prediction(prob.knn, validation[,8])
roc.perf.knn <- performance(roc.pred.knn, "tpr", "fpr")
auc.knn <- performance(roc.pred.knn, measure = "auc")@y.values
auc.knn
#0.8739

par(mfrow=c(1,3))
plot(roc.perf.logit, colorize=T, main="Regressione Logistica", lwd=2)
abline(v=0.2, lty="dashed")
plot(roc.perf.qda, colorize=T , main="QDA", lwd=2)
abline(v=0.2, lty="dashed")
plot(roc.perf.knn, colorize=T, main="KNN-17", lwd=2)
abline(v=0.2, lty="dashed")

#decidiamo di proseguire escludendo la QDA

par(mfrow=c(1,1))
#________________________________________________________________________________________________
#                                   TRAINING + VALIDATION 
#________________________________________________________________________________________________

trn.val <- trn.val.original
summary(trn.val)
any(is.na(trn.val))

#applico trasformazioni decise
trn.val$fLength <- log(trn.val$fLength)

#calcolo e salvo medie e mediane
medianAll <- matrix(NA, nrow = 1, ncol = dim(trn.val)[2]-1)
colnames(medianAll) <- colnames(trn.val)[-11]
rownames(medianAll) <- "Median"

meanAll <- matrix(NA, nrow = 1, ncol = dim(trn.val)[2]-1) 
colnames(meanAll) <- colnames(trn.val)[-11]
rownames(meanAll) <- "Mean"

for (i in 1:(dim(trn.val)[2]-1)){
               meanAll[,i] <- mean(trn.val[,i])
               medianAll[,i] <- median(trn.val[,i])
}
meanAll
medianAll

#imputazione della mediana 
trn.val[trn.val$fWidth==0, "fWidth"] <- medianAll["Median", "fWidth"]


#____STANDARDIZZAZIONE____
indicators2 <- matrix(NA, nrow = 2, ncol = dim(trn.val)[2]-1)
colnames(indicators2) <- colnames(trn.val)[-11]
rownames(indicators2) <- c("Mean", "StDev")

for(i in 1:(dim(trn.val)[2]-1)) {
         indicators2["Mean", i] <- mean(trn.val[,i])
         indicators2["StDev", i] <- sd(trn.val[,i])
}
indicators2

for (i in 1:(dim(trn.val)[2]-1)) {
        trn.val[,i] <- (trn.val[,i]-indicators2["Mean",i])/indicators2["StDev", i]
}
summary(trn.val)

#modifico codifica class
trn.val$class <- ifelse(trn.val$class=="g", 0, 1)

#_____ANALISI DELLE COMPONENTI PRINCIPALI__________

# ~ unisco 1:3 ~
pca.trn.val <- princomp(trn.val[,c(1:3)])
plot(pca.trn.val, type="lines")
summary(pca.trn.val)
#mi fermo alla prima componente
trn.val[,1] <- trn.val$fLength*pca.trn.val$loadings[1,1] + trn.val$fWidth*pca.trn.val$loadings[2,1]+ trn.val$fSize*pca.trn.val$loadings[3,1]
colnames(trn.val)[1] <- "fLenWidSiz"


#  ~ unisco 4:5 ~
pca2.trn.val <- princomp(trn.val[,4:5])
plot(pca2.trn.val, type="l")
summary(pca2.trn.val)
#mi fermo alla prima componente
trn.val[,4] <- trn.val$fConc*pca2.trn.val$loadings[1,1] + trn.val$fConc1*pca2.trn.val$loadings[2,1]

trn.val <- trn.val[,-c(2,3,5)]
summary(trn.val)



#_________REGRESSIONE LOGISTICA________
mod.logit <- glm(class~fLenWidSiz+fConc+fAsym+fM3Long+fAlpha, data=trn.val, family="binomial")
summary(mod.logit)

influencePlot(mod.logit)
#rimuovo queste osservazioni
r.el <- c("16396", "12518", "11693", "3") 
trn.val1 <- trn.val

for (i in 1:length(r.el)) { r <- r.el[i]
trn.val1 <- trn.val1[rownames(trn.val1)!=r, ]
}
dim(trn.val1)

mod.logit1 <- glm(class~fLenWidSiz+fConc+fAsym+fM3Long+fAlpha, data=trn.val1, family="binomial")
summary(mod.logit1)
#la variabile fAsym risulta meno significativa rispetto a nel training
#l'aic migliora

#il knn si analizza nella fase di test



#_________________________________________________________________________________________________
#                                       TEST DEI MODELLI
#_________________________________________________________________________________________________

summary(test)

#applico le trasformazioni
test$fLength <- log(test$fLength)

#imputazione della mediana a fWidth, mediana del training+validation
test[test$fWidth==0, "fWidth"] <- medianAll["Median","fWidth"]

#standardizzazione con media e sd del training+validation
for (i in 1:(dim(test)[2]-1)) {
        test[,i] <- (test[,i] - indicators2["Mean", i]) / indicators2["StDev", i]
}

#modifica codifica di class
test$class <- ifelse(test$class=="g", 0, 1)



#______ANALISI DELLE COMPONENTI PRINCIPALI_______

# ~ unisco 1:3 ~
pca.test <- princomp(test[,1:3])
plot(pca.test, type="l")
summary(pca.test)
#mi fermo alla prima componente
test[,1] <- test$fLength*pca.test$loadings[1,1] + test$fWidth*pca.test$loadings[2,1] + 
            test$fSize*pca.test$loadings[3,1]
colnames(test)[1] <- "fLenWidSiz"

# ~ unisco 4:5 ~
pca2.test <- princomp(test[,4:5])
plot(pca2.test, type="l")
summary(pca2.test)
#mi fermo alla prima componente
test[,4] <- test$fConc*pca2.test$loadings[1,1] + test$fConc1*pca2.test$loadings[2,1]

test <- test[,-c(2,3,5)]
summary(test)



# 1) ____________REGRESSIONE LOGISTICA______________

pred.logit.test <- predict(mod.logit1, test[, -8], type = "response")
pred.class.test <- ifelse(pred.logit.test>0.5, 1, 0)
confusionMatrix(factor(test[,8]), factor(as.vector(pred.class.test)))

#ACCURACY -> 76.34%
#SENSITIVITY -> 78.23%
#SPECIFICITY -> 71.46%
1-0.7146

# 2) ____________15 - KNN_______________

knn.mod.test <- knn(trn.val[,-8], test[,-8], cl=trn.val[,8], k=17, prob=T)
confusionMatrix(factor( test[,8]), knn.mod.test)

#ACCURACY -> 80.21%
#SENSITIVITY -> 79.80%
#SPECIFICITY -> 81.42%
1-0.8142



#proseguo considerando il KNN

#_______ROC CURVE E AUC_________

probs.test <- attributes(knn.mod.test)$prob
prob.knn.test <- 2*ifelse(knn.mod.test==0, 1-probs.test, probs.test)-1
roc.pred.knn.test <- prediction(prob.knn.test, test[,8])
roc.perf.knn.test <- performance(roc.pred.knn.test, "tpr", "fpr")
auc.knn.test <- performance(roc.pred.knn.test, measure = "auc") @y.values
auc.knn.test
#0.8553

plot(roc.perf.knn.test, colorize=T, lwd=2, main="KNN - Test")

acc.knn.trn <- confusionMatrix(factor(validation[,8]), knn.mod)$overall["Accuracy"]
trn.error <- 1-acc.knn.trn
spec.knn.trn <- confusionMatrix(factor(validation[,8]), knn.mod)$byClass["Specificity"]
trn.fpr <- 1-spec.knn.trn

acc.knn.test <- confusionMatrix(factor(test[,8]), knn.mod.test)$overall["Accuracy"]
test.error <- 1-acc.knn.test
spec.knn.test <- confusionMatrix(factor(test[,8]), knn.mod.test)$byClass["Specificity"]
test.fpr <- 1-spec.knn.test

round(cbind(trn.error, test.error), 4)
trn.error-test.error
round(cbind(trn.fpr, test.fpr), 4)
