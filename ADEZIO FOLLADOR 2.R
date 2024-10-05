#############################################################
# HOMEWORK 2 ADEZIO FOLLADOR
#############################################################

library(tidyverse)

# import dataset
marvelDC <- read.csv("C:\\Users\\giudi\\OneDrive\\Desktop\\uni\\statistica computazionale\\ADEZIO FOLLADOR 3.csv", fileEncoding = "latin1", sep = ',')

str(marvelDC)

# eliminiamo la prima colonna che contiene solo un numero progressivo da 1 a 39
marvelDC <- marvelDC[,-1] #; View(marvelDC)
str(marvelDC)

# trasformiamo il dataframe in tibble per comodità
(marvelDC <- as_tibble(marvelDC))

# abbiamo notato che alcuni valori dei minuti e del buget hanno degli spazi in più
help(str_trim)

marvelDC$Budget <- str_trim(marvelDC$Budget, side = 'right')    # togliamo gli spazi bianchi
marvelDC$Minutes <- str_trim(marvelDC$Minutes, side = 'right')

marvelDC$Budget <- as.numeric(as.character(marvelDC$Budget))    # trasformiamo in numeric
marvelDC$Minutes <- as.numeric(as.character(marvelDC$Minutes))  # le variabili minutes e budget

#View(marvelDC)
str(marvelDC)


#-------------------------------------
# ANALISI GENERALI DEL DATASET
#-------------------------------------

anyNA(marvelDC) # non ci sono valori mancanti nel dataset


#-------------------------
# boxplot
#-------------------------

b1 <- ggplot(marvelDC,  aes(y = Rate, x = Company, fill = Company)) + 
      geom_boxplot(color = 'black', outlier.shape = 18, outlier.size = 4, show.legend = F) + 
      scale_fill_manual(values = c('slateblue', 'red'))+ theme_get() 
  
b2 <- ggplot(marvelDC,  aes(y = Metascore, x = Company, fill = Company)) + 
      geom_boxplot(color = 'black', outlier.shape = 18, outlier.size = 4, show.legend = F) + 
      scale_fill_manual(values = c('slateblue', 'red'))+ theme_get() 

b3 <- ggplot(marvelDC,  aes(y = Minutes, x = Company, fill = Company)) + 
      geom_boxplot(color = 'black', outlier.shape = 18, outlier.size = 4, show.legend = F) + 
      scale_fill_manual(values = c('slateblue', 'red'))+ theme_get() 

b4 <- ggplot(marvelDC,  aes(y = Budget, x = Company, fill = Company)) + 
      geom_boxplot(color = 'black', outlier.shape = 18, outlier.size = 4, show.legend = F) + 
      scale_fill_manual(values = c('slateblue', 'red')) + theme_get() 

b5 <- ggplot(marvelDC,  aes(y = Opening.Weekend.USA, x = Company, fill = Company)) + 
      geom_boxplot(color = 'black', outlier.shape = 18, outlier.size = 4, show.legend = F) + 
      scale_fill_manual(values = c('slateblue', 'red'))+ theme_get() 

b6 <- ggplot(marvelDC,  aes(y = Gross.USA, x = Company, fill = Company)) + 
      geom_boxplot(color = 'black', outlier.shape = 18, outlier.size = 4, show.legend = F) + 
      scale_fill_manual(values = c('slateblue', 'red'))+ theme_get() 

b7 <- ggplot(marvelDC,  aes(y = Gross.Worldwide, x = Company, fill = Company)) + 
      geom_boxplot(color = 'black', outlier.shape = 18, outlier.size = 4, show.legend = F) + 
      scale_fill_manual(values = c('slateblue', 'red'))+ theme_get()

b8 <- ggplot(marvelDC,  aes(y = Release, x = Company, fill = Company)) + 
      geom_boxplot(color = 'black', outlier.shape = 18, outlier.size = 4, show.legend = F) + 
      scale_fill_manual(values = c('slateblue', 'red'))+ theme_get()

library(gridExtra)
#help("gridExtra")
grid.arrange(b1, b2, b3, b4, b5, b6, b7, b8, ncol = 4)

#-------------------------
# outliers
#-------------------------

library(car)
which(marvelDC$Budget %in% boxplot.stats(marvelDC$Budget)$out)       # 19 22 29 39
which(marvelDC$Gross.USA %in% boxplot.stats(marvelDC$Gross.Worldwide)$out) # no outliers
which(marvelDC$Metascore %in% boxplot.stats(marvelDC$Metascore)$out) # 24
which(marvelDC$Rate %in% boxplot.stats(marvelDC$Rate)$out)           # 24 29
which(marvelDC$Opening.Weekend.USA %in% boxplot.stats(marvelDC$Opening.Weekend.USA)$out) # 22
which(marvelDC$Minutes %in% boxplot.stats(marvelDC$Minutes)$out)     # 22 29
which(marvelDC$Gross.Worldwide %in% boxplot.stats(marvelDC$Gross.Worldwide)$out)         # 19 22
which(marvelDC$Release %in% boxplot.stats(marvelDC$Release)$out)     # no outliers

with(marvelDC, outlierTest(glm(Budget ~ Company)))              # no outliers
with(marvelDC, outlierTest(glm(Opening.Weekend.USA ~ Company))) # 22
with(marvelDC, outlierTest(glm(Minutes ~ Company)))             # 29
with(marvelDC, outlierTest(glm(Metascore ~ Company)))           # no outliers
with(marvelDC, outlierTest(glm(Rate ~ Company)))                # 24
with(marvelDC, outlierTest(glm(Gross.USA ~ Company)))           # no outliers
with(marvelDC, outlierTest(glm(Gross.Worldwide ~ Company)))     # 22
with(marvelDC, outlierTest(glm(Release ~ Company)))             # no outliers


marvelDC[22,] # Endgame budget molto alto, ha incassato nel mondo e 
              # nella settimana di apertura molto più degli altri
marvelDC[24,] # Catwoman, rate troppo basso rispetto agli altri
marvelDC[29,] # Jonah Hex, il film dura molto meno di tutti gli altri
marvelDC[19,] # Infinity War, budget e incassi nel mondo outlier
marvelDC[39,] # Joker, budget outlier


#------------------------
# analisi ulteriori
#------------------------
marvelDC %>%
    group_by(Company) %>%
    summarize(size = n())
# la Marvel ha 23 osservazioni mentre la DC ne ha 16

arrange(marvelDC, desc(Gross.Worldwide))[1:5,]     # 1. Avengers: Endgame
arrange(marvelDC, desc(Gross.USA))[1:5,]           # 1. Avengers: Endgame
arrange(marvelDC, desc(Opening.Weekend.USA))[1:5,] # 1. Avengers: Endgame
arrange(marvelDC, desc(Budget))[1:5,]              # 1. Avengers: Endgame
arrange(marvelDC, desc(Minutes))[1:5,]             # 1. Avengers: Endgame
arrange(marvelDC, Minutes)[1:5,]                   # 39. Jonah Hex
arrange(marvelDC, desc(Rate))[1:5,]                # 1. The Dark Knight
arrange(marvelDC, desc(Metascore))[1:5,]           # 1. Black Panther

due <- marvelDC %>% 
       select(Original.Title, Company, Budget) %>%
       mutate(Budget_in_Milioni = Budget / 1000000)  

arrange(due, desc(Budget_in_Milioni))
arrange(due, Budget_in_Milioni)         # 39. The Incredible Hulk


#-------------------------
# barplot
#-------------------------

# creiamo delle statistiche riassuntive calcolando per Company la media di incassi 
# nel mondo, nel primo weeekend e in USA e la media dei budget
marvelDC2 <- marvelDC %>%
             group_by(Company) %>%
             summarize(average_budget = mean(Budget), average_gross = mean(Gross.Worldwide),
                       average_grossUSA = mean(Gross.USA), average_openingweekend = mean(Opening.Weekend.USA))


(marvelDC2_long <- gather(marvelDC2, key = "STAT", value = "VALUE", -Company))

# barplot delle statistiche sopra specificate:
ggplot(marvelDC2_long, aes(x = Company, y = VALUE, fill = STAT)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = 'single'), color = 'black') +
  labs(title = "STATISTICHE DC VS MARVEL") +
  scale_fill_manual(values = c("average_budget" = "red", "average_gross" = "slateblue", 
                               "average_grossUSA" = "orange", "average_openingweekend" = "lightblue")) +
  theme_get()


#------------------------
# scatterplot
#------------------------

library(scatterplot3d)
par(mfrow = c(1,1))

(marvelDC3 <- marvelDC %>% 
             mutate(Budget_in_milioni = Budget / 1000000,
                    Gross.Worldwide_in_milioni = Gross.Worldwide / 1000000))
marvelDC3$pcolor[marvelDC$Company == 'Marvel'] <- "red"
marvelDC3$pcolor[marvelDC$Company == 'DC'] <- "slateblue"


with(marvelDC3, {
  s3d <- scatterplot3d(x = Budget_in_milioni, y = Gross.Worldwide_in_milioni, z = Rate, main="SCATTERPLOT 3D",
                       pch = 19, type = 'h', color = pcolor,
                       xlab = "Budget in milioni", ylab = "Gross Worldwide in milioni", zlab="Rate")
  s3d.coords <- s3d$xyz.convert(Budget_in_milioni, Gross.Worldwide_in_milioni, Rate)
                text(s3d.coords$x, s3d.coords$y,
                labels = marvelDC3$Original.Title, cex = .6, pos = 4)
   
  legend("topleft", legend = c( 'Marvel', 'DC'), 
         bty = "n", fill = c("red","slateblue"))
})

# dal grafico si può ben notare ciò che abbiamo detto in precedenza sulle unità statistiche
# Endgame, Infinity War, The Dark Knight, Jonah Hex e Catwoman

#di seguito versione interattiva dello scatterplot3d sopra realizzato
library(plotly)

scatterplot <- plot_ly(mode = 'markers', x = marvelDC3$Budget_in_milioni, 
                       y = marvelDC3$Gross.Worldwide_in_milioni, z = marvelDC3$Rate,
                       text = marvelDC3$Original.Title,
                       marker = list(color = marvelDC3$pcolor, size = 6)) %>% 
                       layout(scene = list(
                         xaxis = list(title = "Budget in milioni"),
                         yaxis = list(title = "Gross Worldwide in milioni"),
                         zaxis = list(title = "Rate")
                       ), hovermode = 'closest', xaxis = list(title = "Budget in milioni"), 
                       yaxis = list(title = "Gross Worldwide in milioni"), 
                       zaxis = list(title = "Rate"))


scatterplot
  
#------------------------
# correlation
#------------------------
library(ggcorrplot)
cor <- round(cor(marvelDC[-c(1,2)]), 3) # nel calcolo della correlazione abbiamo 
                                        # tolto le variabili non numeriche (titolo e Company)

ggcorrplot(cor, type = 'lower', lab = T, lab_size = 3,
           title = 'GRAFICO CORRELAZIONE', outline.color = 'black', 
           colors =  c('blue', 'yellow','red'), hc.order = T,
           legend.title = 'val corr', ggtheme = ggplot2::theme_get)

library(GGally)
ggpairs(marvelDC[- c(1,2)], aes(color = as.factor(marvelDC$Company))) +
   scale_color_manual(values = c('slateblue', 'red')) +
   scale_fill_manual(values = c('slateblue', 'red'))

# da una prima analisi possiamo vedere che le variabili che in assoluto risultano
# più correlate sono opening weekend USA e gross USA che è anche ugualmente
# correlata con gross worldwide (correlata naturalmente con opening weekend USA)
# altre variabili abbastanza correlate sono rate a metascore come potevasi immaginare
# Le meno correlate risultano essere minutes e release.
# tutte le variabili sono correlate positivamente anche se risultano esserci alcune 
# variabili quasi incorrelate.



#------------------------------------
# ANALISI COMPONENTI PRINCIPALI
#-------------------------------------

pca <- princomp(marvelDC[-c(1,2)], cor = T)
summary(pca)
sum((pca$sdev[1:3])^2)/8
pca$loadings[,1:3]
biplot(pca)

# criteri di arresto componenti principali
cumsum(summary(pca)$sdev^2/sum(summary(pca)$sdev^2))>=0.8
(c<-mean(pca$sdev^2))
pca$sdev^2>c
plot(pca, type = 'l') # screeplot

names(marvelDC[-c(1,2)])[apply(pca$loadings[,1:3], 2, function(x) which(x**2==max(x**2)))]
# "Gross.USA" "Metascore" "Release" 

# prime tre variabili componenti principali 
marvelDC_CP <- marvelDC[c(4,6,9)]
ggpairs(marvelDC_CP, mapping = aes(color = as.factor(marvelDC$Company))) +
  scale_color_manual(values = c('slateblue', 'red')) +
  scale_fill_manual(values = c('slateblue', 'red')) # Gross.USA e Metascore sono le più significativamente correlate

pairs(marvelDC_CP, col = as.factor(marvelDC$Company)) 

# scatterplot relazione Metascore e Gross.USA
# da queste due variabili sembra almeno a primo impatto che si riesca un po' a vedere 
# la divisione tra i cluster quindi spostiamo il focus sul loro scatterplot
ggplot(data = marvelDC, mapping = aes(x = Gross.USA, y = Metascore, color = Company)) + 
   geom_point(alpha = .9, size = 4, shape = 18) + 
   scale_color_manual(values = c('slateblue', 'red')) +
   labs(title = 'SCATTERPLOT METASCORE INCASSI') +
   theme_get()


#------------------------------------
# model based clustering
#-------------------------------------

library(mclust)

marvelDC_clust <- Mclust(marvelDC)
summary(marvelDC_clust)

#---------------------------------------------------- 
#  Gaussian finite mixture model fitted by EM algorithm 
#---------------------------------------------------- 
#  
#  Mclust VEV (ellipsoidal, equal shape) model with 2 components: 
#
#  log-likelihood  n  df       BIC       ICL
#       -3518.724 39 122 -7484.402 -7484.416
#
#  Clustering table:
#    1  2 
#   21 18 


# confronto

marvelDC_clustICL <- mclustICL(marvelDC)
summary(marvelDC_clustICL)
# Best ICL values:
#              VEV,2       EEE,1       EEV,1
# ICL      -7484.416 -7496.19806 -7496.19806
# ICL diff     0.000   -11.78183   -11.78183


marvelDC_clust$BIC
# Top 3 models based on the BIC criterion: 
#   VEV,2     EEE,1     EEV,1 
# -7484.402 -7496.198 -7496.198


# confronto grafico
par(mfrow = c(1,2))
plot(marvelDC_clust, what = "BIC", legend = list(x = 'topright'))
plot(marvelDC_clustICL, legend = list(x = 'topright'))

marvelDC_clust$classification

# conosco le vere etichette quindi procedo a confrontare i labels
(labels_cluster <- marvelDC_clust$classification)    #labels clustering
(labels_true <- marvelDC$Company)                    #true labels

classError(labels_cluster, class = labels_true)
#$misclassified
#[1] 19 22
#
#$errorRate
#[1] 0.05128205 (CER)

# CER
2/39 #0.05128205

# ACCURACY
1 - 0.05128205 #complemento a 1 del CER
#0.948718

# ARI
adjustedRandIndex(labels_true, labels_cluster)
#0.8002972


# confusion matrix
library("caret")

(true <- as.factor(labels_true))
(clust <- as.factor(labels_cluster))
levels(clust)<-c("Marvel","DC")

confusionMatrix(clust, true)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction DC Marvel
# DC         16      2
# Marvel     0     21
# 
# Accuracy : 0.9487          
# 95% CI : (0.8268, 0.9937)
# No Information Rate : 0.5897          
# P-Value [Acc > NIR] : 4.398e-07       
# 
# Kappa : 0.896           
# 
# Mcnemar's Test P-Value : 0.4795          
#                                           
#             Sensitivity : 1.0000          
#             Specificity : 0.9130          
#          Pos Pred Value : 0.8889          
#          Neg Pred Value : 1.0000          
#              Prevalence : 0.4103          
#          Detection Rate : 0.4103          
#    Detection Prevalence : 0.4615          
#       Balanced Accuracy : 0.9565          
#                                           
#        'Positive' Class : DC 
# 
(16 + 21)/39 # accuracy come traccia fratto n


marvelDC_df <- as.data.frame(marvelDC)
str(marvelDC_df)    #dataframe

plot(marvelDC_clust, what = 'classification')

par(mfrow = c(1,2))
coordProj (marvelDC_df, dimens = c(4,9), what = "classification",
           classification = labels_true,
           col = c("slateblue","red"), symbols = c(18, 18), cex = 1.5,
           sub = "1. TRUE CLASSIFICATION")

coordProj (marvelDC_df, dimens = c(4, 9), what = "classification",
           classification = labels_cluster,
           col = c("red","slateblue"), symbols = c(18, 18), cex = 1.5,
           sub = "2. MODEL BASED CLUSTERING") 

(misclassified <- classError(labels_cluster, class = labels_true)$misclassified)
points(marvelDC[misclassified, c(4, 9)], pch = 18, cex = 1.5)

# unità misclassificate
marvelDC[19,] # Avengers: Infinity War
marvelDC[22,] # Avengers: Endgame

# grafico dell'incertezza
par(mfrow = c(1,1))
coordProj (marvelDC_df, dimens = c(4, 9), what = "uncertainty",
           parameters = marvelDC_clust$parameters , z = marvelDC_clust$z) 
# solo a un'unità è associata un'incertezza moto alta le altre sembrano 
# classificate con certezza
uncerPlot(z = marvelDC_clust$z, truth = labels_true)




#--------------------------------------------------------------
# model based solo con variabili prese dall'analisi delle CP
#-------------------------------------------------------------

marvelDC_clustCP <- Mclust(marvelDC_CP, G = 2)
summary(marvelDC_clustCP)

#---------------------------------------------------- 
#  Gaussian finite mixture model fitted by EM algorithm 
#---------------------------------------------------- 
#  
#  Mclust VEV (ellipsoidal, equal shape) model with 2 components: 
#  
#  log-likelihood  n df       BIC       ICL
#       -1040.631 39 17 -2143.543 -2147.554
#  
#  Clustering table:
#    1  2 
#    25 14 


# confronto

summary(mclustICL(marvelDC_CP, G = 2))
# Best ICL values:
#                VEV,2        EVE,2        VEE,2
#   ICL      -2147.554 -2149.080133 -2150.332434
#   ICL diff     0.000    -1.526483    -2.778784


marvelDC_clustCP$BIC
# Top 3 models based on the BIC criterion: 
#   VEV,2     VEE,2     EVE,2 
# -2143.543 -2144.492 -2145.174 

# confronto grafico
par(mfrow = c(1,2))
plot(marvelDC_clustCP, what = "BIC")
plot(mclustICL(marvelDC_CP, G = 2))

marvelDC_clustCP$classification

# conosco le vere etichette quindi procedo a confrontare i labels
(labels_clusterCP <- marvelDC_clustCP$classification)        #labels clustering

classError(labels_clusterCP, class = labels_true)
#$misclassified
#[1]  1  2  3  4  5  6  7  8 11 18 19 22 35 38 39
#
#$errorRate
#[1] 0.3846154


# ACCURACY
1 - 0.3846154 
#0.6153846

# ARI
adjustedRandIndex(labels_true, labels_clusterCP)
#0.02797718


# confusion matrix
(clustCP <- as.factor(labels_clusterCP))
levels(clustCP) <- c("DC","Marvel")

confusionMatrix(clustCP, true)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction DC Marvel
# DC         13     12
# Marvel      3     11


# Nonostante il modello stimato (imponendo però il numero di componenti) sia uguale
# a quello stimato con tutte le variabili in realtà l'adattamento ai dati risulta
# essere peggiore infatti il CER è alto e l'ARI è basso rispetto alle quantità trovate prima
# Quindi procediamo a eseguire la classificazione con tutte le variabili.



#---------------------------------
# classification with EDDA
#---------------------------------

library(Rmixmod)
set.seed(123)

# PRIMO STEP
# known labels
class <- unlist(as.factor(marvelDC$Company))

res <- mixmodLearn(marvelDC[-c(1,2)], class, 
                   models = mixmodGaussianModel(family = "all", equal.proportions = FALSE),
                   criterion = c('CV','BIC'))               # non possiamo fare un campionamento retrospettivo

# risultati classificazione 
str(res)
summary(res)

res@bestResult

res@models

# primi tre modelli
res@results[[1]]@model   #Gaussian_pk_Lk_C
# il modello scelto dalla classificazione ha free proportion free volume, equal
# shape e equal orientation (ellissoidale, VEE)
res@results[[2]]@model   #Gaussian_pk_Lk_D_Ak_D (VVE)
res@results[[3]]@model   #Gaussian_pk_L_C (EEE, LDA)

# CV valori primi tre modelli
res@results[[1]]@criterionValue [1]   #0.1025641    
res@results[[2]]@criterionValue [1]   #0.1282051   
res@results[[3]]@criterionValue [1]   #0.1794872

# BIC primi tre modelli
res@results[[1]]@criterionValue [2]   #7126.291
res@results[[2]]@criterionValue [2]   #7161.175
res@results[[3]]@criterionValue [2]   #7158.957


# tiriamo fuori i valori del BIC e del CV per tutti i modelli 
BIC <- CV <- mod <- rep(NA ,length(res@models@listModels) )
for (i in 1: length(res@models@listModels)){
  ind = which(res@results [[i]] @model == res@models@listModels)
  CV[ind] = res@results [[i]] @criterionValue [1]
  BIC[ind] = res@results [[i]] @criterionValue [2]
  mod[ind] = res@results [[i]] @model
}

# troviamo il minimo per BIC e CV
round(BIC,1)
min(BIC)       
which.min(BIC)

round(CV,3)
min(CV)
which.min(CV)


# creiamo due tibble con i BIC e i CV
dataBIC <- tibble(mod = mod, BIC = as.numeric(BIC))
dataCV <- tibble(mod = mod, CV = as.numeric(CV))

# ordiniamo il tibble per valori decrescenti di BIC e CV
dataBIC <- arrange(dataBIC, desc(BIC))
dataCV <- arrange(dataCV, desc(CV))

# grafico di confronto BIC CV 
minBIC <- dataBIC$mod[which.min(dataBIC$BIC)]
(p1 <- ggplot(dataBIC, aes(x = mod, y = BIC, group = 1)) +
        geom_line(color = 'slateblue') +
        geom_point(color = 'slateblue', size = 3) +
        geom_vline(xintercept = minBIC, linetype = 'dashed') +
        theme(axis.text.x = element_text(angle = 35, hjust=1))+
        labs(x = ''))

minCV <- dataCV$mod[which.min(dataCV$CV)]
(p2 <- ggplot(dataCV, aes(x = mod, y = CV, group = 1)) +
      geom_line(stat = 'identity', color = 'red') +
      geom_point(color = 'red', size = 3) +
      geom_vline(xintercept = minCV, linetype = 'dashed') +
      theme(axis.text.x = element_text(angle = 35, hjust=1))+
      labs(x = ''))

grid.arrange(p1, p2, ncol = 1)
# dal grafico del confronto dei criteri notiamo che la minimizzazione di entrambi 
# criteri si ha per il modello 8
mod[8] #"Gaussian_pk_Lk_C"


# SECONDO STEP
(n <- nrow(marvelDC))
test_set <- sample(1:n, 7) # campionamento test set

marvelDCnum <- marvelDC[-c(1,2)]
                             # allenamento al netto del test set
classificatore <- mixmodLearn(marvelDCnum[-test_set,], class[-test_set], 
                       models = mixmodGaussianModel(family="all",equal.proportions=FALSE))
# farà tramite CV perché non ho specificato nulla

classificatore@bestResult 
# nbCluster   =  2 
# model name  =  Gaussian_pk_Lk_C 
# criterion   =  CV(0.1250)
# likelihood  =  -2850.3066 

#prediction
prediction <- mixmodPredict(data = marvelDCnum[test_set,], classificationRule = classificatore["bestResult"])

str(prediction)

prediction@partition
prediction@proba

prediction

#  *** PREDICTION:
#  ****************************************
#  * partition     =  2 2 2 1 1 2 1 
#* probabilities = |   0.0008   0.9992 |
#     0.0033   0.9967 |
#     0.0018   0.9982 |
#     0.9851   0.0149 |
#     1.0000   0.0000 |
#     0.0053   0.9947 |
#     1.0000   0.0000 |
#  ****************************************

# compare prediction with real results
class[test_set]
# [1] Marvel Marvel Marvel DC     DC     Marvel DC    
# Levels: DC Marvel
prediction@partition
#[1] 2 2 2 1 1 2 1
mean(as.integer(class[test_set]) == prediction@partition) 
# complemento a 1 del MER
# [1] 1
# la prediction ha azzeccato tutte le us 
# il test è andato a buon fine


#il test è stato eseguito una seconda volta
# ****************************************
#   *** PREDICTION:
#   ****************************************
#   * partition     =  1 1 2 1 1 1 1 
# * probabilities = |   0.9009   0.0991 |
#      0.7722   0.2278 |
#      0.0002   0.9998 |
#      0.5033   0.4967 |
#      1.0000   0.0000 |
#      0.9928   0.0072 |
#      1.0000   0.0000 |
#   ****************************************

# complemento a 1 del MER = 0.7142857
# ha sbagliato due unità statistiche
which(as.integer(class[test_set]) != prediction@partition) #2 4

# modello scelto con allenamento e test applicato a tutti i dati
def <- mixmodPredict(data = marvelDCnum, classificationRule = classificatore["bestResult"])
# ****************************************
#   * nbCluster   =  2 
# * model name  =  Gaussian_pk_Lk_C 
# * criterion   =  CV(0.1250)
# * likelihood  =  -2828.2413 
# ****************************************
classError(def@partition, class) # CER = 0.05128205

mis <- which(as.integer(class) != def@partition) # salviamo i misclassificati

# creiamo una nuova variabile su un dataset temporaneo per colorare i punti misclassificati
marvelDCcol <- marvelDC %>%
  mutate(colorc = ifelse(Original.Title %in% marvelDC[mis,]$Original.Title, "misclassified", Company))

classgraph1 <- ggplot(marvelDCcol, aes(x = Gross.USA, y = Metascore, color = Company)) +
               geom_point(size = 4, shape = 18, show.legend = F) +
               scale_color_manual(values = c('slateblue', 'red')) +
               labs(subtitle = 'GRAFICO CON VERA CLASSIFICAZIONE') 

ggplot(marvelDCcol, aes(x = Gross.USA, y = Metascore, color = as.factor(def@partition)))+
  geom_point(size = 4, shape = 18, show.legend = F) +
  scale_color_manual(values = c('slateblue', 'red', 'black'))

classgraph2 <- ggplot(marvelDCcol, aes(x = Gross.USA, y = Metascore, color = colorc))+
               geom_point(size = 4, shape = 18, show.legend = F) +
               scale_color_manual(values = c('slateblue', 'red', 'black')) +
               labs(subtitle = 'GRAFICO CON MISCLASSIFICATI') 

grid.arrange(classgraph1, classgraph2, ncol = 2)




#---------------------------------
# classification with MDA
#---------------------------------
# al netto del test set allenamento del classificatore
mod <- MclustDA(marvelDCnum[-test_set ,], class[- test_set])
summary(mod)
str(mod)    

# ------------------------------------------------ 
#   Gaussian finite mixture model for classification 
# ------------------------------------------------ 
#   
#   MclustDA model summary: 
#   
#   log-likelihood  n  df       BIC
#        -2659.264 32 164 -5886.908
# 
# Classes   n     % Model G
#   DC     14 43.75   VEV 2
#   Marvel 18 56.25   VEV 2

# il modello è lo stesso della clusterizzazione 


predict(mod)  #computed on the training set
predict(mod, marvelDCnum[test_set ,])$class
# ora utilizza il classificatore che abbiamo scelto e applica il predict al test set
sum(predict(mod, marvelDCnum[test_set ,])$class != class[test_set])
# con questo modello sbaglia la classificazione di due unità statistiche 

mean(predict(mod, marvelDCnum[test_set ,])$class != class[test_set])
# MER 0.2857143

# per il momento abbiamo scelto per BIC


# scelta di G per cross-validation (VFOLD)
G <- 5; V <- 3; perm <- sample(n)
B <- round(n/V); err <- matrix(NA, G, V)
# b numerosità sottogruppi arrotondata chiaramente

for (g in 1:G){
  for (v in 1:V){
    validation.set.labels = perm[(B*(v-1)+1):(B*v)]                                       # imponiamo il modello trovato                      
    mod = MclustDA(marvelDCnum[- validation.set.labels ,], class[-validation.set.labels],G = g, modelNames = 'VEV')
    err[g,v] = sum(predict(mod, marvelDCnum[validation.set.labels,])$class != class[validation.set.labels]) / B
  }
}

err
round(rowMeans(err),4) # ogni riga indica un g diverso

# [,1]      [,2]      [,3]
# [1,] 0.2307692 0.3076923 0.6153846
# [2,] 0.3076923 0.1538462 0.2307692
# [3,] 0.6153846 0.1538462 0.3846154
# [4,] 0.5384615 0.1538462 0.3076923
# [5,] 0.3846154 0.1538462 0.4615385

err_data <- tibble(G = 1:G, error_means = rowMeans(err))
# [1] 0.3846 0.2308 0.3846 0.3333 0.3333

ggplot(err_data, aes(x = G, y = error_means)) +
  geom_line(col = 'slateblue', lwd = 1.5) + 
  geom_point(shape = 18, col = 'red', size = 3) + 
  geom_text(aes(label = round(error_means, 3)), vjust = - 1, hjust =  0.5, col = 'red') +
  labs(x = 'GRUPPI', y = 'CLASSIFICATION ERROR', title = "SCELTA DI G CON V-FOLD CV") + 
  ylim(0, 0.5) +  theme_get()  
# rieseguendo i risultati potrebbero cambiare, ma dai dati sopra riportati 
# confermiamo che imponendo il modello trovato (VEV) anche la cross validation
# con VFold sceglie di dividere il dataset in due cluster




#-------------------------------------
# Finite mixture of regression model
#-------------------------------------

# mixture of expert models
library(flexmix)
set.seed(49)

fit <- flexmix(Gross.USA ~ ., data = marvelDCnum, k = 2,
               concomitant = FLXPmultinom(~ .))

# explore the expert
fit
parameters(fit) 
# Comp.1        Comp.2
# coef.(Intercept)          1.026088e+10  5.156097e+09
# coef.Rate                -4.351112e+07  1.503369e+07
# coef.Metascore            4.188510e+06 -1.207543e+05
# coef.Minutes              4.296309e+05  5.244698e+05
# coef.Release             -5.062393e+06 -2.621524e+06
# coef.Budget              -5.174591e-01  1.006072e-01
# coef.Opening.Weekend.USA  1.540780e+00  4.200538e-01
# coef.Gross.Worldwide      2.318848e-01  2.187083e-01
# sigma                     2.938061e+07  1.070442e+07

ICL(fit)       #1496.424
KLdiv(fit) 
# [,1]     [,2]
# [1,]   0.0000 1240.365
# [2,] 175.4285    0.000

ref <- refit(fit) # il refit viene una matrice con tutti NA
summary(ref)
# plot(ref)

summary(fit)

# Call:
#   flexmix(formula = Gross.USA ~ ., data = marvelDCnum, k = 2, concomitant = FLXPmultinom(~.))
# 
#         prior size post>0 ratio
# Comp.1  0.41   16     16 1.000   i ratio sono molto buoni
# Comp.2  0.59   23     24 0.958
# 
# 'log Lik.' -698.7534 (df=27)
# AIC: 1451.507   BIC: 1496.423 

posterior(fit)                   # posteriors
apply(posterior(fit), 2, sum)/39 # priors

str(fit)
fit@cluster

parameters(fit, which = "concomitant") # la prima è baseline tutti 0

plot(fit) # rootogram, i cluster sembrano ben separati

# rieseguiamo più volte flexmix con stepflexmix
(final <- stepFlexmix(Gross.USA ~ ., data = marvelDCnum, k = 1:5, 
                       concomitant = FLXPmultinom(~ .),
                        nrep = 8, verbose = TRUE, drop = F, unique = FALSE))

# Call:
#   stepFlexmix(Gross.USA ~ ., data = marvelDCnum, concomitant = FLXPmultinom(~.), 
#               k = 1:5, nrep = 8, verbose = TRUE, drop = F, unique = FALSE)
# 
# iter converged k k0    logLik      AIC      BIC      ICL
# 1    2      TRUE 1  1 -740.5711 1499.142 1514.114 1514.114
# 2   13      TRUE 2  2 -693.2870 1440.574 1485.490 1485.490
# 3   86      TRUE 3  3 -664.2352 1418.470 1493.331 1493.331
# 4  177      TRUE 3  4 -664.6674 1419.335 1494.195 1494.205
# 5   26      TRUE 2  5 -701.8765 1457.753 1502.669 1502.669


# grafico di cofronto tra ICL e BIC
(icl <- ICL(final))
#       1        2        3        4        5
#1514.114 1485.490 1493.331 1494.205 1502.669  
tibble1 <- tibble(k = 1:length(icl), ICL = icl)

g1 <- ggplot(tibble1, aes(x = k, y = ICL)) +
      geom_line(col = 'slateblue', linewidth = 1.5) +
      geom_point(shape = 18, size = 3, col = 'red') +
      labs(ylab = 'ICL') +
      theme_get()

(bic <- BIC(final))
#       1        2        3        4        5
#1514.114 1485.490 1493.331 1494.195 1502.669 
tibble2 <- tibble(k = 1:length(bic), BIC = bic)

g2 <- ggplot(tibble2, aes(x = k, y = BIC)) +
      geom_line(col = 'slateblue', linewidth = 1.5) +
      geom_point(shape = 18, size = 3, col = 'red') +
      labs(ylab = 'BIC') +
      theme_get()

grid.arrange(g1, g2, ncol = 1)


# salvo i labels della MEM ottenuti con lo stepflexmix
labs <- final@models$`2`@cluster
classError(labs, class = as.factor(marvelDC$Company))
# $misclassified
# [1]   1  3  6 10 15 18 19 21 23 24 26 28 29 30 33 36 38
# 
# $errorRate
# [1] 0.4358974 CER molto alto


c1 <- ggplot(data = marvelDC, mapping = aes(x = Gross.USA, y = Metascore, color = factor(labs)))+
      geom_point(size = 3, shape = 18) + 
      geom_smooth(method = "lm", se = F, size = 1.5)+
      scale_colour_manual(name = 'MEM', values = c( "slateblue", "red"))
  
c2 <- ggplot(data = marvelDC, mapping = aes(x = Gross.USA, y = Metascore, color = factor(Company)))+
      geom_point(size = 3, shape = 18) + 
      geom_smooth(method = "lm", se = F, size = 1.5)+
      scale_colour_manual(name = 'company', values = c( "slateblue", "red"))

grid.arrange(c1, c2, nrow = 1)
# anche dal grafico vediamo che il modello non si adatta perfettamente ai dati come
# invece quello ottenuto con la clusterizzazione e la classificazione
# soprattutto confontandolo con le vere etichette