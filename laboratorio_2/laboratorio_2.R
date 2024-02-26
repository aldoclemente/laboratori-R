#'###########################################################'#
#'########               LABORATORIO 2               ########'#
#'########            STATISTICA DESCRITTIVA         ########'#
#'###########################################################'#

rm( list = ls() )
graphics.off()

# Argomenti del secondo laboratorio:
# 1 - Analisi descrittiva di una variabile qunatitativa (confronto fra gruppi)
# 2 - Analisi descrittiva di più variabili quantitative (multivariata)
# 3 - Esempio di confronto tra più gruppi
# 4 - Esempi più complessi

# IMPOSTARE LA WORKING DIRECTORY (CARTELLA DI LAVORO): -------------------------
# Da interfaccia:
# 1.
# 'Session' -> 'Set Working Directory' -> 'Choose Directory' -> ...

# 2. 
# 'Session' -> 'Set Working Directory' -> 'To Source File Location'

# Da console:
# setwd( 'C:/percorso/file' )

# Da pacchetto:
if(!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1 - ANALISI DI UNA VARIABILE QUANTITATIVA (DATI NUMERICI) -------------------- 
# Analisi di variabili quantitative:
# 1. Indici statistici di posizione/dispersione
# 2. Confronto tra istogrammi
# 3. Confronto tra box plot

### ESERCIZIO 1 (Confronto tra due gruppi)
rm ( list = ls( ) )
graphics.off()

pazienti = read.table( 'pazienti.txt', header = TRUE )

### RICHIESTA 1: calcolare i principali indici statistici di posizione e di
### dispersione della variabile "Peso" separatamente per ogni sottocampione
### (maschi e femmine)

# -> tapply( x, y, f )
# x: variabile numerica
# y: variabile categorica
# f: funzione

# Media campionaria
tapply( pazienti$Peso, pazienti$Sesso, mean )  

# Varianza campionaria
tapply( pazienti$Peso, pazienti$Sesso, var )   

# Deviazione standard campionaria
tapply( pazienti$Peso, pazienti$Sesso, sd )

# Minimo
tapply( pazienti$Peso, pazienti$Sesso, min )   

# Massimo
tapply( pazienti$Peso, pazienti$Sesso, max )  

# Range
diff( tapply( pazienti$Peso, pazienti$Sesso, range )$F ) 
diff( tapply( pazienti$Peso, pazienti$Sesso, range )$M )

# Mediana
tapply( pazienti$Peso, pazienti$Sesso, median )

# Quartili
Q = tapply( pazienti$Peso, pazienti$Sesso, quantile )
Q

# Primo quantile: Q1
Q1 = c( Q$F[2], Q$M[2] )
Q1

# Terzo quantile: Q3
Q3 = c( Q$F[4], Q$M[4] )
Q3

# IQR
Q3 - Q1

# Quartile di ordine p = 0.9
tapply( pazienti$Peso, pazienti$Sesso, quantile, probs = 0.9 )

# Sintesi
tapply( pazienti$Peso, pazienti$Sesso, summary )

### RICHIESTA 2: visualizzare graficamente la distribuzione della variabile
### "Peso" separatamente per ogni sottocampione (maschi e femmine)

femmine = pazienti[ pazienti$Sesso == 'F', ]
maschi = pazienti[ pazienti$Sesso == 'M', ]

# Istogrammi affiancati
dev.new()
par ( mfrow = c( 2, 1 ) )
hist( femmine$Peso, prob = TRUE, col = 'orange',
      breaks = seq( min( pazienti$Peso ), max( pazienti$Peso ), length = 10 ), 
      xlim = range( pazienti$Peso ), ylim = c( 0, 0.06 ),
      main = 'Istogramma: PESO FEMMINE', xlab = 'Peso [kg]', ylab = 'Densità' )
hist( maschi$Peso, prob = TRUE, col = 'forestgreen',
      breaks = seq( min( pazienti$Peso ), max( pazienti$Peso ), length = 10 ),
      xlim = range( pazienti$Peso ), ylim = c( 0, 0.06 ),
      main = 'Istogramma: PESO MASCHI', xlab = 'Peso [kg]', ylab = 'Densità' )

# Nota: il confronto sensato se i grafici hanno le stesse scale sugli assi e le
# stesse classi!

# Box plot distinti per classi
dev.new()
boxplot( pazienti$Peso ~ pazienti$Sesso, col = c ( 'orange', 'forestgreen' ),
         xlab = 'Sesso', ylab = 'Peso [kg]', names = c( 'Femmine', 'Maschi' ),
         main = 'Box plot: PESO')

# Nota: confrontare sempre box plot con stesse scale!

# 2 - ANALISI MULTIVARIATA -----------------------------------------------------
# 1. Covarianza
# 2. Indice di correlazione lineare
# 3. Matrice di varianza e covarianza
# 4. Matrice di correlazione
# 5. Scatterplot
# 6. Box plot

# ESERCIZIO 2 
record = read.table( 'record.txt', header = TRUE )

head( record )

dim( record )

str( record )

### RICHIESTA: analizzare le distribuzioni delle variabili m100 e m1000
# Media campionaria e varianza campionaria
media_m100 = mean( record$m100 )

sd_m100 = sd( record$m100 )

media_m1000 = mean( record$m1000 )

sd_m1000 = sd( record$m1000 )

# Istogramma
dev.new()
par( mfrow = c( 1, 2 ) )
hist( record$m100, prob = TRUE, main = 'Istogramma:\nm100', col = 'forestgreen',
      breaks = 12, xlab = 'm100', ylab = 'Densità' )
hist( record$m1000, prob = TRUE, main = 'Istogramma:\nm1000', col = 'royalblue',
      breaks = 12, xlab = 'm1000', ylab = 'Densità' )

# Istogramma e distribuzione normale
# Griglia per il campionamento dalla normale
griglia_m100 = seq( min( record$m100 ), max( record$m100 ), length = 100 )

griglia_m1000 = seq( min( record$m1000 ), max( record$m1000 ), length = 100 )

dev.new()
par( mfrow = c( 1, 2 ) )
hist( record$m100, prob = TRUE, main = 'Istogramma:\nm100', col = 'forestgreen',
      breaks = 12, xlab = 'm100', ylab = 'Densità', xlim = c( 10, 13 ) )
lines( griglia_m100, dnorm( griglia_m100, media_m100, sd_m100 ),
       col = 'black', lwd = 2 )
hist( record$m1000, prob = TRUE, main = 'Istogramma:\nm1000', col = 'royalblue',
      breaks = 12, xlab = 'm1000', ylab = 'Densità', xlim = c( 0, 700 ) )
lines( griglia_m1000, dnorm( griglia_m1000, media_m1000, sd_m1000 ),
       col = 'black', lwd = 2 )

### ESERCIZIO 3 

### RICHIESTA: analizzare le dipendenze tra le variabili m100 e m200
dev.new()
plot( record$m100, record$m200, asp=1,  col = "black", pch = 16,
      main = 'm100 vs. m200', xlab = 'm100', ylab = 'm200' )

# Covarianza (Cov): valutare come due variabili si discostano dalle loro medie
#   - se Cov(X, Y) > 0 => variabili direttamente proporzionali
#   - se Cov(X, Y) < 0 => variabili inversamente proporzionali
#   - se Cov(X, Y) = 0 => variabili scorrelate

# Covarianza campionaria:
#   Cov(X, Y) = sum( ( x_i - media.campionaria(X) ) * ( y_i - media.campionaria(Y) ) ) / ( n - 1 )
cov( record$m100, record$m200 )   # oppure: cov( record$m200, record$m100 )

# Nota: Cov(X, X) = Var(X)
cov( record$m100, record$m100 )
var( record$m100 )

# Nota: scorrelazione non equivale ad indipendenza. Infatti:
# variabili indipendenti => variabili scorrelate (è falsa l'implicazione <=)
# Esempio: X~N(0, 1) vs. Y=X^2
# Cov(X, Y) = E[XY] - E[X] E[Y] = E[X^3] - E[X] E[X^2] = 0, ma X e Y NON sono indipendenti!

# Indice di correlazione lineare (Cor)
#   Cor(X, Y) = Cov(X, Y) / [ sd(X) * sd(Y) ] 

# Proprietà:
#   a) -1 <= Cor(X, Y) <= 1
#   b) Cor(X, Y) = 1   :  Y = a + b*X, con b > 0 
#   c) Cor(X, Y) = -1  :  Y = a + b*X, con b < 0

# Indice di correlazione lineare campionario
cov( record$m100, record$m200 ) / ( sd( record$m100 ) * sd( record$m200 ) )

cor( record$m100, record$m200 )

# Nota: Cor(X, X) = 1
cor( record$m100, record$m100 )

# Matrice di varianza e covarianza (dataset multivariato)
cov( record )

round( cov( record ), 2 )

# Matrice di correlazione
cor( record )

round( cor( record ), 2 )

# Scatterplot
dev.new()
pairs( record )

# Box plot
dev.new()
boxplot( record, col = c( 2, 3, 4, 5, 6, 7 ), main = 'Box plot' )

# 3 - ESEMPIO DI CONFRONTO TRA PIU' GRUPPI -------------------------------------

rm( list = ls( ) )
graphics.off( )

# ESERCIZIO 4 
# Analisi dei dati contenuti nel dataset 'iris',
# già presente in R nel pacchetto di base 'datasets'

# Il dataset contiene la lunghezza e la larghezza di sepali e petali
# per 3 diverse specie di iris (setosa, versicolor e virginica).
# Abbiamo 150 osservazioni di 5 variabili:
#  Sepal.Length: lunghezza sepalo
#  Sepal.Width: larghezza sepalo
#  Petal.Length: lungezza petalo
#  Petal.Width: larghezza petalo
#  Species:   specie di iris

data(iris)

head(iris)
dim(iris)
names(iris)

# Per avere una descrizione completa del dataset:
help(iris)

# Ispezioniamo il dataset con le funzioni viste:
table(iris$Species) # Il dataset è bilanciato rispetto alle diverse tipologie di fiore

# Possiamo calcolare i principali indici di posizione su tutte le
# variabili contemporaneamente sfruttando la funzione tapply

tapply(iris$Sepal.Width, iris$Species, summary) 
# summary, terzo parametro della funzione, viene applicato a Sepal.Width, primo parametro della funzione,
# per ciascun livello contenuto in Species, secondo parametro della funzione

tapply(iris$Sepal.Width, iris$Species, sd)


# A) Confrontiamo la variabile Sepal.Width tra le diverse specie, ovvero stratificando
# per la variabile Species
# Confrontiamo i  tre gruppi tramite l'istogramma dei dati

dev.new()
par(mfrow= c(3,1))
hist(iris$Sepal.Width[iris$Species=="setosa"],
     probability = T,
     main = 'Larghezza sepalo Iris Setosa',
     xlab = 'Iris Setosa',
     ylab = 'Densità',
     col = 2,
     breaks = seq(min(iris$Sepal.Width), 
                  max(iris$Sepal.Width), length.out=12)) 

hist(iris$Sepal.Width[iris$Species=="versicolor"],
     probability = T,
     main = 'Larghezza sepalo Iris Versicolor',
     xlab = 'Iris Versicolor',
     ylab = 'Densità',
     col = 3,
     breaks = seq(min(iris$Sepal.Width), 
                  max(iris$Sepal.Width), length.out=12))

hist(iris$Sepal.Width[iris$Species=="virginica"],
     probability = T,
     main = 'Larghezza sepalo Iris Virginica',
     xlab = 'Iris Virginica',
     ylab = 'Densità',
     col = 4,
     breaks = seq(min(iris$Sepal.Width), 
                  max(iris$Sepal.Width), length.out=12))

# Istogramma -> Le iris setosa sembrano avere un sepalo più largo delle altre due specie.
#               Non si vede alcuna differenza tra le altre due specie.

# Confrontiamo i  tre gruppi tramite il boxplot dei dati
dev.new()
boxplot( iris$Sepal.Width ~ iris$Species, 
         ylab = 'Larghezza sepalo', 
         main = 'Boxplot Larghezza sepalo', 
         col = c( 2, 3, 4 ))

# Boxplot -> Il boxplot conferma la tendenza delle iris setosa ad avere un sepalo più largo
#            rispetto alle altre due specie.
#            Le iris versicolor sembrano essere quelli con il sepalo più stretto.

# PER CASA:
# B) Confrontare la variabile Sepal.Length tra le diverse specie, ovvero stratificando
# per la variabile Species

# C) Confrontare la variabile Petal.Length tra le diverse specie, ovvero stratificando
# per la variabile Species

# D) Confrontare la variabile Petal.Width tra le diverse specie, ovvero stratificando
# per la variabile Species

# Consideriamo il dataset complessivo

# Scatter plot
col=rep(0, nrow(iris))
col[iris$Species=='setosa']     = 2 #rosso
col[iris$Species=='versicolor'] = 3 #verde
col[iris$Species=='virginica']  = 4 #blu

dev.new()
pairs( iris [,1:4], 
       main = 'Scatter plot Iris', 
       col = col, 
       pch = 16 )
    
# Matrice di correlazione per specie
cor(iris[iris$Species== 'setosa', 1:4])
cor(iris[iris$Species== 'versicolor', 1:4])
cor(iris[iris$Species== 'virginica', 1:4])

rm(list = ls() )
graphics.off()

# 4 - Esempi più complessi -----------------------------------------------------

# ESERCIZIO ANALISI di AIRQUALITY

# Analisi dei dati contenuti nel dataset 'airquality',
# già presente in R nel pacchetto di base 'datasets'
# Carichiamo il nuovo dataset
data('airquality')

# Il dataset contiene 153 osservazioni relative alle misurazioni giornaliere della
# qualità dell'aria a New York, dal settembre al maggio 1973.
# Il dataset è formato da 6 variabili

help('airquality')
head(airquality)

range(airquality$Month)

# Codifichiamo la variabile 'Month' come factor
airquality$Month <-
  factor(airquality$Month, labels = c('Maggio', 'Giugno', 'Luglio', 'Agosto', 'Settembre'))

# Analisi descrittiva univariata: guardiamo il dataset
summary(airquality[, 1:4])
table(is.na(airquality$Ozone)) # ci sono 37 valori mancanti
table(is.na(airquality$Solar.R)) # ci sono 7 valori mancanti

mean(airquality$Ozone)
mean(airquality$Ozone, na.rm = T) # calcoliamo la media omettendo valori mancanti

airquality <-
  airquality[complete.cases(airquality), ] # elimino osservazioni con dati mancanti

rm(list = ls())
graphics.off()

# ESERCIZIO: ANALISI ARANCE 
# Si consideri il dataset contenuto nel file di testo "arance.txt", contenente
# 35 osservazioni relative alla crescita di 5 alberi di arance con 3 variabili
# osservate:
#   - albero: codice identificativo dell'albero
#   - giorni: età dell'albero in giorni dal 31/12/1968
#   - circonferenza: valore della circonferenza in mm

# Si richiede di:
# 1. Caricare il dataset.
# 2. Trasformare le età in mesi, considerandoli tutti di 30 giorni e
#    approssimandoli all'unità. [Suggerimento: utilizzare il comando
#    round( mesi, 0 ).]
# 3. Trasformare la circonferenza in diametro. [Suggerimento: utilizzare pi per
#    il valore di pi greco.]
# 4. Aggiungere le nuove variabili al dataframe.
# 5. Calcolare il massimo diametro raggiunto da ogni albero. Assicurarsi che la
#    variabile sulla quale vogliamo stratificare il campione sia categorica.
#    [Suggerimento: utilizzare il comando as.factor().]
# 6. Qual è l'albero che ha il diametro più piccolo dopo 22 mesi?
# 7. Quali sono gli alberi che presentano una maggiore crescita? Riportare i
#    valori della crescita e mostrare il risultato graficamente.

# 1. Caricare il dataset
arance = read.table( 'arance.txt', header = TRUE )

str( arance )

# 2. Trasformare le età in mesi
mesi = round( arance$giorni / 30, 0 )
mesi

# 3. Trasformare la circonferenza in diametro
diametro = round( arance$circonferenza / pi, 2 )
diametro

# 4. Aggiungere le nuove variabili al dataframe.
arance = data.frame( arance, mesi, diametro )

head( arance )

# 5. Calcolare il massimo diametro raggiunto da ogni albero
class( arance$albero )

arance$albero = as.factor( arance$albero )

arance$albero

tapply( arance$diametro, arance$albero, max )

# 6. Qual è l'albero che ha il diametro più piccolo dopo 22 mesi? 
diametro_min = min( arance$diametro[ arance$mesi == 22 ] )

arance$albero[ which( arance$diametro == diametro_min ) ]

# 7. Qual è l'albero che presenta una maggiore crescita?
MAX = tapply( arance$diametro, arance$albero, max )
MIN = tapply( arance$diametro, arance$albero, min ) 

crescita_tot = MAX - MIN

max( crescita_tot )

which( crescita_tot == max( crescita_tot ) )  # oppure: which.max( crescita_tot )

# Box plot: diametro vs. albero
dev.new()
boxplot( arance$diametro ~ arance$albero, main = "Box plot",
         col = rainbow( length( levels( arance$albero ) ) ),
         ylab = 'Diametro', xlab = 'Albero')
abline ( h = median( arance$diametro ), lty = 2, lwd = 2 )
legend( 'topright', legend = 'Mediana', lty = 2, lwd = 2)

# Andamento temporale del diametro degli alberi
dev.new()
plot( unique( arance$mesi ), arance$diametro[ which( arance$albero == '1' ) ], 
      col = 1, type = 'l', ylim = c( 10, 80 ), lwd = 2, xlab = 'Mesi',
      ylab = 'Diametro', main = 'Andamento temporale del diametro degli alberi' )
lines( unique( arance$mesi ), arance$diametro[ which( arance$albero == '2' ) ], col = 2, lwd = 2 )
lines( unique( arance$mesi ), arance$diametro[ which( arance$albero == '3' ) ], col = 3, lwd = 2 )
lines( unique( arance$mesi ), arance$diametro[ which( arance$albero == '4' ) ], col = 4, lwd = 2 )
lines( unique( arance$mesi ), arance$diametro[ which( arance$albero == '5' ) ], col = 5, lwd = 2 )
grid()
legend( 'topleft', col = 1:5, lty = rep( 1, 5 ), lwd = rep( 2, 5 ),
        legend = c( 'Albero 1', 'Albero 2', 'Albero 3', 'Albero 4', 'Albero 5' ) )

# Andamento temporale della crescita degli alberi
crescite = tapply( arance$diametro, arance$albero, diff )

dev.new()
plot( unique( arance$mesi)[ -1 ], crescite[[ 1 ]], 
      type = 'l', lwd = 2, col = 1, ylim = c( 0, 20 ),
      xlab = 'Mesi', ylab = 'Crescita',
      main = 'Andamento temporale della crescita degli alberi' )
lines( unique( arance$mesi)[ -1 ], crescite[[ 2 ]], col = 2, lwd = 2 )
lines( unique( arance$mesi)[ -1 ], crescite[[ 3 ]], col = 3, lwd = 2 )
lines( unique( arance$mesi)[ -1 ], crescite[[ 4 ]], col = 4, lwd = 2 )
lines( unique( arance$mesi)[ -1 ], crescite[[ 5 ]], col = 5, lwd = 2 )
legend( 'topright', col = 1:5, lty = rep( 1, 5 ), lwd = rep( 2, 5 ),
        legend = c( 'Albero 1', 'Albero 2', 'Albero 3', 'Albero 4', 'Albero 5' ) )
