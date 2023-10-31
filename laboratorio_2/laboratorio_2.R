###############################################################
##################    CORSO DI STATISTICA    ##################
################## per INGEGNERIA MATEMATICA ##################
###############################################################

###############################################################
#########               LABORATORIO 2               ###########
#########            STATISTICA DESCRITTIVA         ###########
###############################################################

rm( list = ls() )
graphics.off()

# Argomenti del secondo laboratorio:
# 1 - I pacchetti
# 2 - Funzioni
# 3 - I cicli e le condizioni
# 4 - Analisi descrittiva di due variabili quantitative (bivariata)
# 5 - Analisi descrittiva di più variabili quantitative (multivariata)
# 6 - Esempio di confronto tra due gruppi
# 7 - Esempio di confronto tra più gruppi
# 8 - Alcuni esempi più complessi

# set working directory
if(!require(pacman, quietly = TRUE)) install.packages("pacman")
pacman::p_load("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))


###############################################################
################        1 - PACCHETTI        ##################
###############################################################
# In generale, funzioni e dati di nostro interesse sono contenuti nei 
# 'pacchetti' R, che possono essere pensati come dei veri e propri archivi. 

# Abbiamo due possibilità per scaricare pacchetti in R:

# 1) tramite il seguente comando da linea di comando:
# install.package("MioPacchetto") 
# Il pacchetto viene scaricato dal CRAN (è necessaria una connessione internet)

# 2) tramite interfaccia grafica da RStudio:
# 'Tools' -> 'Install Packages...' -> "MioPacchetto" -> 'Install'

# Una volta scaricato il pacchetto di nostro interesse è necessario caricarlo
# durante la sessione per usufruire delle funzioni e/o dati al suo interno:
# library(MioPacchetto) # carico MioPacchetto nella sessione corrente

# installiamo il pacchetto MASS (contiene vari datasets)
install.packages("MASS") 

# carico il pacchetto
library(MASS)
###############################################################
################ 2 - I CICLI E LE CONDIZIONI ##################
###############################################################

# IL FOR: itera su un contatore

for (i in 1:5) {
  print('Ripeto finchè non arrivo a 5!')
}
# il contatore i assume i valori contenuti in 1:5 = 1 2 3 4 5

# altro esempio
# i assume i valori 2 5 10 
for (i in c(2,5,10)) {
  print(i)
}

# IL WHILE: itera fino a quando la condizione specificata (dentro le parentesi tonde) 
#           è valida
i <-  0
while (i < 5) {
  print(paste(i,' -> La condizione è verificata!', sep=""))
  i <- i + 1
}

# IF-THEN: se la condizione specificata (dentro le parentesi tonde) è vera 
#          allora vengono eseguite le istruzioni dopo if (then).
i <- 1
if (i < 5){
  i <- i+1
}
i

i <- 6
if (i < 5){
  i <- i+1
}
i

# IF-THEN-ELSE: se la condizione specificata (dentro le parentesi tonde) è vera 
#               vengono eseguite le istruzioni dopo if (then), 
#               altrimenti (else) vengono eseguite le istruzioni della condizione 
#               alternativa.

i <- 6
if (i < 5){
  i <- i+1
}else{
  i <- i-1
}
i

###############################################################
################        3 - FUNZIONI         ##################
###############################################################

#NOME_FUNZIONE = function(parametro_1, paramentro_2, ...){
#             ... CORPO FUNZIONE ...
#                 return(...)
#}

# Esempio 1

Hello_ = function(Nome="Mario Rossi"){ 

    cat(paste("Ciao ", Nome, "!",sep=""))

}

# viene utilizzato il parametro indicato come default  
Hello_()

Hello_(Nome="Aldo")

# Esempio 2: Calcolo della frequenza cumulata (Esercizio 1 - Esercitazione 1)

freq_ass = c(14, 46, 58, 76, 68, 62, 48, 22, 6)

freq_rel = freq_ass / sum(freq_ass)
freq_rel

SommaCumulata = function(Vettore){

  risultato = vector(mode = "numeric", length = length(Vettore))
  
  for(i in 1:length(Vettore)){
   if(i == 1){
     risultato[i] = Vettore[i]
   }else{  
   risultato[i] = risultato[i-1] + Vettore[i] #i>=2
   } 
  }
  
  return(risultato)
}

freq_cum = SommaCumulata(freq_rel)
freq_cum

# funzione built-in
cumsum(freq_rel)

freq_ass = sample(x=1:20,size= 1e7, replace=T)
freq_rel = freq_ass / sum(freq_ass)

# Non implementare una funzione già esistente!
system.time(SommaCumulata(freq_rel))
system.time(cumsum(freq_rel))

###############################################################
#### 4 - ANALISI DESCRITTIVA DI DUE VARIABILI QUANTITATIVE ####
###############################################################
# Analisi dei dati quantitativi contenuti nel file 'record.txt'.
# Il dataset contiene i record dei tempi ottenuti da rappresentanti di diverse nazioni
# in varie discipline di corsa di velocità e resistenza
# ( 100 metri, 200 metri, 400 metri, 800 metri, 1500 metri, 3000 metri )


rm( list = ls() )   # elimina variabili dall'ambiente di lavoro
graphics.off()      # chiude le finestre grafiche attive 

# Importiamo il dataset
record = read.table( 'record.txt', header = TRUE )

head( record )
dim( record )

# Consideriamo le variabili 100m e 200m.

record100200 = record[,1:2] # SALVO una parte di record in una nuova variabile.
                            # In particolare, record100200 contiene
                            # TUTTE le righe e le prime DUE COLONNE di record
head(record100200)
dim(record100200)
# LE STATISTICHE:
# Calcoliamo la matrice di varianza e covarianza delle variabili
var( record100200 ) 

# Calcoliamo la matrice delle correlationi
cor( record100200 ) # correlazione_xy = cov_xy / ( sqrt( var_x * var_y ) )
                    
# PLOT

# 1. Scatterplot
dev.new()
plot( record100200[ , 1 ] , record100200[ , 2 ] , 
      main = 'Scatter plot records 100m e 200m', 
      xlab = 'Records 100m', 
      ylab = 'Records 200m', 
      pch=16)

colMeans( record100200 ) # Vettore delle medie
points(colMeans( record100200 )[1], colMeans( record100200 )[2], 
       pch=16 , 
       col='red', 
       cex=2)

# NOTA:
# points( x , y, ...) AGGIUNGE il punto/i di coordinate (x,y) al grafico precedente
# cex = ... fissa la dimensione del punto (cex=1 default)
# pch = ... fissa la forma del punto (vedi allegati) 

# Possiamo aggiungere ai punti i nomi dei paesi
dev.new()
plot( record100200[,1] , record100200[,2], 
      xlab = 'Records 100m', 
      ylab = 'Records 200m' ,
      col = "white" )
text( record100200[,1] , record100200[,2], 
      rownames(record100200))

# 2. Boxplot
dev.new()
boxplot( record100200, 
         main = 'Boxplot records 100m e 200m', 
         ylab = 'Tempo ( sec )' )

# Notiamo che i tempi per correre i 200 mt sono sensibilmente più grandi

# 3. Istogrammi
dev.new()
par(mfrow=c(1,2))
hist( record100200[ , 1], prob = T, 
      main = 'Istogramma records 100m', 
      xlab = 'Tempo ( sec )', 
      ylab = 'Densità' )

hist( record100200[ , 2], prob = T, 
      main = 'Istogramma records 200m', 
      xlab = 'Tempo ( sec )', 
      ylab = 'Densità' )
# NOTA
# La densità in un istogramma è una rappresentazione simile alla frequenza relativa,
# ma tale per cui l'area di ogni rettangolo è pari alla frequenza relativa di quella classe.
# La densità del rettangolo i-esimo risulta essere d_i = fr_i/c_i, 
# dove fr_i è la frequenza relativa della classe i-esima e c_i è la sua dimensione 


# Istogrammi nella stessa scala del tempo (ascisse).
dev.new()
par(mfrow = c(2,1))

hist( record100200[ , 1], prob = T, 
      main = 'Istogramma records 100m', 
      xlab = 'Tempo ( sec )', ylab = 'Densità' , 
      breaks= seq(from=10,to=30,by=0.5))

hist( record100200[ , 2], prob = T, 
      main = 'Istogramma records 200m', 
      xlab = 'Tempo ( sec )', ylab = 'Densità' ,  
      breaks= seq(from=10,to=30,by=0.5))

dev.new()
par( mfrow = c( 1, 1 ) )
hist( record100200[,1], prob = T,
      col = "blue3",
      main="Confronto",
      xlab = 'Tempo ( sec )', ylab = 'Densità' , 
      breaks= seq(from=10,to=30,by=0.5))

hist( record100200[,2], prob = T,
      col = "red2",
      xlab = 'Tempo ( sec )', ylab = 'Densità' ,  
      breaks= seq(from=10,to=30,by=0.5), 
      add=T)
legend("topright", c("100m", "200m"), 
       col=c("blue3", "red2"), pch=15, cex=1.5)
# NOTA: add = T aggiunge il secondo istogramma alla finestra grafica attualmente attiva

# 4. Inserisco tutti i grafici nella stessa figura 
{ 
dev.new()
par( mfrow = c( 2, 2 ) )

hist( record100200[,1] , prob = T, 
      main = 'Istogramma records 100m', 
      xlab = 'Tempo ( sec )', 
      ylab = 'Densità' )
hist( record100200[,2] , prob = T, 
      main = 'Istogramma records 200m', 
      xlab = 'Tempo ( sec )',
      ylab = 'Densità' )

boxplot( record100200, 
         main = 'Boxplot records 100m e 200m', 
         ylab = 'Tempo ( sec )' )
plot( record100200[ ,1] , record100200[ ,2] , 
      main = 'Scatter plot records 100m e 200m', 
      xlab = 'Records 100m', 
      ylab = 'Records 200m' )
}

###############################################################
### 5 - ANALISI DESCRITTIVA DI PIU' VARIABILI QUANTITATIVE ####
###############################################################

# Consideriamo il dataset completo, con tutte le variabili

head(record)

# PLOT: subplot degli scatterplot appaiati tra tutte le variabili
dev.new()
pairs(record)

# LE STATISTICHE:
colMeans(record)
cov(record)
cor(record)

# I PLOT:
# 1. Boxplot
dev.new()
boxplot(record, main = 'Boxplot records', 
        xlab = 'Categorie', 
        ylab = 'Tempo ( sec )' )

# I tempi per percorrere le gare di velocità e quelle di resistenza
# aumentano esponenzialmente

###############################################################
########### 6 - ESEMPIO DI CONFRONTO TRA DUE GRUPPI ###########
###############################################################

# Analisi dei dati quantitativi contenuti nel file 'anagrafica.txt'.
# I dati consistono in osservazioni di 4 variabili:
#    ETA
#    PESO
#    ALTEZZA
#    SESSO ( 1 = femmina, 2 = maschio )
# che rappresentano l'età, il peso, l'altezza e il sesso di pazienti
# ammessi a diversi reparti di ospedali della Lombardia con una diagnosi di infarto

#  confrontare la distribuzione di una variabile quantitativa ( ALTEZZA ) 
#  in diversi gruppi individuati da una variabile categorica ( SESSO )
# Fonte: database clinico, Lombardia

rm( list = ls( ) )
graphics.off( )

# Importiamo il dataset
anagrafica = read.table( 'anagrafica.txt', header = TRUE )

head( anagrafica )
dim( anagrafica )
names( anagrafica )

attach(anagrafica)

# Codifichiamo la variabile categoriale SESSO  come factor e
# aggiungiamo delle etichette ai livelli
table(SESSO)
SESSO[ SESSO == 1 ] = 'F' # SESSO == 1  restituisce un vettore di booleani (TRUE / FALSE) 
                           # Vengono selezionate tutte le posizioni di SESSO in corrispondenza di 1
                           # quindi vengono sostituiti gli "1" con le "F"
SESSO[ SESSO == 2 ] = 'M'

SESSO = as.factor(SESSO)   
summary(SESSO)

# OPPURE 
SESSO = factor(anagrafica$SESSO, labels = c("F","M"))
summary(SESSO)

# Coloriamo lo scatter plot in base al sottogruppo
col = rep( 0, nrow(anagrafica)) # Nota. nrow(anagrafica) restituisce il numero di righe di anagrafica

col [ SESSO == 'F' ] = 'pink'           
col [ SESSO == 'M' ] = 'lightblue'
class(col)

dev.new()
pairs( anagrafica[ ,1:3] , main = 'Scatter plot', 
       col = col, 
       pch = 16 )

# Matrice di correlazione
cor(anagrafica[,1:3]) 

# A) Confrontiamo la variabile ALTEZZA tra maschi e femmine, ovvero stratificando
# per la variabile SESSO

# Istogramma dei dati per sottogruppi

# Nota per poter confrontare i due grafici, usiamo la stessa scala 
#      sull'asse delle ascisse e le stesse classi

dev.new()
par( mfrow = c( 2, 1 ) )
hist( ALTEZZA[ SESSO == 'F' ] , prob = TRUE,
      xlab = 'Altezza [ cm ] ', 
      ylab = 'Densità', 
      main = 'Istogramma Altezza Femmine',
      col = 'pink', xlim = range( ALTEZZA ),
      breaks = seq( 150, 195, by = 5 ) )

hist( ALTEZZA[ SESSO == 'M' ] , prob = TRUE, 
      xlab = 'Altezza [ cm ] ', 
      ylab = 'Densità',
      main = 'Istogramma Altezza Maschi',
      col = 'lightblue', xlim = range( ALTEZZA ),
      breaks = seq( 150, 195, by = 5 ) )

# Istogrammi -> tendenza delle donne ad essere più basse rispetto agli uomini
#		            maggiore simmetria nella distribuzione degli uomini
#		            a fronte di una asimmetria nella distribuzione delle donne.

# Nota: il confronto sensato se i grafici hanno le stesse scale sugli assi e le
#       stesse classi!

# Boxplot
dev.new()
boxplot( ALTEZZA[ SESSO == 'F' ] , ALTEZZA[ SESSO == 'M' ] , 
         names = c( 'Femmine', 'Maschi' ), 
         main = 'Boxplot Altezza',
         col = c( 'pink', 'lightblue' ), 
         ylab = 'Altezza [ cm ] ', 
         ylim = c( 150, 195 ) )

# Oppure utilizzo la formula per specificare il fattore di raggruppamento
dev.new()
boxplot( ALTEZZA ~ SESSO, 
         names = c( 'Femmine', 'Maschi' ), 
         main = 'Boxplot Altezza',
         col = c( 'pink', 'lightblue' ), 
         ylab = 'altezza [ cm ] ', 
         ylim = c( 150, 195 ) )

# Boxplot -> la distribuzione dell'altezza nelle donne si concentra su valori inferiori 
#            rispetto a quella negli uomini. La distribuzioni dell'altezza negli uomini 
#            sembra abbastanza simmetrica, mentre nelle donne si osserva 
#            una asimmetria positiva (verso destra). Coerente istogramma precedente.


# Quattro Grafici nella stessa finestra grafica con boxplot (orizzontali)
dev.new()
par(mfrow=c(2,2))
hist( ALTEZZA[ SESSO == 'F' ] , prob = TRUE,
      xlab = 'Altezza [ cm ] ', 
      ylab = 'Densità', 
      main = 'Istogramma Altezza Femmine',
      col = 'pink', xlim = range( ALTEZZA ),
      breaks = seq( 150, 195, by = 5 ) )
  
hist( ALTEZZA[ SESSO == 'M' ] , prob = TRUE, 
      xlab = 'Altezza [ cm ] ', 
      ylab = 'Densità',
      main = 'Istogramma Altezza Maschi',
      col = 'lightblue', xlim = range( ALTEZZA ),
      breaks = seq( 150, 195, by = 5 ) )
  
boxplot( ALTEZZA[ SESSO == 'F' ], 
         names = 'Femmine', 
         main = 'Boxplot Altezza\n Femmine',
         col = 'pink' , 
         xlab = 'Altezza [ cm ] ', 
         ylim = c( 150, 195 ),
         horizontal = TRUE)
  
boxplot(ALTEZZA[ SESSO == 'M' ] , 
        names =  'Maschi' , 
        main = 'Boxplot Altezza\n Maschi',
        col =  'lightblue' , 
        xlab = 'Altezza [ cm ] ', 
        ylim = c( 150, 195 ),
        horizontal = TRUE)
# NB il parametro "horizontal = TRUE" ruota il boxplot in orizzontale

# Scatter plot
dev.new()
plot( ALTEZZA, ETA, main = 'Scatter plot altezza-eta', 
      xlab = 'Altezza', ylab = 'Eta' )

detach(anagrafica)

###############################################################
########### 7 - ESEMPIO DI CONFRONTO TRA PIU' GRUPPI ##########
###############################################################

rm( list = ls( ) )
graphics.off( )

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

attach(iris)

# Ispezioniamo il dataset con le funzioni viste:
table(Species) # Il dataset è bilanciato rispetto alle diverse tipologie di fiore

# Possiamo calcolare i principali indici di posizione su tutte le
# variabili contemporaneamente sfruttando la funzione tapply

tapply(Sepal.Width, Species, summary) 
# summary, terzo parametro della funzione, viene applicato a Sepal.Width, primo parametro della funzione,
# per ciascun livello contenuto in Species, secondo parametro della funzione

tapply(Sepal.Width, Species, sd)


# A) Confrontiamo la variabile Sepal.Width tra le diverse specie, ovver stratificando
# per la variabile Species
# Confrontiamo i  tre gruppi tramite l'istogramma dei dati

######################################## 
#         INSERISCI CODICE QUI         #
#                                      #
# dev.new()
# par(mfrow= c(3,1))
# hist(Sepal.Width[Species=="setosa"],
#      probability = T,
#      main = 'Larghezza sepalo Iris Setosa',
#      xlab = 'Iris ',
#      ylab = 'Densità', ... )
#hist(...)
#hist(...)

dev.new()
par(mfrow= c(3,1))
hist(Sepal.Width[Species=="setosa"],
     probability = T,
     main = 'Larghezza sepalo Iris Setosa',
     xlab = 'Iris Setosa',
     ylab = 'Densità',
     col = 2,
     breaks = seq(min(Sepal.Width), 
                  max(Sepal.Width), length.out=12)) 

hist(Sepal.Width[Species=="versicolor"],
     probability = T,
     main = 'Larghezza sepalo Iris Versicolor',
     xlab = 'Iris Versicolor',
     ylab = 'Densità',
     col = 3,
     breaks = seq(min(Sepal.Width), 
                  max(Sepal.Width), length.out=12))

hist(Sepal.Width[Species=="virginica"],
     probability = T,
     main = 'Larghezza sepalo Iris Virginica',
     xlab = 'Iris Virginica',
     ylab = 'Densità',
     col = 4,
     breaks = seq(min(Sepal.Width), 
                  max(Sepal.Width), length.out=12))

# Istogramma -> Gli iris setosa sembrano avere un sepalo più largo delle altre due specie.
#               Non si vede alcuna differenza tra le altre due specie.

# Confrontiamo i  tre gruppi tramite il boxplot dei dati
dev.new()
boxplot( Sepal.Width ~ Species, 
         ylab = 'Larghezza sepalo', 
         main = 'Boxplot Larghezza sepalo', 
         col = c( 2, 3, 4 ))

# Boxplot -> Il boxplot conferma la tendenza degli iris setosa ad avere un sepalo più largo
#            rispetto alle altre due specie.
#             Gli iris versicolor sembrano essere quelli con il sepalo più stretto.

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
col[Species=='setosa']     = 2 #rosso
col[Species=='versicolor'] = 3 #verde
col[Species=='virginica']  = 4 #blu

dev.new()
pairs( iris [,1:4], 
       main = 'Scatter plot Iris', 
       col = col, 
       pch = 16 )
    
# Matrice di correlazione per specie
cor(iris[Species== 'setosa', 1:4])
cor(iris[Species== 'versicolor', 1:4])
cor(iris[Species== 'virginica', 1:4])

detach(iris)
      
rm(list = ls() )
graphics.off()

###############################################################
############### 8 - ALCUNI ESEMPI PIU' COMPLESSI ##############
###############################################################

############### ESEMPIO 1 ###############

# Carichiamo il dataset GEYSER
library(MASS) # Pacchetto che contiene molti dataset
data(geyser)

head(geyser)
dim(geyser)
names(geyser)

# Per avere una descrizione completa del dataset:
help(geyser)

attach(geyser)

# PLOT:
# 1. Consideriamo la variabile duration e ispezioniamo i dati tramite boxplot
dev.new()
boxplot(duration, ylab = 'Tempi ( sec )', main = 'Boxplot Durata eruzione')

# Boxplot -> Distribuzione asimmetrica verso sinistra:
# mediana molto spostata verso l'alto

#install.packages("beeswarm")
library(beeswarm)
beeswarm(duration, add = T, col = "indianred3")
# Aggiungiamo le osservazioni per vedere come sono distribuite

# 2. Consideriamo la variabile duration e ispezioniamo i dati tramite istogramma
hist(
  duration,
  prob = TRUE,
  main = 'Durata eruzione',
  xlab = 'Tempi ( min )',
  ylab = 'Densità'
)

# Istogramma -> distribuzione bimodale

# 3. Consideriamo la variabile waiting e ispezioniamo i dati tramite boxplot
boxplot(waiting, ylab = 'Tempi ( sec )', main = 'Boxplot Attesa eruzione')

# Boxplot -> Distribuzione asimmetrica verso sinistra (mediana molto spostata verso l'alto)
beeswarm(waiting, add = T, col = "indianred3")

# 4. Consideriamo la variabile waiting e ispezioniamo i dati tramite istogramma
hist(
  waiting,
  prob = TRUE,
  main = 'Attesa eruzione',
  xlab = 'Tempi ( min )',
  ylab = 'Densità'
)

# Istogramma -> distribuzione bimodale

# 5. Indaghiamo (in modo esplorativo) sulla bimodalità della distribuzione,
# tenendo conto simultaneamente delle due variabili (waiting e duration), 
# ispezionando i dati tramite scatterplot
plot(duration,
     waiting,
     main = 'Scatter plot geyser',
     xlab = 'Durata',
     ylab = 'Attesa')


detach(geyser)
rm(list = ls())
graphics.off()

############### ESEMPIO 2 ###############

# Analisi dei dati contenuti nel dataset 'airquality',
# già presente in R nel pacchetto di base 'datasets'

# Carichiamo il nuovo dataset
data('airquality')

# Il dataset contiene 153 osservazioni relative alle misurazioni giornaliere della
# qualità dell'aria a New York, dal settembre al maggio 1973.
# Il dataset è formato da 6 variabili

help('airquality')
head(airquality)

# Codifichiamo la variabile 'Month' come factor
airquality$Month <-
  factor(airquality$Month, labels = c('M', 'G', 'L', 'A', 'S'))

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

############### ESEMPIO 3 ###############

# Analisi dei dati contenuti nel dataset 'ChickWeight' 
# già presente in R nel pacchetto di base 'datasets'

# I dati consistono in 578 misurazioni del peso di 50 pulcini in diversi tempi.
# I vari pulcini sono sottoposti a diversi regimi alimentati.
# In particolare abbiamo 4 variabili
#  weight: peso del pulcino
#  Time:  tempo in cui viene pesato il pulcino (numero di giorni dalla nascita)
#  Chick: identificatore di ciascuno dei 50 pulcini
#  Diet:  tipo di dieta a cui è stato sottoposto il pulcino ( 1, ...4 )

# Per avere una descrizione completa del dataset:
help( ChickWeight )

head( ChickWeight )
dim( ChickWeight )
names( ChickWeight )

# NB: anche se tutte le variabili sono codificate con numeri naturali,
#     non sono tutte dello stesso tipo!
#     weight e Time sono variabili quantitative
#     Chick e Diet sono variabili categoriche

attach( ChickWeight )

# Scatter plot
dev.new()
plot( Time, weight, 
      main = 'Scatter plot ChickWeight', 
      xlab = 'Giorni', 
      ylab = 'Peso' )

# Inseriamo l'informazione delle variabili categoriche,
# provando a rappresentare separatamente i vari pulcini e diete

# Numero di pulcini per ogni dieta
for(i in 1:4){
  print(length( unique( Chick[Diet==i]) ))
}

# Selezioniamo il pulcino con identificatore 1
ChickWeight [Chick==1, ]
dev.new()
plot( Time [Chick==1], weight[Chick==1], 
      main = 'ChickWeight', 
      xlab = 'Tempo',
      ylab = 'Peso', 
      ylim = range( weight ),
      type = 'l', lty= 1,      # oppure lty = "dashed"
      col=2,
      lwd=2)
# NOTA: type='l' -> LINEA (type='p' default -> PUNTO)
#       lwd=2        fissa lo spessore della linea (lwd -> "Line WiDth")
#       lty=1        fissa il tipo di linea        (lty -> "Line TYpe")

# Disegnamo una linea per ogni pulcino, colorando in base alla dieta

colors_ = 2:5 # 1 colore per ogni Dieta
for ( i in 2:50 ){
  points(Time[which(Chick==i)], weight[which(Chick==i)], 
       type = 'l', lwd=2,
       col=colors_[ unique(Diet[ which(Chick==i)]) ])
  legend("topleft", legend=levels(Diet), lty=1, lwd=3, col=colors_)
}
# NOTA: points(...) AGGIUNGE le linee alla finestra grafica attualmente attiva

# Consideriamo per ora solo il peso finale
TF = max( Time )
TF

ChickWeight_TF = ChickWeight[Time==TF,]

detach( ChickWeight )
attach( ChickWeight_TF )

# Numero di pulcini per ogni dieta
for(i in 1:4){
  print(length( unique( Chick[Diet==i]) ))
}

# oppure
# count_chicks <- function(x){length(unique(x))}
# tapply(Chick, Diet, count_chicks)
# Alla misurazione finale sono arrivati un numero di polli diverso per le varie diete...

# Istogramma dei dati per sottogruppi
# NB: per poter confrontare i due grafici, usiamo la stessa scala sull'asse 
#     delle ascisse e le stesse classi
dev.new()
par( mfrow = c(4,1))
for(i in 1:4){
  hist( weight [Diet == i] , prob = TRUE, 
        xlab = 'Peso', 
        ylab = 'Densità', 
        main = paste('Istogramma Peso Dieta ', i),
        col = colors_[i], 
        xlim = range( weight ), breaks = seq( 50, 400, by = 50 ) )
}
# Istogrammi -> tendenza della dieta 3 a dare una crescita maggiore delle altre diete.
#        Maggiore simmetria nella distribuzione della dieta 4


# Boxplot dei dati per sottogruppi

dev.new()
boxplot( weight ~ Diet, 
         names = c( 'Diet 1', 'Diet 2', 'Diet 3', 'Diet 4' ), 
         main = 'Boxplot Peso',
         col = colors_, 
         ylab = 'Peso', ylim = c( 50, 400 ) )
# Boxplot -> confermano la nostra ipotesi che la dieta 3 si concentri su valori di peso più elevati
#            La distribuzione del peso nelle diete 2 e 4 sono abbastanza simmetriche, 
#            almeno nella parte centrale, mentre le diete 1 e 3 sono più asimmetriche.
#            La variabilità nella dieta 4 è minore che per le altre diete.

detach( ChickWeight_TF )

rm(list = ls())
graphics.off()
