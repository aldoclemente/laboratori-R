#'###########################################################'#
#'########               LABORATORIO 1               ########'#
#'######## INTRODUZIONE A R & STATISTICA DESCRITTIVA ########'#
#'###########################################################'#

# Argomenti del primo laboratorio:
# 0 - Introduzione a R
# 1 - Primi passi 
# 2 - Oggetti e assegnazioni
# 3 - Linea di comando
# 4 - Workspace
# 5 - Tipologie di dati
# 6 - Analisi descrittiva di una variabile numerica
# 7 - Analisi descrittiva di una variabile categorica

# 0 - BASI DI R -----------------------------------------------

# Vedi slide

# 1 - PRIMI PASSI ---------------------------------------------

# La console di R può essere utilizzata come semplice calcolatrice. 
# E' un ambiente interattivo, ossia i comandi producono una risposta
# immediata:

# 1.1 SOMMA
2+2

# 1.2 MOLTIPLICAZIONE
2*2

# 1.3 SOTTRAZIONE E DIVISIONE
(2 - 3)/6

# 1.4 ELEVAMENTO A POTENZA
2^2

# Esistono diverse funzioni per le principali operazioni matematiche
# e trigonometriche. Per dare solo qualche esempio:

# 1.5 RADICE QUADRATA
sqrt(9)

# 1.6 LOGARITMO
log(1)

# 1.7 ESPONENZIALE
exp(0)

# 1.8 OPERATORI LOGICI
# minore: <
# minore o uguale: <=
# maggiore: >
# maggiore o uguale: >=
# uguale: ==
# diverso: !=
# è l’intersezione (AND): &
# è l’unione (OR): |
# è la negazione (NOT): !

# In R i valori mancanti sono rappresentati dal valore NA (“Not Available”).
# Le operazioni logiche eseguite su valori mancanti o comunque non utilizzabili,
# restituiscono ancora il valore NA.

# Operazioni aritmetiche “impossibili”, come ad esempio le divisioni per zero,
# restituiscono il valore NaN che significa che “non `e un numero” ( “Not a Number”).


# 2 - OGGETTI E ASSEGNAMENTO ----------------------------------

# Ogni entità che il programma crea e manipola è definita un oggetto, 
# che può essere un numero, una variabile, una funzione, o più in generale, 
# strutture costruite a partire da tali componenti.

# I nomi degli oggetti non devono contenere spazi vuoti, simboli matematici
# e non devono iniziare con un numero:
pro va = 2
x-y = 2
2x = 2

# Alcuni nomi hanno un significato in R e non sono quindi utilizzabili:
for = 3

# Tutte le lettere ed i numeri possono essere utilizzati per dare nomi
# a variabili, funzioni e procedure; sono però esclusi gli spazi e tutti
# i segni di punteggiatura ad eccezione del punto, comunemente usato per
# separare le parole che compongono il nome.

# VARIABILE NUMERICA
variabile.numerica <- 2 # oppure
variabile.numerica = 2
variabile.numerica

# VARIABILE LOGICA
variabile.logica <- 3>5 # oppure
variabile.logica = 3>5
variabile.logica

# STRINGHE (ovvero insieme di caratteri)
variabile.stringa <- "pippo" # oppure
variabile.stringa = "pippo"
variabile.stringa

# Ogni comando deve essere separato da un punto e virgola (“;”) oppure
# deve essere su una nuova linea.

# Testi di commento possono essere aggiunti dopo il simbolo cancelletto (“#”),
# tutto ciò che si trova dopo questo simbolo e fino alla riga successiva viene
# ignorato da R.

# 3 - LINEA DI COMANDO ----------------------------------------

ls() # Fornisce la lista dei nomi degli oggetti in memoria
rm(variabile.numerica, variabile.logica) # Rimuove tutti gli oggetti tra parentesi
ls()
rm(list = ls()) # Cancella tutti gli oggetti nello spazio di lavoro
ls()

# Ogni installazione di R contiene ottima ed abbondante documentazione.
# Inoltre, è disponibile un help in linea consultabile con la funzione help():

# help(nome.funzione) # oppure
# ?nome.funzione

help(cbind) # oppure
?cbind

# Le funzioni di R, come si può osservare dall’help, si compongono in:
# funzione(argomento1 = ..., argomento2 = ..., ...)
# L’ordine degli argomenti è importante all’interno di una funzione se
# non viene indicato il nome dell’argomento, mentre se questo viene
# specificato, la sua posizione è indifferente.

rm(list = ls())

# 4 - WORKSPACE -----------------------------------------------

# Gli oggetti creati dall’utente vengono temporaneamente salvati nello
# spazio di lavoro (workspace).

getwd() # mostra il percorso della cartella nel computer che è stata
# automaticamente selezionata al momento dell’installazione del programma.

setwd() # per modificare lo spazio di lavoro, e cambiare directory. 
setwd("path/to/laboratorio_1/")

# In alternativa, dal menu in alto:
#       "Session" -> "Set Working Directory" -> "To Source File Location"
# seleziona come working directory la cartella in cui il 'laboratorio_1.R' e' salvato.

# In alternativa, le seguenti righe di codice righe permettono di selezionare 
# come working directory la cartella in cui il 'laboratorio_1.R' e' salvato.
if(!require(pacman, quietly = TRUE)) install.packages("pacman")
pacman::p_load("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))

dir() # mostra il nome dei file presenti nella directory di lavoro.

# 5 - TIPOLOGIE DI DATI ---------------------------------------

# 5.1 VARIABILI
# contengono un singolo dato
variabile.numerica <- 42
variabile.numerica

stringa <- "stringa"
stringa

# 5.2 VETTORI
# insieme lineare di elementi omogenei per tipologia (tutti numeri, tutte
# stringhe, ecc.)

# Definizione di un vettore (colonna)
vettore.colonna <- c(1,2,3,4,5,6,7,8,9,10)
vettore.colonna
vettore.riga <- t(vettore.colonna)
vettore.riga

vettore.stringhe <- c("pippo", "topolino", "pluto", "paperino")
vettore.stringhe

# Definizione tramite funzioni ad hoc
vec1 <- seq(from = 3, to = 15, by = 3)
vec1
vec2 <- seq(from = 3, to = 15, length = 5)
vec2
vec1 == vec2

vec <- 1:5
vec

vec.ones <- rep(x = 1, times = 5)
vec.ones

vec.rep <- rep(c(1, 2), 2)
vec.rep

# Concatenare due vettori
contatenata = c(vettore.colonna, vettore.stringhe) 
contatenata
# i valori sono diventati tutti stringhe

# Selezionare un elemento del vettore
vettore.colonna[1]

which(vettore.stringhe == "pippo")
which(vettore.stringhe == "pippo" | vettore.stringhe == "paperino")

# Selezionare diversi elementi del vettore
vettore.colonna[c(6, 3)]
vettore.stringhe[2:4]

# Cancellare un valore dal vettore
vettore.colonna[-4]

# Accedere alla tipologia di vettore
class(vettore.colonna)
is.numeric(vettore.colonna)

class(vettore.stringhe)
is.numeric(vettore.stringhe)
is.character(vettore.stringhe)

# Aritmetica dei vettori
campione <- c (6, 1, 5, 9, 4, 7, 8, 2, 5, 8)
campione^2 # elevamento al quadrato
min(campione) # valore minimo
max(campione) # valore massimo
length(campione) # numero di valori (lunghezza del vettore)
sum(campione) # somma degli elementi
mean(campione) # media
median(campione) # mediana
range(campione) # restituisce un vettore di due elementi, il min e max
sd(campione) # deviazione standard
var(campione) # varianza
sum((campione - mean(campione))^2)/(length(campione)-1)
sort(campione) # mette in ordine crescente i valori del vettore
order(campione) # restituisce l’ordine dei valori nel vettore

# Operazioni tra vettori: il RECYCLING
# Quando si compiono operazioni tra due vettori, il primo elemento dell’uno
# viene operato con il primo elemento dell’altro, il secondo col secondo e
# così fino alla fine. Se un vettore è più corto dell’altro, quando termina
# viene riutilizzato dall’inizio.
a <- c (10,10,10,10,10,10,10,10,10,10)
b <- c (1,2)
a * b

rm(list = ls())

# 5.3 FATTORI
# vettore utilizzato per classificare o suddividere in livelli gli elementi
# di un altro vettore
PROV <- c("RM", "RM", "TO", "NA", "TO", "NA")
PROV <- factor(PROV)
PROV

# Accedere alla tipologia di vettore
is.factor(PROV)

rm(list = ls())

# 5.4 MATRICI
# sono vettori multidimensionali, le matrici hanno due dimensioni: righe e colonne.

# Definizione di una matrice
dati <- c(1,4,6,32,6,7,4,6,8,5,3,6,7,67,4)
n.righe <- 5
n.colonne <- 3

matrice <- matrix(data=dati, nrow=n.righe, ncol=n.colonne)
matrice
# oppure
matrice2 <- matrix(data=dati, nrow=n.righe, ncol=n.colonne, byrow=TRUE)
matrice2
# oppure
matrice3 <- array(data=dati, dim=c(n.righe,n.colonne))
matrice3

# Richiedere la dimensione dell’array o matrice
dim(matrice)

# Trasporre la matrice
t(matrice)

# Selezionare un sottoinsieme di elementi di un array o matrice
matrice[1,2] # estrazione dell’elemento sulla riga 1 e la colonna 2
matrice[5,] # estrazione della riga 5
matrice[,2] # estrazione della colonna 2
matrice[4:5,2]

# Operazioni su matrici
matrice.b <- c(1,3,5,2)
matrice.b[1] <- 100 
matrice.b

matrice.b - 1
matrice.b * 2
log(matrice.b)

matrice.c <- c(2,3,1,2)
matrice.b * matrice.c # prodotto elemento per elemento

inner.product <- t(matrice.b) %*% matrice.c # prodotto scalare b’c: moltiplico ogni
# elemento di b per il corrispondente di c e sommo; ottengo uno scalare
inner.product

outer.product <- matrice.b %*% t(matrice.c) # bc’: moltiplico ogni elemento di b
# per tutti gli elementi di t(c); ottengo una matrice 4x4
outer.product

matrix.d <- cbind(outer.product[,1],outer.product[,4]) # genera una matrice 4x2 
# selezionando le colonne 1 e 4 di outer.product e concatenandole per colonna
matrix.d
matrix.e <- rbind(outer.product[,1],outer.product[,4]) # stessa cosa con le righe
matrix.e

# Altre operazioni di algebra lineare
matrix.z <- matrix(c(3,4,6,7,1,12,4,9,21),3,3)
diag(matrix.z) # ritorna la diagonale
det(matrix.z) # calcola il determinante
solve(matrix.z) # ritorna la matrice inversa

rm(list = ls())

# 5.5 DATAFRAMES
# sono matrici bidimensionali dove ogni colonna può avere un tipo di dato
# diverso dalle altre

# Importiamo il data.frame da un file .txt, utilizzando il comando read.table().
help(read.table)

# Prima di importare i dati, guardare il file che viene importato!
vitamina <- read.table('vitaminaD.txt', header = TRUE)

head(vitamina)
dim(vitamina)
colnames(vitamina)
rownames(vitamina)

# 6 - ANALISI DESCRITTIVA DI UNA VARIABILE QUANTITATIVA -------

# Le variabili quantitative possono essere descritte numericamente,
# utilizzando opportuni indici di posizione e di dispersione
# e, graficamente, mediante istogrammi e boxplot.

# Analisi dei dati quantitativi contenuti nel file 'vitaminaD.txt'.
# Il dataset contiene 26 osservazioni di una variabile,
# che rappresenza la concentrazione della vitamina D in un campione di uomini sani.

# Fonte: Bland (1995) An introduction to Medical Statistics.
# Oxford University Press

# Media
mean(vitamina$Vitamina_D)

# Mediana
median(vitamina$Vitamina_D)

# Minimo
min(vitamina$Vitamina_D)

# Massimo
max(vitamina$Vitamina_D)

# Primo quartile Q1 (venticinquesimo percentile delle osservazioni)
Q1 <- quantile(vitamina$Vitamina_D, prob = 0.25)
Q1
names(Q1) <- NULL

# Terzo quartile Q3 (settantacinquesimo percentile delle osservazioni)
Q3 <- quantile(vitamina$Vitamina_D, prob = 0.75)
Q3
names(Q3) <- NULL

# Range interquartile
IQR <- Q3 - Q1
IQR

# Funzione summary: riassunto di min, max, quartili e media
summary(vitamina$Vitamina_D)

# Disegnare un istogramma dei dati
hist(vitamina$Vitamina_D) # In ordinata ci sono le frequenze assolute
hist(vitamina$Vitamina_D, prob = TRUE) # In ordinata ci sono le densità in modo che l'area totale sommi a 1.

# Parametri estetici del grafico
hist(
  vitamina$Vitamina_D,
  prob = TRUE,
  xlab = 'Concentrazione',
  ylab = 'Densità',
  main = 'Istogramma Vitamina D'
)

# R decide automaticamente il numero di classi in cui dividere i dati e la loro dimensione

# Possiamo imporre all'istogramma un certo numero di classi usando l'argomento breaks
# Non esiste un numero di classi 'giusto', la scelta sta alla sensibilità dello statistico
# Regola euristica: numero di classi circa uguale alla radice quadrata della dimensione del campione
par(mfrow = c(2, 2)) # Quattro grafici in una sola finestra
hist(
  vitamina$Vitamina_D,
  prob = TRUE,
  xlab = 'Concentrazione',
  ylab = 'Densità',
  main = 'Istogramma Vitamina D',
  breaks = 2
)
hist(
  vitamina$Vitamina_D,
  prob = TRUE,
  xlab = 'Concentrazione',
  ylab = 'Densità',
  main = 'Istogramma Vitamina D',
  breaks = 8
)
hist(
  vitamina$Vitamina_D,
  prob = TRUE,
  xlab = 'Concentrazione',
  ylab = 'Densità',
  main = 'Istogramma Vitamina D',
  breaks = 12
)
hist(
  vitamina$Vitamina_D,
  prob = TRUE,
  xlab = 'Concentrazione',
  ylab = 'Densità',
  main = 'Istogramma Vitamina D',
  breaks = 26
)

graphics.off()

# Costruire un boxplot dei dati
boxplot(vitamina$Vitamina_D)
# Linea nera: mediana
# Scatola: Primo e terzo quartile
# Baffo inferiore: massimo tra minimo e valore minimo osservato entro Q1-1.5*IQR
# Baffo superiore: minimo tra massimo e valore massimo osservato entro Q3+1.5*IQR
# Potenziali outlier: osservazioni fuori dal baffi

graphics.off()

# 7 - ANALISI DESCRITTIVA DI UNA VARIABILE CATEGORICA ---------

# Creo un vettore di realizzazioni di una variabile categorica
province <-  c("MI",
               "MI",
               "VA",
               "BG",
               "LO",
               "LO",
               "CR",
               "CR",
               "MI",
               "CR",
               "LO",
               "VA",
               "MI",
               "LO",
               "MI")
province

province <- factor(province)
province

levels(province) # modalità della variabile categoriale

# Tabella delle frequenze assolute
province.assolute <-  table(province)
province.assolute

# Calcoliamo la moda
province.assolute[province.assolute == max(province.assolute)]

# Tabella delle frequenze relative
province.relative <- table(province) / length(province)
province.relative
# in alternativa
province.relative <- prop.table(province.assolute)
province.relative

plot(province)
# Il comando plot, se applicato ad una variabile creata con 'factor' o 'as.factor',
# disegna il barplot corrispondente alla frequenza assoluta di ogni livello.

# Ulteriori parametri della funzione
plot(
  province,
  col = 'red',
  xlab = 'province',
  ylab = 'frequenze assolute',
  main = 'Grafico a barre Province'
)
# col = '...' serve per cambiare il colore
# xlab = '...' serve per cambiare la scritta sotto l'asse delle ascisse
# ylab = '...' serve per cambiare la scritta sotto l'asse delle ordinate
# main = '...' serve per dare un titolo al grafico

# grafico a barre con le frequenze relative
barplot(
  province.relative,
  col = 'blue',
  xlab = 'province',
  ylab = 'frequenze relative',
  main = 'Grafico a barre Province'
)

graphics.off()

pie(province.relative) # Grafico a torta semplice

pie(
  province.relative,
  # Grafico a torta personalizzato
  labels = c('BG', 'CR', 'LO', 'MI', 'VA'),
  radius = 1,
  col = c('red', 'orange', 'yellow', 'green', 'lightblue', 'violet'),
  main = 'Grafico a torta Province'
)

graphics.off()
