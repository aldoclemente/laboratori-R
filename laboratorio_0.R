
################ LABORATORIO 0 ################

## INTRODUZIONE A R ------------------------------------------------------------

# Riferimenti bibliografici per R:
# TITOLO: Introductory Statistics with R
# AUTORI: Dalgaard, P. (2008)
# EDITORE: Pearson
# available E-book at http://link.springer.com/book/10.1007/978-0-387-79054-1/page/1

# Riferimenti bibliografici per gli argomenti trattati nel corso: 
# TITOLO: Introduzione alla Statistica, Seconda Edizione,
# AUTORI:  Ross, S. M. (2014)
# EDITORE:  Maggioli Editore

# Per scaricare R:   http://www.r-project.org/
# disponibile per Windows, Mac e Linux
# Per maggiori informazioni su R:
# - dall'interfaccia R: 'aiuto' -> 'Guida Html'
#   oppure 'aiuto' -> 'sito CRAN'

# Ambiente di sviluppo integrato (IDE) per R: RStudio
# strumento per facilitare lo sviluppo software (interfaccia grafica, debugger etc.)
# Per scaricare RStudio: http://www.rstudio.com/products/rstudio/download/
# disponibile per Windows, Mac e Linux

# Argomenti trattati nel laboratorio 0:
# - Comandi base di R (scalari, vettori, matrici e relative operazioni)
# - L'oggetto data.frame, l'import di file di dati e loro esplorazione

# R e' un linguaggio interpretato; e' possibile:
# - scrivere il codice direttamente sulla console R e fare invio
# - (preferibile) scrivere il codice su uno script .R (come questo) 
#   e poi eseguirlo in console, una riga per volta oppure una selezione. 
#   Per eseguire il codice in console: ctrl + Enter.

# Es 
3
# Per aprire un nuovo script:
# - File -> Nuovo script
# - Archivio -> Nuovo Documento

# Commento: tutto quanto preceduto da '#' non viene letto dalla Console di R
# E' possibile dunque eseguire indistintamente nella Console comandi e commenti
# senza dover togliere questi ultimi

# Esempio
5
# 5

# MOLTO IMPORTANTE:  R deve avere una DIRECTORY DI LAVORO,
# ovvero una cartella dove di default verranno cercati o salvati i file 
# utilizzati da R.

# per selezionare la cartella di lavoro abbiamo due possibilita':

# 1.  seleziono la finestra della Console, e poi bottone 
#    'Session' -> 'set working directory' -> 'choose directory',

# 2. setwd('my_path') dove 'my_path' e' una stringa che contiene
#    il percorso della cartella di lavoro


# le seguenti due righe di codice righe permettono di selezionare come working directory
# la cartella in cui il 'Laboratorio_0.R' e' salvato.
if(!require(pacman)) install.packages("pacman")
pacman::p_load("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))

# per sapere tutti i file presenti nella directory:
dir()

# se non mi ricordo la directory di lavoro:
getwd()

## R COME CALCOLATORE -----------------------------------------------------------

# E' possibile utilizzare R per eseguire operazioni semplicissime

# Operatori matematici di base

(17*0.35)+(1/3)-(1/2)

# in R sono definite le principali funzioni matematiche
# (alcune serviranno spesso nell'analisi dei dati!)

log(10) # logaritmo in base naturale

log10(10) #logaritmo in base 10

exp(1)

3^(-1)


## OGGETTI IN R: ASSEGNARE VALORI A VARIABILI ----------------------------------

# operatore di assegnamento: =  
# funziona anche: <-

# 1. Scalari

a = 1
a
a <- 2
a

b = 3
b

a+b
a
b

a = b
a
b

# 2. Arrays (Vettori)

# c() e' la funzione che serve a concatenare: un array
# ? una lista di numeri ai quali si puo' accedere singolarmente (vedremo dopo)
v = c( 6, 1, -7, 12 )
v


# arrays costituiti da sequenze ordinate di numeri:
# e' possibile automatizzare la procedura

# sequenza di passo 1
u = 1:5
u

# posso imporre il passo
u = seq( 1, 5, by = 2 )
u

# passo negativo significa sequenza decrescente
u = seq( 5 , 1, by = -1 )
u

# oppure la lunghezza del vettore
u = seq( 0, 1, length = 10 )
u

length(u)

# vettori costituiti da ripetizioni di numeri:
# E' possibile automatizzare la procedura

w = rep( 1, 10 )
w

# Primo argomento di rep: valore o vettore di valori che voglio ripetere
# Secondo argomento di rep: valore o vettore di valori che indicano
# come il primo argomento va ripetuto

w1 = rep( c( 1, 2, 3, 4 ), 3 )
w1

# quale sara' la lunghezza di w1?
# ...

length(w1)

# usando l'opzione each
w2  = rep( 1:8, each = 3 )
w2


# 3. Matrici

W = matrix( data = c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 4, ncol = 3, byrow = FALSE)
W

# oppure
W = rbind( c(1,5,9), c(2,6,10), c(3,7,11), c(4,8,12) )
W

# oppure 
# costruisco la matrice Z le cui colonne sono i vettori v1, v2, v3
v1 = 1:4
v2 = 5:8
v3 = 9:12

Z = cbind( v1, v2, v3 )
Z

colnames(Z)
colnames(Z)= c("a","b","c")

Z

## ESTRAZIONE DI ELEMENTI DA UN ARRAY: -----------------------------------------

# Attenzione: in R i vettori non sono matrici n*1 o 1*n!
# fondamentale ricordarselo quando si vuole estrarre un elemento 
# da un vettore
# [<index_number>] per accedere direttamente ai singoli elementi di vettori e matrici
# R e' 1-indexed: il primo elemento di un array e' in posizione 1
# () contraddistingue le chiamate a funzioni, es: length()

v
v[2]
v[ 2:3 ]
v[ c( 1, 3 ) ]
v[ -1 ]         # tutto il vettore tranne il primo elemento
v[ -length(v) ] # tutto il vettore tranne l'ultimo elemento



# 1. La funzione which 
# restituisce GLI INDICI di tutti gli elementi che soddisfano la condizione logica 
# specificata
# Operatori logici:
# ==: uguale
# != : diverso
# > : maggiore
# < : minore
# & : and
# |: or

w4 = c(5,9,9,9,9,1,1)

which(w4==5 )  
which( w4 != 5 )  

which( w4 > 1 ) 

which( w4 > 1 & w4 <= 9)

which(w4 > 6 | w4 == 5)


# 2. Accesso diretto agli elementi 
# (non restituisce la posizione ma il valore del vettore):

w4[ w4 > 1 ]

w4[ w4 == 5 ]  ## cosa vi aspettate?


# 3. La funzione unique permette di determinare i valori univoci di un vettore.
w4
unique(w4)
# e se volessimo sapere quanti elementi univoci ci sono in un vettore ?

length( unique( w4 ) ) 


# 4. condizione logica sull'intero vettore
w4 > 1
w4 ==7
# restituisce un vettore di valori TRUE o FALSE



# ESTRAZIONE DI ELEMENTI DA UNA MATRICE: ---------------------------------------

W
dim(W) ## differenza tra length e dim

# Accesso diretto a righe e colonne:
W[ 2, 3 ]

W[ 2:4, 1 ]

W[ 4, c( 1, 3 ) ]

# intere colonne o righe
W[ 3, ]

W[ , 2 ]

# estrazione di sottomatrici
W[ c( 1, 3, 4 ) , 2:3 ]



## OPERAZIONI ALGEBRICHE IN R --------------------------------------------------

# NB: R di default effettua le operazioni componente per componente

# introduciamo un nuovo scalare
a = 5
a

# 1. Operazioni tra scalari e vettori

a + 2 * a   # scalare + scalare
a + w4      # scalare + vettore
a * w4      # scalare * vettore
w4 + 2 * w4 # vettore + (scalare * vettore)
w4^2        # attenzione: operazioni sono sempre componente per componente
exp(w4)     # vedi sopra


# 2. Operazioni tra matrici

W 
Z

# # matrice + matrice (componente per componente), CON STESSE DIM
Z + W 

# # si pu? moltiplicare W * Z ?
# # Non ? il classico prodotto matriciale, ma anche qui compon * compon
P = Z * W 
P
# 
# # R fa sempre il prodotto componente per componente, esattamente
# # come per i vettori! 
# 
# ## funzioni che permettono operazioni algebriche in R
#
w4
sum(w4)   # somma componenti vettore w4
W
sum(W)    # somma componenti matrice W (somma tutto!!)
# 
prod(w4)  # prodotto componenti vettore w4
prod(W)   # prodotto componenti matrice W (come sopra)

V = t(W) # trasposizione di matrice: V e una matrice 3x4
V


# # matrice * matrice (componente per componente)
V * W 

# # Errore: le matrici hanno dimensioni diverse!
dim(V)
dim(W)

# # Vera Moltiplicazione matriciale: anche qui bisogna fare 
# # attenzione alle dimensioni..

V %*% W
W %*% V 


# Per visualizzare e cancellare le variabili

ls()              # fornisce la lista delle variabili esistenti
rm(a)             # rimuove la variabile a
ls()
rm( list = ls() ) # rimuove tutte le variabili nel workspace
# ctrl + l pulisce la console
ls()


## LA STRUTTURA DATI DATA.FRAME ------------------------------------------------

# I dataframe sono oggetti costituiti da vettori di uguale lunghezza, ma
# non necessariamente dello stesso tipo. Contrariamente alle matrici, 
# il dataframe e' piu' flessibile  e puo' essere utilizzato 
# con dati di tipo diverso tra loro. 
# Comunemente usato per analisi statistiche

# N.B. sembrano matrici ma non lo sono; infatti, i vettori in essi contenuti, 
# se presi per colonna, per R hanno significato di "variabili statistiche" 
# (posso associare dei nomi!)

esame = data.frame(
  matricola = as.character( c( 45020, 45679, 46789, 43126, 42345, 47568 ) ),
  voti_S = c( 30, 19, 29, NA, 25, 26 ), 
  voti_O = c( 3, 3, 1, NA, 3, 2 ), 
  voti_TOT = c( 30, 22, 30, NA, 28, 28 ) )
esame

# ( NA = Not Available)
# purtroppo non e' possibile accedere agli elementi di un dataframe in modo 
#diretto. E' necessario utilizzare il comando $ (dollaro) subito dopo 
#il nome del dataframe, seguito dal nome della variabile che ci interessa
# Questo e' uno dei due modi per evitare che R ignori il contenuto 
#di un dataframe e per fare in modo di poter accedere al contenuto stesso. 

voti_S # guardare l'errore
esame$voti_S
