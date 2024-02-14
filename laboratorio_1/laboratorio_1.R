#'###########################################################'#
#'########               LABORATORIO 1               ########'#
#'######## INTRODUZIONE A R & STATISTICA DESCRITTIVA ########'#
#'###########################################################'#

# Argomenti del primo laboratorio:
# 1 - Primi passi: variabili, vettori, matrici 
# 2 - Dataframe 
# 3 - Analisi descrittiva di una variabile categorica
# 4 - Analisi descrittiva di una variabile numerica

# IMPOSTARE LA WORKING DIRECTORY (CARTELLA DI LAVORO): -------------------------
# Da interfaccia:
# 'Session' -> 'Set Working Directory' -> 'Choose Directory' -> ...

# Da console:
# setwd( 'C:/percorso/file' )

# Da pacchetto:
if(!require(pacman)) install.packages("pacman")
pacman::p_load("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

# CONOSCERE LA WORKING DIRECTORY (CARTELLA DI LAVORO):
getwd()

# CONOSCERE I FILES PRESENTI NELLA DIRECTORY:
dir()

# HELP 
# help( nome_comando )

help( getwd )

?getwd

# 1 - PRIMI PASSI ------------------------------------------------------------------
(17*0.35)+(1/2)-exp(1)+log(3)-2^2

### OGGETTI IN R: ASSEGNARE VALORI A VARIABILI ---------------------------------
# 1. Scalari
a = 1

b = 3

a + b

a = b

# 2. Vettori
v = c( 6, 1, -7, 12 )
v

length(v)

# 3. Matrici
M = matrix( data = c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ),
            nrow = 4, ncol = 3, byrow = FALSE)
M

dim( M )

### ESTRAZIONE DI ELEMENTI DA UN VETTORE ---------------------------------------

# R è 1-indexed: il primo elemento di un vettore è in posizione 1
# L'operatore () contraddistingue le chiamate a funzioni (es.: length())
# L'operatore [] contraddistingue gli accessi a vettori/matrici

# 1. Accesso diretto agli elementi
v

v[ 2 ]          # secondo elemento di v

v[ c( 1, 3 ) ]  # primo e terzo elemento di v

v[ -1 ]         # tutto il vettore v tranne il primo elemento

# 2. which()
# La funzione which(condizione) restituisce gli INDICI di tutti gli elementi che
# soddisfano la condizione logica specificata
w = c( 5, 9, 9, 9, 9, 1, 1 )

which( w == 5 )

which( w != 5 )  

which( w > 1 ) 

which( w > 1 & w <= 5 )

which( w > 6 | w == 7 )

# 3. unique()
# La funzione unique(vettore) permette di determinare i valori univoci di un
# vettore
w
unique( w )

# E se volessimo sapere quanti elementi unici ci sono in un vettore?
length( unique( w ) ) 

# 4. Condizione logica sull'intero vettore
w > 1

w == 7

### ESTRAZIONE DI ELEMENTI DA UNA MATRICE --------------------------------------

M

# Accesso diretto ad elementi
M[ 2, 3 ]

M[ 4, c( 1, 3 ) ]

# Accesso ad intere colonne o righe
M[ 3, ]

M[ , 2 ]

# Visualizzazione e rimozione delle variabili
ls()              # fornisce la lista delle variabili esistenti

rm( a )           # rimuove la variabile a

ls()

rm( list = ls() ) # rimuove tutte le variabili nel workspace

# Pulizia della console
# Windows: ctrl + l
# macOS: Options + Command + l

ls()
# 2 - DATAFRAME ----------------------------------------------------------------

### LA STRUTTURA DATI DATA.FRAME -----------------------------------------------
# I dataframe sono oggetti costituiti da vettori di uguale lunghezza, ma non
# necessariamente dello stesso tipo. Contrariamente alle matrici, il dataframe
# è più flessibile e può essere utilizzato con dati di tipo diverso tra loro

# Importazione di un nuovo dataframe
# Prima si deve localizzare dove si trova il file da importare
# setwd("C:/percorso/cartella/dove/si/trova/il/dataset")

pazienti = read.table( "pazienti.txt", header = TRUE, stringsAsFactors = FALSE)
# header = TRUE: il file importato contiene i nomi delle variabili nella prima riga
# stringAsFactors = FALSE: le variabili character come stringhe (non come factor)

names( pazienti ) # mostra i nomi delle variabili nel dataframe 

head( pazienti )  # mostra le prime sei righe del dataframe

tail( pazienti )  # mostra le ultime sei righe del dataframe

pazienti

# Accedere alle variabili del dataframe
pazienti$Peso

# Dati mancanti
is.na( pazienti ) 

sum( is.na( pazienti ) )

# Che tipo di variabili ci sono nel nostro dataframe?
# Quali sono variabili categoriche e quali numeriche?

str( pazienti )

class( pazienti )

class( pazienti$Sesso )

# Creazione di una variabile categorica tramite factor
pazienti$Sesso = as.factor( pazienti$Sesso )

class( pazienti$Sesso )

levels( pazienti$Sesso ) # livelli/categorie della variabile qualitativa

# VARIABILI CATEGORICHE 
# Selezionare le femmine
femmine = pazienti[ which( pazienti$Sesso == 'F' ) , ]

dim( femmine )

# Come calcolare il numero di femmine senza creare un nuovo dataframe? 
length( which( pazienti$Sesso == 'F' ) )
dim( femmine )[1]

# Tabella di frequenza (per variabile categorica/factor)
table( pazienti$Sesso )

table( pazienti$Peso ) # non molto leggibile (variabile numerica)!

# 3 - ANALISI DESCRITTIVA DI UNA VARIABILE QUALITATIVA (DATI CATEGORICI) -------

# Analisi di variabili qualitative:
# 1. Tabella di distribuzione di frequenze per le categorie della variabile
# 2. Grafici (diagrammi a barre e a torta)

### RICHIESTA 1: da quali province provengono gli pazienti?

# factor() converte l'argomento in realizzazioni di una variabile categorica,
# i cui valori possibili sono riportati in levels
pazienti$Provincia = as.factor( pazienti$Provincia )

# Specificare i levels non è necessario, ma permette di ordinare i levels 
# come si vuole (default: ordine alfabetico)
levels( pazienti$Provincia )

# Tabella delle frequenze assolute
freq_ass = table( pazienti$Provincia )
freq_ass

# Tabella delle frequenze relative
freq_rel = table( pazienti$Provincia ) / length( pazienti$Provincia )   # oppure: prop.table( freq_ass )
freq_rel

sum( freq_rel )

### RICHIESTA 2: rappresentare graficamente le frequenze assolute e relative

# Primo grafico
dev.new()
par( mfrow = c( 1, 2 ) )
barplot( table( pazienti$Provincia ),
         col = rainbow( length( levels( as.factor( pazienti$Provincia ) ) ) ),
         main = 'Frequenze assolute:\nPROVINCE' )
barplot( table( pazienti$Provincia ) / sum( table( pazienti$Provincia ) ),
         col = rainbow( length( levels( as.factor( pazienti$Provincia ) ) ) ),
         main = 'Frequenze relative:\nPROVINCE' )

# Secondo grafico
dev.new()
par( mfrow = c( 1, 2 ) )
barplot( table( pazienti$Esito ), col = c( 'red', 'green' ),
         main = 'Frequenze assolute:\nLIVELLO COLESTEROLO' )
barplot( table( pazienti$Esito ) / sum( table( pazienti$Esito ) ), col = c( 'red', 'green' ),
         main = 'Frequenze relative:\nLIVELLO COLESTEROLO' )

# Terzo grafico
dev.new()
pie( table( pazienti$Provincia ) / sum( table( pazienti$Provincia ) ),
     col = rainbow( length( levels( as.factor( pazienti$Provincia ) ) ) ),
     main = 'Frequenze relative:\nPROVINCE' )

# Quarto grafico
dev.new()
pie ( table( pazienti$Esito ) / sum( table( pazienti$Esito ) ), col = c( 'red', 'green' ),
      main = 'Frequenze relative:\nLIVELLO COLESTEROLO' )


# 4 - ANALISI DESCRITTIVA DI UNA VARIABILE QUANTITATIVA ------------------------
# Le variabili quantitative possono essere descritte numericamente,
# utilizzando opportuni indici di posizione e di dispersione
# e, graficamente, mediante istogrammi e boxplot.

### RICHIESTA 1: calcolare i principali indici statistici di posizione e di
### dispersione della variabile "Peso"

N = dim( pazienti )[1]

# Media campionaria dei dati: media = sum_i(x_i) / N
media = mean( pazienti$Peso )
media
# oppure:
media = sum( pazienti$Peso ) / N
media

# Varianza campionaria dei dati: varianza = sum_i(x_i - media)^2 / (N-1)
varianza = var( pazienti$Peso )
varianza
# oppure:
varianza = sum( ( pazienti$Peso - media )^2 ) / ( N - 1 )
varianza

# Deviazione standard campionaria: dev_std = sqrt( varianza )
dev_std = sd( pazienti$Peso ) 
dev_std
# oppure:
dev_std = sqrt(varianza)
dev_std

# Range
r = range( pazienti$Peso )
r
# oppure:
r = c( min( pazienti$Peso ), max( pazienti$Peso ) )
r

# Differenza tra massimo e minimo elemento
diff(r)
# oppure:
r[2]-r[1]

# Mediana: valore centrale del dataset
# Dopo aver ordinato i dati:
#   - se N è DISPARI, mediana = (N+1)/2 -esima osservazione
#   - se N è PARI,    mediana = media tra N/2 -esima osservazione e
#                               (N/2 + 1) -esima osservazione
median( pazienti$Peso )

# Moda: valore che si verifica con maggiore frequenza nei dati
# Nota: non per forza questo valore è unico
freq_ass = table( pazienti$Peso )
freq_ass

max_freq_ass = max( freq_ass )
max_freq_ass

moda = freq_ass[ freq_ass == max_freq_ass ]
moda

# Alternativamente, sarebbe più appropriato dividere i valori della variabile
# "Peso" in intervalli, associando ogni intervallo ad una classe, per poi
# identificare la classe modale (con frequenza maggiore)

# Quantili e Quartili
# Dopo aver ordinato i dati:
#   - Primo quartile   (Q1)  :  x tale per cui il 25% dei dati è minore di x
#   - Secondo quartile (Q2)  :  x tale per cui il 50% dei dati è minore di x
#   - Terzo quartile   (Q3)  :  x tale per cui il 75% dei dati è minore di x
# Nota: Q2 = mediana
quantile( pazienti$Peso, probs = 0.25 ) # Q1

quantile( pazienti$Peso, probs = 0.50 ) # Q2
median( pazienti$Peso )

quantile( pazienti$Peso, probs = 0.75 ) # Q3

# Range interquartile: IQR = Q3 - Q1
IQR = quantile( pazienti$Peso, probs = 0.75 ) - quantile( pazienti$Peso, probs = 0.25 )
# oppure:
IQR( pazienti$Peso )

# Quartili (riassunto)
quantile( pazienti$Peso )

# Quantile di ordine p: x tale per cui il 100p% dei dati è minore di x
p = 0.85
quantile( pazienti$Peso, probs = p )

# Quantili di ordine 0%, 10%, 20%, ..., 80%, 90%, 100%
p =  (0:10)  / 10
quantile( pazienti$Peso, probs = p )

# Valori di sintesi (media, minimo, massimo e tre quartili)
summary( pazienti$Peso )

### RICHIESTA 2: visualizzare graficamente la distribuzione della variabile
### "Colesterolo"

# 2.1 Istogramma

# Istruzioni:
#   - dividere il range dei dati in classi (intervalli)
#   - calcolare, per ogni classe, le frequenze relative e densità
#      [ densità = ( frequenza relativa ) / ( ampiezza intervallo classe ) ]
#   - in corrispondenza di ogni classe disegnare rettangoli di area pari alla
#      frequenza relativa della classe considerata (ovvero di altezza pari alla
#      densità)

# Istogramma con ordinata = frequenze assolute
dev.new()
hist( pazienti$Colesterolo,
      xlab = 'Colesterolo', ylab = 'Frequenze assolute',
      main = 'Istogramma:\nCOLESTEROLO' )

# Istogramma con ordinata = densità (freq_rel = densità * ampiezza intervallo classe ) 
dev.new()
hist( pazienti$Colesterolo, prob = TRUE,
      xlab = 'Colesterolo', ylab = 'Densità',
      main = 'Istogramma:\nCOLESTEROLO' )

# Istogramma con breaks = 7
dev.new()
par( mfrow = c( 1, 2 ) )
hist( pazienti$Colesterolo, prob = TRUE, breaks = 7,
      xlab = 'Colesterolo', ylab = 'Densità',
      main = 'Istogramma (7 classi):\nCOLESTEROLO' )

# Istogramma con breaks = 30
hist( pazienti$Colesterolo, prob = TRUE, breaks = 30,
      xlab = 'Colesterolo', ylab = 'Densità',
      main = 'Istogramma (30 classi):\nCOLESTEROLO' )

# Possibili scelte del numero di classi:
#   - breaks = sqrt(N)
#   - breaks = ceiling( 1 + log2( N ) )
#   - breaks = scelta manuale dell'utente

# Tabella di distribuzione di frequenze
istogramma = hist( pazienti$Colesterolo, plot = FALSE )

# La funzione restituisce un oggetto con i seguenti attributi:
# breaks: estremi delle classi
# counts: frequenze assolute delle classi
# density: densità associate alle classi
# mids: valori centrali delle classi
# xname: nome della variabile
# equidist: TRUE se le classi hanno tutte la stessa ampiezza; FALSE altrimenti

estremi_classi = istogramma$breaks
estremi_classi

freq_ass = istogramma$counts
freq_ass

N = sum( freq_ass )   # oppure: N = length( pazienti$Colesterolo )
N

freq_rel = ( freq_ass ) / N
freq_rel

density = istogramma$density
density

# Dato che densità = freq_rel/(ampiezza intervallo classe), segue che:
freq_rel2 = density * diff( estremi_classi )
freq_rel
freq_rel2

# La distribuzione del colesterolo è simmetrica?
Q1 = quantile( pazienti$Colesterolo, probs = 0.25, type = 2 ) # Q1
Q2 = quantile( pazienti$Colesterolo, probs = 0.50, type = 2 ) # Q2
Q3 = quantile( pazienti$Colesterolo, probs = 0.75, type = 2 ) # Q3

dev.new()
hist( pazienti$Colesterolo, prob = TRUE,
      xlab = 'Colesterolo', ylab = 'Densità',
      main = 'Istogramma:\nCOLESTEROLO' )
abline( v = Q1, col = 'blue3', lwd = 2, lty = 2 )
abline( v = Q2, col = 'red3', lwd = 2 )
abline( v = mean( pazienti$Colesterolo ), col = 'green3', lwd = 2 )
abline( v = Q3, col = 'blue3', lwd = 2, lty = 2 )
legend( 'topright', legend = c( 'Q1', 'Mediana [Q2]', 'Media', 'Q3' ),
        col = c( 'blue3', 'red3', 'green3', 'blue3' ),
        lwd = c( 2, 2, 2, 2 ), lty = c( 2, 1, 1, 2 ) )

# Nota: dall'istogramma non si rilevano particolari asimmetrie

# 2.2 Box plot
#   - eventuali asimmetrie della distribuzione
#   - presenza di eventuali valori estremi (outliers).

# Istruzioni:
#   - costruire un rettangolo con basi inferiore e superiore uguali,
#     rispettivamente, al primo e al terzo quartile (il rettangolo contiene
#     il 50% centrale delle osservazioni)
#   - all'interno del rettangolo tracciare una linea in corrispondenza
#     della mediana
#   - tracciare il primo baffo del box plot collegando la base inferiore
#     del rettangolo all'osservazione più bassa maggiore di lim_inf = Q1-1.5*IQR
#   - tracciare il secondo baffo del box plot collegando la base superiore
#     del rettangolo all'osservazione più alta minore di lim_sup = Q3+1.5*IQR
#   - segnalare la presenza di eventuali outliers (osservazioni fuori dal range
#   - definito da lim_inf e lim_sup).

# Box plot
dev.new()
boxplot( pazienti$Colesterolo, ylab = 'Colesterolo', col = 'lightblue',
         main = 'Box plot:\nCOLESTEROLO' )

# Box plot vs. Istogramma: COLESTEROLO
dev.new()
par( mfrow = c( 2, 1 ) )
boxplot( pazienti$Colesterolo, xlab = 'Colesterolo', col = 'lightblue',
         main = 'Box plot:\nCOLESTEROLO', horizontal = TRUE, ylim = c( 165, 230 ) )
abline( v = Q1, col = 'blue3', lwd = 2, lty = 2 )
abline( v = Q2, col = 'red3', lwd = 2 )
abline( v = mean( pazienti$Colesterolo ), col = 'green3', lwd = 2 )
abline( v = Q3, col = 'blue3', lwd = 2, lty = 2 )

hist( pazienti$Colesterolo, prob = TRUE,
      xlab = 'Colesterolo', ylab = 'Densità',
      main = 'Istogramma:\nCOLESTEROLO' )
abline( v = Q1, col = 'blue3', lwd = 2, lty = 2 )
abline( v = Q2, col = 'red3', lwd = 2 )
abline( v = mean( pazienti$Colesterolo ), col = 'green3', lwd = 2 )
abline( v = Q3, col = 'blue3', lwd = 2, lty = 2 )

# Box plot vs. Istogramma: GLICEMIA
Q1 = quantile( pazienti$Glicemia, probs = 0.25, type = 2 ) # Q1
Q2 = quantile( pazienti$Glicemia, probs = 0.50, type = 2 ) # Q2
Q3 = quantile( pazienti$Glicemia, probs = 0.75, type = 2 ) # Q3

dev.new()
par( mfrow = c( 2, 1 ) )
boxplot( pazienti$Glicemia, xlab = 'Glicemia', col = 'lightblue',
         main = 'Box plot:\nGLICEMIA', horizontal = TRUE, ylim = c( 60, 320 ) )
abline( v = Q1, col = 'blue3', lwd = 2, lty = 2 )
abline( v = Q2, col = 'red3', lwd = 2 )
abline( v = mean( pazienti$Glicemia ), col = 'green3', lwd = 2 )
abline( v = Q3, col = 'blue3', lwd = 2, lty = 2 )

hist( pazienti$Glicemia, prob = TRUE,
      xlab = 'Glicemia', ylab = 'Densità',
      main = 'Istogramma:\nGLICEMIA' )
abline( v = Q1, col = 'blue3', lwd = 2, lty = 2 )
abline( v = Q2, col = 'red3', lwd = 2 )
abline( v = mean( pazienti$Glicemia ), col = 'green3', lwd = 2 )
abline( v = Q3, col = 'blue3', lwd = 2, lty = 2 )

# La distribuzione della glicemia nei pazienti risulta caratterizzata da
# asimmetria positiva, con media superiore alla mediana.