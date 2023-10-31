#########################################
############  LABORATORIO 2  ############ 
#########################################

### ARGOMENTI:
### 1. STATISTICA DESCRITTIVA PER VARIABILI NUMERICHE UNIVARIATE
### 2. STATISTICA DESCRITTIVA PER VARIABILI NUMERICHE BIVARIATE

### IMPOSTARE LA WORKING DIRECTORY (CARTELLA DI LAVORO):
# Da interfaccia:
# 'Session' -> 'Set Working Directory' -> 'Choose Directory' -> ...
#
# Da console:
# setwd( 'C:/percorso/file' )

### CONOSCERE LA WORKING DIRECTORY (CARTELLA DI LAVORO):
getwd()

### CONOSCERE I FILES PRESENTI NELLA DIRECTORY:
dir()

########################################
###  VARIABILI NUMERICHE UNIVARIATE  ### 
########################################

### ANALISI DI UNA VARIABILE QUANTITATIVA (DATI NUMERICI) ----------------------

# Analisi di variabili quantitative:
# 1. Scatterplot
# 2. Indici statistici di posizione/dispersione
# 3. Istogramma
# 4. Box plot

### ESERCIZIO 1 ----------------------------------------------------------------

studenti = read.table( 'studenti.txt', header = TRUE )

head( studenti )

dim( studenti )

names( studenti )

# Selezionare le femmine
femmine = studenti[ which( studenti$Sesso == 'F' ) , ]
dim( femmine )

# Selezionare i maschi
maschi = studenti[ which( studenti$Sesso == 'M' ) , ]
dim(maschi)

# Scatterplot
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
plot( studenti$Peso, studenti$Colesterolo, 
      xlab = 'Peso [lbs]', ylab = 'Colesterolo [mg/l]', pch = 19,
      main = 'Scatterplot:\nColesterolo vs. Peso', col = 'royalblue')
points( femmine$Peso, femmine$Colesterolo, col = 'magenta', pch = 19 )
legend( 'bottomright', inset = 0.02, legend = c( 'Femmine', 'Maschi' ),
        col=c( 'magenta', 'royalblue' ), pch = c( 19, 19 ), bg = 'white' )

# Zoom dello scatterplot sui dati relativi alle femmine
min_p = min( femmine$Peso )
max_p = max( femmine$Peso )

min_c = min( femmine$Colesterolo )
max_c = max( femmine$Colesterolo )

#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
plot( studenti$Peso, studenti$Colesterolo, 
      xlab = 'Peso [lbs]', ylab = 'Colesterolo [mg/l]', pch = 19,
      main = 'Scatterplot (zoom):\nColesterolo vs. Peso', col='royalblue',
      xlim = c( min_p, max_p ), ylim = c( min_c, max_c) )
points( femmine$Peso, femmine$Colesterolo, col = 'magenta', pch = 19 )
legend( 'bottomright', inset = 0.02, legend = c( 'Femmine', 'Maschi' ),
        col=c( 'magenta', 'royalblue' ), pch = c( 19, 19 ), bg = 'white' )

# Tabella delle frequenze assolute per genere
freq_ass = table( studenti$Sesso )
freq_ass

# Tabella delle frequenze relative per genere
freq_rel = freq_ass / length( studenti$Sesso )  # oppure: prop.table(freq_ass)
freq_rel

# Grafico a barre delle frequenze assolute e relative
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
par( mfrow = c( 1, 2 ) )
barplot( freq_ass, col = c( 'pink', 'lightblue' ), ylab = 'Frequenze assolute' )
barplot( freq_rel, col = c( 'pink', 'lightblue' ), ylab = 'Frequenze relative' )

### RICHIESTA 1: calcolare i principali indici statistici di posizione e di
### dispersione della variabile "Peso"

N = dim( studenti )[1]

# Media campionaria dei dati: media = sum_i(x_i) / N
media = mean( studenti$Peso )
media
# oppure:
media = sum( studenti$Peso ) / N
media

# Varianza campionaria dei dati: varianza = sum_i(x_i - media)^2 / (N-1)
varianza = var( studenti$Peso )
varianza
# oppure:
varianza = sum( ( studenti$Peso - media )^2 ) / ( N - 1 )
varianza

# Deviazione standard campionaria: dev_std = sqrt( varianza )
dev_std = sd( studenti$Peso ) 
dev_std
# oppure:
dev_std = sqrt(varianza)
dev_std

# Range
r = range( studenti$Peso )
r
# oppure:
r = c( min( studenti$Peso ), max( studenti$Peso ) )
r

# Differenza tra massimo e minimo elemento
diff(r)
# oppure:
r[2]-r[1]

# Nota: sia v un vettore, allora il comando diff(v) genera un altro vettore, il
# cui elemento i-esimo è pari a v[i+1] - v[i] per i = 1, ..., length(v) - 1
# Esempio:
v = c( 1, 2, 8, -8 )
diff(v)

# Mediana: valore centrale del dataset
# Dopo aver ordinato i dati:
#   - se N è DISPARI, mediana = (N+1)/2 -esima osservazione
#   - se N è PARI,    mediana = media tra N/2 -esima osservazione e
#                               (N/2 + 1) -esima osservazione
median( studenti$Peso )

# Moda: valore che si verifica con maggiore frequenza nei dati
# Nota: non per forza questo valore è unico
freq_ass = table( studenti$Peso )
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
#   - Primo quartile (Q1):   x tale per cui il 25% dei dati è minore di x
#   - Secondo quartile (Q2): x tale per cui il 50% dei dati è minore di x
#   - Terzo quartile (Q3):   x tale per cui il 75% dei dati è minore di x
# Nota: Q2 = mediana
quantile( studenti$Peso, probs = 0.25 ) # Q1

quantile( studenti$Peso, probs = 0.50 ) # Q2
median( studenti$Peso )

quantile( studenti$Peso, probs = 0.75 ) # Q3

# Range interquartile: IQR = Q3 - Q1
quantile( studenti$Peso, probs = 0.75 ) - quantile( studenti$Peso, probs = 0.25 )
# oppure:
IQR( studenti$Peso )

# Quartili (riassunto)
quantile( studenti$Peso )

# Quantile di ordine p: x tale per cui il 100p% dei dati è minore di x
p = 0.85
quantile( studenti$Peso, probs = p )

# Quantili di ordine 0%, 10%, 20%, ..., 80%, 90%, 100%
p = ( 0:10 ) / 10
quantile( studenti$Peso, probs = p )

# Valori di sintesi (media, minimo, massimo e tre quartili)
summary( studenti$Peso )

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
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
hist( studenti$Colesterolo,
      xlab = 'Colesterolo', ylab = 'Frequenze assolute',
      main = 'Istogramma:\nCOLESTEROLO' )
abline( v = median( studenti$Colesterolo ), col = 'red' )
abline( v = mean( studenti$Colesterolo ), col = 'green' )

# Nota: dall'istogramma non si rilevano particolari asimmetrie 

# Istogramma con ordinata = densità
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
hist( studenti$Colesterolo, prob = TRUE, col = 'orange',
      xlab = 'Colesterolo', ylab = 'Densità',
      main = 'Istogramma:\nCOLESTEROLO' )

# Istogramma con breaks = "Sturges" (valore di default)
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
hist( studenti$Colesterolo, prob = TRUE, col = 'orange', breaks = "Sturges",
      xlab = 'Colesterolo', ylab = 'Densità',
      main = 'Istogramma:\nCOLESTEROLO' )

# Istogramma con breaks = 7
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
hist( studenti$Colesterolo, prob = TRUE, col = 'orange', breaks = 7,
      xlab = 'Colesterolo', ylab = 'Densità',
      main = 'Istogramma (7 classi):\nCOLESTEROLO' )

# Istogramma con breaks definito manualmente dall'utente
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
hist( studenti$Colesterolo, prob = TRUE, col = 'orange',
      main = 'Istogramma (classi scelte manualmente):\nCOLESTEROLO',
      xlab = 'Colesterolo', ylab = 'Densità',
      breaks = c( 160, 180, 187, 190, 198, 204, 210, 230 ) )

# Possibile scelte del numero di classi:
#   - breaks = sqrt(N)
#   - breaks = ceiling( 1 + log2( N ) )
#   - breaks = scelta manuale dell'utente

# Tabella di distribuzione di frequenze
istogramma = hist( studenti$Colesterolo, plot = FALSE )

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

N = sum( freq_ass )   # oppure: N = length( studenti$Colesterolo )
N

freq_rel = ( freq_ass ) / N
freq_rel

density = istogramma$density
density

# Dato che densità = freq_rel/(ampiezza intervallo classe), segue che:
freq_rel2 = density * diff( estremi_classi )
freq_rel
freq_rel2

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

#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
boxplot( studenti$Colesterolo, ylab = 'Colesterolo', col = 'forestgreen',
         main = 'Box plot:\nCOLESTEROLO' )

#########################################
####  VARIABILI NUMERICHE BIVARIATE  #### 
#########################################

### ANALISI DI UNA VARIABILE QUANTITATIVA (DATI NUMERICI) ----------------------
# Analisi di variabili quantitative:
# 1. Indici statistici di posizione/dispersione
# 2. Confronto tra istogrammi
# 3. Confronto tra box plot

### ESERCIZIO 2 ----------------------------------------------------------------
rm ( list = ls( ) )

studenti = read.table( 'studenti.txt', header = TRUE )

### RICHIESTA 1: calcolare i principali indici statistici di posizione e di
### dispersione della variabile "Peso" separatamente per ogni sottocampione
### (maschi e femmine)

# -> tapply( x, y, f )
# x: variabile numerica
# y: variabile categorica
# f: funzione

# Media campionaria
tapply( studenti$Peso, studenti$Sesso, mean )  

# Varianza campionaria
tapply( studenti$Peso, studenti$Sesso, var )   

# Deviazione standard campionaria
tapply( studenti$Peso, studenti$Sesso, sd )

# Minimo
tapply( studenti$Peso, studenti$Sesso, min )   

# Massimo
tapply( studenti$Peso, studenti$Sesso, max )  

# Range
diff( tapply( studenti$Peso, studenti$Sesso, range )$F ) 
diff( tapply( studenti$Peso, studenti$Sesso, range )$M )

# Mediana
tapply( studenti$Peso, studenti$Sesso, median )

# Quartili
Q = tapply( studenti$Peso, studenti$Sesso, quantile )
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
tapply( studenti$Peso, studenti$Sesso, quantile, probs = 0.9 )

# Sintesi
tapply( studenti$Peso, studenti$Sesso, summary )

### RICHIESTA 2: visualizzare graficamente la distribuzione della variabile
### "Peso" separatamente per ogni sottocampione (maschi e femmine)

femmine = studenti[ studenti$Sesso == 'F', ]
maschi = studenti[ studenti$Sesso == 'M', ]

# Istogrammi affiancati
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
par ( mfrow = c( 2, 1 ) )
hist( femmine$Peso, prob = TRUE, col = 'pink',
      breaks = seq( min( studenti$Peso ), max( studenti$Peso ), length = 10 ), 
      xlim = range( studenti$Peso ), ylim = c( 0, 0.025 ),
      main = 'Istogramma: PESO FEMMINE', xlab = 'Peso [lbs]', ylab = 'Densità' )
hist( maschi$Peso, prob = TRUE, col = 'royalblue',
      breaks = seq( min( studenti$Peso ), max( studenti$Peso ), length = 10 ),
      xlim = range( studenti$Peso ), ylim = c( 0, 0.025 ),
      main = 'Istogramma: PESO MASCHI', xlab = 'Peso [lbs]', ylab = 'Densità' )

# Nota: il confronto sensato se i grafici hanno le stesse scale sugli assi e le
# stesse classi!

# Istogrammi sovrapposti (con opzione add = TRUE nel secondo grafico)
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
hist( femmine$Peso, prob = TRUE, col = 'pink',
      breaks = seq( min( studenti$Peso ), max( studenti$Peso ), length = 10 ),
      xlim = range( studenti$Peso ), ylim =c( 0, 0.025 ), angle = 45,
      density = 14, main = 'Istogramma: PESO',
      xlab = 'Peso [lbs]', ylab = 'Densità' )
hist( studenti$Peso[ studenti$Sesso == 'M' ], prob = TRUE, col = 'royalblue',
      breaks = seq( min( studenti$Peso ), max( studenti$Peso ), length = 10 ),
      xlim = range( studenti$Peso ), ylim =c( 0, 0.025 ), angle = 135,
      density = 14, add = TRUE )
legend( 'topright', legend = c( 'Femmine', 'Maschi' ),
        fill = c( 'pink', 'royalblue' ) )

# Box plot distinti per classi
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
boxplot( studenti$Peso ~ studenti$Sesso, col = c ( 'pink', 'royalblue' ),
         xlab = 'Sesso', ylab = 'Peso [lbs]', names = c( 'Femmine', 'Maschi' ),
         main = 'Box plot: PESO')

# Nota: confrontare sempre box plot con stesse scale!

### ESERCIZIO 3 ----------------------------------------------------------------
rm ( list = ls( ) )

polimi_studenti = read.table( file = "polimi_studenti.txt", header = TRUE,
                              quote = "\"", dec = ',', sep = '\t', fill = TRUE )

dim( polimi_studenti )

str ( polimi_studenti )

head( polimi_studenti )

### RICHIESTA 1: analizzare la variabile "ISTITUTO_REGIONE"
attach( polimi_studenti )

# Tabelle delle frequenze assolute e relative
table( ISTITUTO_REGIONE )
prop.table( table( ISTITUTO_REGIONE ) )

#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
par( mar = c( 10, 3, 3, 3 ) )
barplot( table( ISTITUTO_REGIONE ), las = 3 )

#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
pie( table( ISTITUTO_REGIONE ) )

dati_NO_LOMB = polimi_studenti[ -which( ISTITUTO_REGIONE == "Lombardia" ), ]
dim( dati_NO_LOMB )

#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
par( mar = c( 10, 3, 3, 3 ) )
barplot( table( dati_NO_LOMB$ISTITUTO_REGIONE ), las = 3 )

#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
pie( table( dati_NO_LOMB$ISTITUTO_REGIONE ) )

### RICHIESTA 2: confrontare la media degli esami sostenuti nel primo anno 
### tra studenti che hanno terminato il corso di studi e coloro che hanno
### abbandonato dopo il primo anno
levels( as.factor( ABBANDONI ) )

table( ABBANDONI )

media_voti_abbandoni = MEDIA_ESAMI_1[ which( ABBANDONI > 1 & MEDIA_ESAMI_1 > 0 ) ]
media_voti_laureati = MEDIA_ESAMI_1[ which( ABBANDONI == 0 & MEDIA_ESAMI_1 > 0 ) ]

length( media_voti_abbandoni )
length( media_voti_laureati )

# Box plot
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
boxplot( media_voti_abbandoni, media_voti_laureati,
         col = c('red', 'forestgreen'), names = c('Abbandoni', 'Laureati'), 
         main = "Box plot:\nMEDIA VOTI ESAMI PRIMO ANNO")

# Sintesi
summary( media_voti_abbandoni ) # oppure: boxplot( media_voti_abbandoni )$stats
summary( media_voti_laureati ) # oppure: boxplot( media_voti_laureati )$stats

# Outliers
boxplot( media_voti_abbandoni )$out
boxplot( media_voti_laureati )$out

# Istogrammi
#quartz()   # aprire una finestra grafica su macOS
x11()       # aprire una finestra grafica su Windows, Unix
par( mfrow = c( 2, 1 ) )
hist( media_voti_abbandoni, prob = TRUE, col = 'red',
      breaks = seq( 18, 30, length = 10 ), xlim = c( 18,30 ),
      ylim = c( 0, 0.15 ), xlab = 'Abbandoni', ylab = 'Densità',
      main = 'Istogramma: MEDIA VOTI ESAMI PRIMO ANNO' )
abline( v = mean( media_voti_abbandoni ), col = 'yellow' )
hist( media_voti_laureati, prob = TRUE, col='forestgreen',
      breaks = seq( 18, 30, length = 10 ), xlim = c( 18,30 ),
      ylim = c( 0, 0.15 ), xlab = 'Laureati', ylab = 'Densità',
      main = 'Istogramma: MEDIA VOTI ESAMI PRIMO ANNO' )
abline( v = mean( media_voti_laureati ), col = 'yellow' )

detach(polimi_studenti)
