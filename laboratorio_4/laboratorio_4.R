#'###########################################################'#
#'#####                 LABORATORIO 4                   #####'#
#'#####    INFERENZA E TEST PER UNA E DUE POPOLAZIONI   #####'#
#'###########################################################'#

# Argomenti del quarto laboratorio:
# 1.1 - IC e test per la media di una popolazione normale a varianza nota
# 1.2 - IC e test per la media di una popolazione normale a varianza incognita
# 2 - IC e test per una proporzione
# 3 - IC e test per la varianza di una popolazione normale
# 4.1 - IC e test per la differenza tra due popolazioni normali (dati accoppiati)
# 4.2 - IC e test per la differenza tra due popolazioni normali (dati non accoppiati)
# 5 - Calcolo della potenza e della curva di potenza di un test via Monte Carlo
# 6 - Calcolo del livello di confidenza reale via Monte Carlo

# IMPOSTARE LA WORKING DIRECTORY (CARTELLA DI LAVORO): -------------------------
# Da interfaccia:
# 'Session' -> 'Set Working Directory' -> 'Choose Directory' -> ...

# Da console:
# setwd( 'C:/percorso/file' )

# Da pacchetto:
if(!require(rstudioapi)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## 1 - IC E TEST PER LA MEDIA DI UNA POPOLAZIONE -------------------------------

### 1.1 VARIANZA NOTA ----------------------------------------------------------

library(BSDA)
library(EnvStats) 

# Carichiamo i dati contenuti nel file lampadine.txt:
# Essi rappresentano il tempo di vita (espresso in ore) di un campione di 
# 20 lampadine da 75 watt. Il tempo di vita della lampadina è una variabile aleatoria 
# normale con deviazione standard nota pari a 25 ore.  

# 1) E' ragionevole supporre che la media di vita delle lampadine sia pari a 1000 ore? 
# Usare un livello di significatività pari al 5%.
# Calcolare il p-value del test. 

# Importiamo il dataset
dati <- read.table('lampadine.txt', dec='.', header=T)
head(dati)

dim(dati)
n <- dim(dati)[1] # n è il numero di lampadine (dimensione del campione)
names(dati)

# Il testo dell'esercizio dice che i dati sono campionati da una normale, quindi potrei non 
# verificare la normalità. Per scrupolo facciamo comunque una verifica:

# Tramite Q-Q plot:
qqnorm(dati$tempo_vita)
qqline(dati$tempo_vita)

# Tramite Shapiro test:
shapiro.test(dati$tempo_vita) # posso supporre i dati normali!

# Consideriamo il test:
# H_0: mu = 1000     vs     H_1: mu != 1000

alpha <- 0.05 
help(z.test)
z.test(x = dati$tempo_vita,
       alternative = "two.sided", 
       mu = 1000, 
       sigma.x = 25, 
       conf.level = 1-alpha)

res <- z.test(x = dati$tempo_vita, 
              alternative = "two.sided", 
              mu = 1000,
              sigma.x = 25, 
              conf.level = 1-alpha)
names(res)

res$statistic # statistica test del test z
res$p.value # p-value

res$estimate # stima puntuale
res$conf.int # stima intervallare

# Possiamo anche costruire l'intervallo di confidenza "a mano":
sigma <- 25
z.alpha <- qnorm(1-alpha/2)
IC.alpha <- c( mean(dati$tempo_vita) - z.alpha*sigma/sqrt(n),  mean(dati$tempo_vita) + z.alpha*sigma/sqrt(n))

# Plottiamo IC e dati
plot(rep(1,n),dati$tempo_vita,col=4,lwd=2,ylim=range(dati$tempo_vita),xlab='') 
points(res$estimate,pch=4,lwd=3)
points(rep(1,2),res$conf.int,type='l',lwd=2)
points(1000,pch=4,lwd=3,col="red")

# 2) E' ragionevole supporre che la media di vita delle lampadine superi 1000 ore? 

# Consideriamo il test unilatero:
# H_0: mu = 1000     vs     H_1: mu > 1000

z.test(x = dati$tempo_vita, 
       alternative = "greater",
       mu = 1000, 
       sigma.x = 25, 
       conf.level = 1-alpha)

# 3) E' ragionevole supporre che la media di vita delle lampadine sia inferiore a 1000 ore? 

# Consideriamo il test unilatero:
# H_0: mu = 1000     vs     H_1: mu < 1000

z.test(x = dati$tempo_vita, 
       alternative = "less", 
       mu = 1000, 
       sigma.x = 25,
       conf.level = 1-alpha)

rm(list=ls())
graphics.off()

### 1.2 VARIANZA INCOGNITA -----------------------------------------------------

# Carichiamo i dati contenuti nel file temperatura.txt:
# Essi rappresentano la temperatura corporea (espressa in gradi Fahrenheit)
# di un campione di 130 soggetti, di cui 65 maschi e 65 femmine.
# Si vuole stabilire se la media reale della temperatura corporea della popolazione
# sia 98.6 gradi F

# Importiamo il dataset
dati <- read.table('temperatura.txt', dec='.', header=T)
head(dati)

dim(dati)
n <- dim(dati)[1] # n è il numero di pazienti (dimensione del campione)
names(dati)

# Per impostare un test corretto per la media della variabile Temperatura, 
# dobbiamo sapere se la variabile Temperatura può essere considerata normale

par(mfrow = c(1,2))
hist(dati$Temperatura,prob=TRUE)
qqnorm(dati$Temperatura)
qqline(dati$Temperatura,lwd=2,col='red')

shapiro.test(dati$Temperatura)
# pvalue abbastanza alto quindi possiamo assumere che i dati siano normali

# effettuo un test per verificare l'ipotesi
#       H_0: mu = 98.6 F     vs     H_1: mu != 98.6 F

alpha<-0.05
t.test(x = dati$Temperatura, alternative = "two.sided", mu = 98.6, conf.level = 1-alpha)

# il p-value è circa 0, per cui ho forte evidenza per affermare che la media vera
# sia diversa da 98.6.

rm(list=ls())
graphics.off()

## 2 - IC E TEST PER UNA PROPORZIONE -------------------------------------------

# Carichiamo i dati contenuti nel file penicillina.txt:
# Essi rappresentano la temperatura corporea (espressa in gradi Fahrenheit)
# di 100 pazienti ricoverati per meningite, che sono stati trattati con un'ampia
# dose di penicillina. Se dopo 3 giorni è stata osservata una diminuzione di
# temperatura, il trattamento è stato considerato un successo.
# Vogliamo stabilire se i dati ci permettono di affermare, con un livello di significatività
# pari a 0.05, che il trattamento ha successo almeno nel 60% dei casi.

# Importiamo il dataset
dati <- read.table('penicillina.txt', header=T)
head(dati)

dim(dati)
n <- dim(dati)[1] # n è il numero di pazienti (dimensione del campione)
names(dati)

# effettuo un test per verificare l'ipotesi
#        H_0: p = 0.6 vs H_1: p > 0.6

p.0 <- 0.6

# Per poter usare l'approssimazione Gaussiana dobbiamo verificare che siano soddisfatte 
# le ipotesi 
n>50
n*p.0>5
n*(1-p.0)>5
# sono tutte soddisfatte

z.test(x=dati$Rid_Temp, alternative = "greater", mu = p.0, sigma.x = sqrt(p.0*(1 - p.0)))

# ad un livello di significatività del 5%,  non possiamo rifiutare l'ipotesi nulla
# ad un livello di significatività del 10%, possiamo rifiutare l'ipotesi nulla

rm(list=ls())
graphics.off()

## 3 - IC E TEST PER LA VARIANZA DI UNA POPOLAZIONE NORMALE --------------------

# Carichiamo il dataset funi.txt
# Esso i risultati di 25 prove di rottura (risultati espressi 
# in Newton) di funi prodotte in una fabbrica per mezzo di un nuovo processo 
# produttivo
# Si sa che le funi tradizionali hanno una resistenza di rottura pari a 1730N. 
# 1) Il processo produttivo ha significativamente migliorato la qualità 
# delle funi?
# 2) Si vuole poi verificare con un opportuno test al 5% che la varianza 
# della resistenza delle funi non sia superiore a 28900N^2 
# (deviazione standard 170N) e calcolare poi il p-value di tale test.
# 3) Calcolare un intervallo di confidenza bilatero per la varianza al 95%.

# Importiamo il dataset
dati <- read.table('funi.txt', header=T)
dati

dim(dati)
n <- dim(dati)[1] # n è il numero di pazienti (dimensione del campione)
names(dati)

# verifica normalità
par(mfrow = c(1,2))
hist(dati$resistenza,prob=TRUE)
qqnorm(dati$resistenza)
qqline(dati$resistenza, lwd=2, col='red')

shapiro.test(dati$resistenza)
# pvalue abbastanza alto quindi possiamo assumere che i dati siano normali

# 1) Vogliamo effettuare un test sulla media 
# H0: mu=1730  contro  H1: mu>1730 

mu0 <- 1730
t.test(dati$resistenza, mu=mu0, alternative="greater")

# Si conclude che il processo produttivo ha significativamente migliorato  
# la qualità delle funi.

# 2) Test per la varianza di una popolazione normale.
help(varTest)

# Test per varianza di una gaussiana
# H0: sigma2 >= 28900 vs H1: sigma2 < 28900

sigma2_0 <- 28900

alpha<-0.05
varTest(x = dati$resistenza, alternative = "less", conf.level = 1-alpha, 
        sigma.squared = sigma2_0)

# A livello 5% viene rifiutata l'ipotesi nulla e si può quindi dichiarare che
# la resistenza delle funi ha una deviazione standard NON superiore ai 170N.

# 3) Intervallo di confidenza bilatero per la varianza al 95%

varTest(x = dati$resistenza, alternative = "two.sided", conf.level = 1-alpha)

rm(list=ls())
graphics.off()

## 4 - IC E TEST PER LA DIFFERENZA TRA DUE POPOLAZIONI NORMALI -----------------

### 4.1 DATI ACCOPPIATI --------------------------------------------------------

# Analizziamo il dataset jogging.txt.
# Questo dataset contiene le misurazioni della frequenza cardiaca a riposo
# relative a 50 volontari che non hanno mai svolto attività fisica
# prima di iniziare un programma di un mese di jogging e dopo il mese 
# di attività sportiva.

# Importazione dei dati
dati <- read.table('jogging.txt')
head(dati)

# Si vuole stabilire se l'attività sportiva porta ad una diminuzione 
# della frequenza cardiaca a riposo.
# Viene quindi richiesto di:
# 1) eseguire un'esplorazione grafica dei dati;
# 2) eseguire un test d'ipotesi per stabilire se l'attività sportiva porta ad una diminuzione 
#    della frequenza cardiaca a riposo;
# 3) calcolare un intervallo di confidenza per la differenza di medie al livello di confidenza 90%.

dim(dati)
n <- dim(dati)[1] # n ? il numero di pazienti (dimensione del campione)
names(dati)

# 1): Esplorazione grafica della frequenza cardiaca prima e dopo
# il programma di jogging

boxplot(dati, col=c('green','orange'))

dev.new()
par(mfrow=c(2,1))
hist(dati$prima, prob=T, xlim=c(50,120), breaks=seq(50,120,by=5), col='green')
hist(dati$dopo, prob=T, xlim=c(50,120), breaks=seq(50,120,by=5), col='orange')

# Dopo il mese di attività sportiva la mediana della frequenza cardiaca è leggermente
# diminuita ma è aumentata la variabilità del campione.

# 2): Test d'ipotesi per la differenza di medie

# I dati sono accoppiati perchè le misurazioni prima e dopo sono effettuate 
# sulle stesse persone.
# Si può quindi creare una nuova variabile diff che rappresenta la differenza 
# tra la frequenza prima dell'attività sportiva e dopo.

diff <- dati$prima-dati$dopo

diff.media <- mean(diff)
diff.media

# Prima di procedere con il test doppiamo verifica della normalità di diff
dev.new()
par(mfrow=c(2,1))
hist(diff,prob=TRUE)
qqnorm(diff)
qqline(diff,lwd=2,col='red')

shapiro.test(diff)
# La differenza dei dati può essere considerata normale

# Dato che stiamo considerando diff = prima - dopo, per stabilire se l'attività sportiva porta 
# ad una diminuzione della frequenza cardiaca a riposo, dobbiamo quindi provare che diff 
# sia effettivamente maggiore di 0.
# Per fare questo possiamo effettuare un test sulla media della differenza
# H0: mu.diff<=0 vs H1: mu.diff>0

t.test(diff, mu=0, alternative = "greater")
# Ad un livello di significatività del 5%,  non possiamo rifiutare l'ipotesi nulla

# O in alternativa:
t.test(dati$prima, dati$dopo, mu=0, alternative = "greater", paired = TRUE)

# N.B. Attenzione alla concordanza tra l'ordine di "prima", "dopo" e l'alternative hypothesis.

# 3): IC bilatero per la differenza delle medie al 90%
alpha <- 0.10
t.test(dati$prima, dati$dopo, mu=0, alternative = "two.sided", paired = TRUE, conf.level = 1-alpha)

# IC 90% per (prima-dopo) = [-1.157734; 7.917734]

rm(list=ls())
graphics.off()

### 4.2 DATI NON ACCOPPIATI ---------------------------------------------------- 

# Riprendiamo i dati del file temperatura.txt.
# Carichiamo i dati contenuti nel file temperatura.txt:
dati <- read.table('temperatura.txt', header=T)
dati

# Essi rappresentano la temperatura corporea (espressa in gradi Fahrenheit)
# di un campione di 130 soggetti, di cui 65 maschi e 65 femmine.
# Si vuole stabilire se la media reale della temperatura corporea della popolazione
# sia 98.6 gradi F

# 1) Stabilire se ci sono differenze nella temperatura corporea dovute al
#    sesso del soggetto per mezzo di grafici e di un opportuno test.
# 2) Calcolare un intervallo di confidenza bilatero per la differenza nella 
#    temperatura corporea tra donne e uomini a livello 97%.
# 3) Stabilire se la temperatura corporea media delle donne è superiore 
#    alla temperatura corporea media degli uomini.

dim(dati)
n <- dim(dati)[1] # n è il numero di pazienti (dimensione del campione)
names(dati)

# 1): Rappresentiamo gli istogrammi delle due popolazioni e i boxplot
# e facciamo un test d'ipotesi per la differenza tra le medie
# delle temperature corporee nelle due sottopopolazioni individuate
# dal sesso.

# Consideriamo innanzitutto i due campioni distinti per sesso
temp.m <- dati$Temperatura[which(dati$Sesso=='U')]
temp.f <- dati$Temperatura[which(dati$Sesso=='D')]
# ora ho due campioni di ampiezza dimezzata
n.m <- length(temp.m)
n.f <- length(temp.f)
n.m
n.f

# esplorazione grafica
dev.new()
par(mfrow=c(2,1))
hist(temp.f, prob=TRUE, main='Istogramma Temperatura Donne',
     xlab='temperatura corporea [gradi F]',
     ylab='Densità', col='pink', xlim=range(dati$Temperatura), breaks=seq(96,101,.25))
hist(temp.m, prob=TRUE, main='Istogramma Temperatura Uomini',
     xlab='temperatura corporea [gradi F]',
     ylab='Densità', col='lightblue', xlim=range(dati$Temperatura), breaks=seq(96,101,.25))

dev.new()
par(mfrow=c(1,1))
boxplot(temp.f, temp.m, horizontal=FALSE, main='Boxplot Temperatura',
        names=c('Donne','Uomini'), col=c('pink','lightblue'),
        ylab='temperatura corporea [gradi F]', ylim=c(94,102))

# Tendenza delle donne ad avere una temperatura corporea più alta rispetto agli uomini.

# Prima di effettuare il test sulle medie verifichiamo la normalità
# delle due popolazioni osservazione: per questo tipo di test di ipotesi
# la verifica della normalit? va fatta in maniera distinta per le due popolazioni
dev.new()
par(mfrow=c(2,1)) 
qqnorm(temp.f, main='Temperatura Donne')
qqline(temp.f, col='pink',lwd=2)
qqnorm(temp.m, main='Temperatura Uomini')
qqline(temp.m, col='light blue',lwd=2)

shapiro.test(temp.f)
shapiro.test(temp.m)
# Consideriamo comunque i due campioni normali

# Test d'ipotesi sulla differenza di media tra due popolazioni
#       H_0: mu_F = mu_M    vs     H_1: mu_F != mu_M
t.test(temp.f, temp.m, mu=0, alternative = "two.sided", paired = FALSE, var.equal = TRUE)

# Al livello 5% ho evidenza per rifiutare H_0 ed affermare che esiste una
# differenza nella media della temperatura corporea nelle due sottopopolazioni.
# Al livello 1% non avrei rifiutato H_0.

# 2): IC bilatero per la differenza nella temperatura corporea tra donne 
# e uomini a livello 97%.
alpha <- 0.03
t.test(temp.f, temp.m, mu=0, alternative = "two.sided",
       paired = FALSE, conf.level = 1-alpha, var.equal = TRUE)

# IC al 95% per (donne-uomini): [0.03882; 0.53964]

# 3): Facciamo un test d'ipotesi per la differenza tra le medie
# delle temperature corporee nelle due sottopopolazioni individuate
# dal sesso per stabilire se la temperatura corporea media delle donne è
# superiore a quella degli uomini.

# Abbiamo già verificato che
# 1) Le due popolazioni sono normali
# Possiamo procedere con il test unilatero

# Test unilatero sulla differenza di media tra due popolazioni
#       H_0: mu_F <= mu_M    vs     H_1: mu_F > mu_M
alpha <- 0.05
t.test(temp.f, temp.m, mu=0, alternative = "greater",
       paired = FALSE, conf.level = 1-alpha, var.equal = TRUE)

# Al livello 5% ho evidenza per rifiutare H_0 ed affermare che la temperatura 
# corporea media delle donne è superiore a quella degli uomini.
# Al livello 1% non avrei rifiutato H_0.

## 5 - CALCOLO DELLA POTENZA E DELLA CURVA DI POTENZA VIA MONTE CARLO ----------

rm(list=ls())
graphics.off()

# La lunghezza dei componenti meccanici prodotti da un'azienda segue una distribuzione 
# normale con media mu e con deviazione standard nota pari a 10 cm
# 
# Consideriamo il test:
# H_0: mu = 40 cm    vs     H_1: mu > 40 cm
# 1) calcolare la potenza del test con alpha=0.05 di un campione di 20 componenti sapendo 
#    che la vera media è mu =42 cm
# 2) Cosa succede alla potenza al variare di mu nell'intervallo fra 41 e 50 cm ?

# 3) Ripetere l'esercizio con un campione di 40 componenti? Cosa succede ala potenza al 
#    variare del numero di componenti?

# 1) calcolare la potenza del test con alpha=0.05 di un campione di 20 componenti sapendo 
#    che la vera media è mu =42 cm
n <- 20 
mu0 <- 40  
mu1 <- 42
sigma <- 10
alpha <- 0.05
N <- 2000 # numero di iterazione montecarlo

# vettore in cui inserirò i risultati del test
# Metto 0 se accetto H_0, 1 se rifiuto H_0, quindi alla fine dell'esperimento
# sum(esito) sarà il numero di volte che ho correttamente concluso che H_0 era falsa
esito <- rep(0,N) 

for( i in 1:N){
  # genero un random sample sotto H1, quindi con media mu1
  x <- rnorm (n , mean = mu1 , sd = sigma ) 
  
  # testo se la media è uguale a mu0
  z.test <- z.test(x = x, alternative = "greater", mu = mu0, sigma.x = sigma) # calcolo il test
  if( z.test$p.value < alpha ){ 
    esito[i] <- 1
  }
}

plot(as.factor(esito))
power = mean(esito) # calcolo la potenza del test come proporzione di volte in cui ho rifiutato H0
power

# 2) Cosa succede alla potenza al variare di mu nell'intervallo fra 41 e 50 cm ?

# ripeto l'esercizio al variare di mu sotto H1 nell'intervallo fra 41 e 50 cm
mu <- 41:50 # alternatives
M <- length(mu) 
# creo il vettore dove andrò a salvare la potenza per ogni media 
power <- rep(0, M)
# per ogni media, ripeto il procedimento di prima
for ( j in 1:M ) {
  mu1 <- mu[j]
  
  esito <- rep(0,N)
  for( i in 1:N){
    # genero un random sample sotto H1, quindi con media mu1
    x <- rnorm (n , mean = mu1 , sd = sigma ) 
    
    # testo se la media è uguale a mu0
    z.test <- z.test(x = x, alternative = "greater", mu = mu0, sigma.x = sigma) # calcolo il test
    if( z.test$p.value < alpha ){ 
      esito[i] <- 1
    }
  }
  
  power[j] <- mean(esito)
}

plot(mu,power,pch=20,ylab="Potenza",xlab="Vera media", main = "Potenza al variare della media")
lines(mu,power)


# 3) Ripetere l'esercizio con un campione di 40 componenti? Cosa succede ala potenza al 
#    variare del numero di componenti?

# Salvo la potenza con 20 componenti in un vettore che chiamo power_20 per poterla confrontare 
# con la nuova che calcolo per 40 componenti
power_20 = power
n = 40

# Ripeto la stessa cosa di prima, questa volta con n = 40
mu <- 41:50 # alternatives
power <- rep(0, M)
# per ogni media, ripeto il procedimento di prima
for ( j in 1:M ) {
  mu1 <- mu[j]
  
  esito <- rep(0,N)
  for( i in 1:N){
    # genero un random sample sotto H1, quindi con media mu1
    x <- rnorm (n , mean = mu1 , sd = sigma ) 
    
    # testo se la media ? uguale a mu0
    z.test <- z.test(x = x, alternative = "greater", mu = mu0, sigma.x = sigma) # calcolo il test
    if( z.test$p.value < alpha ){ 
      esito[i] <- 1
    }
  }
  
  power[j] <- mean(esito)
}

# Salvo la potenza nel vettore power_40
power_40 = power

# Confronto col risultato precendente
plot(mu,power_20,pch=20,ylab="Potenza",xlab="Vera media", main = "Potenza al variare della media",col="red")
lines(mu,power_20,col="red")
points(mu,power_40,pch=20,col="blue")
lines(mu,power_40,col="blue")
legend("bottomright",legend = c("n=20","n=40"),col=c("red","blue"),lwd=2)

## 6- CALCOLO DEL LIVELLO DI SIGNIFICATIVITA' REALE VIA MONTE CARLO ------------

rm(list=ls())
graphics.off()

# Vogliamo ora calcolare l'errore di primo tipo (rifiuto H_0 quando H_0 è vera)
# tramite metodo Monte Carlo

# Consideriamo il test:
#       H_0: mu = mu_0     vs     H_1: mu != mu_0
# La regione critica (di rifiuto) del test bilatero di livello alpha è
#       R_alpha = {abs(media.camp - mu_0)/(sigma/sqrt(n)) > z_(1-alpha/2)}

# Vogliamo calcolare ora alpha = probabilità errore di primo tipo (rifiuto H_0 quando H_0 ? vera)

# Simuliamo 1000 realizzazioni di un campione di 10 variabili aleatorie gaussiane
# con media mu = 50 e deviazione standard sigma = 2.5 nota.
# Calcoliamo quindi la percentuale di realizzazioni campionarie che ci portano
# a rifiutare H_0.

N <- 1000   # numero di esperimenti
n <- 10      # dimensione del campione
sigma <- 2.5 # deviazione standard (nota)
mu <- 50     # media vera della popolazione da cui provengono i campioni

# Dato che vogliamo calcolare la probabilità errore di primo tipo, dobbiamo costruire il test
# in modo che H_0 sia vera, quindi facciamo un test in cui mu_0 = 50
mu.0 <-50    # media ipotizzata in H_0

alpha <- 0.05 # livello teorico del test, che supponiamo di non sapere

# Creo un vettore che conterrà il risultato del test ad ogni iterazione
# Metto 0 se accetto H_0, 1 se rifiuto H_0, quindi alla fine dell'esperimento
# sum(esito) sarà il numero di volte che ho concluso che H_0 era falsa, anche se era vera.
esito <- rep(0, N) 

for(i in 1:N){ # ripeto il test N volte
  # set.seed(i) # lo faccio se voglio poter riprodurre gli stessi risultati ogni volta che lancio il codice
  
  # ad ogni iterazione simulo i dati gaussiani su cui effettuare il test
  dati.sim <- rnorm(n, mean = mu, sd = sigma)
  
  # effettuo il test: esito = 1 se rifiuto H_0, 0 se accetto H_0
  z.test = z.test(dati.sim, mu = mu.0,  sigma.x = sigma)
  if( z.test$p.value < alpha ){ 
    esito[i] <- 1
  }
}

# calcolo una stima della probabilità di errore di prima specie:
# proporzione di volte in cui rifiuto
alpha.camp <- mean(esito)
alpha.camp

# la stima di alpha è vicina all'errore di primo tipo reale,
# provare a cambiare N e vedere come cambia la stima. 