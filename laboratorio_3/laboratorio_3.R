#'###########################################################'#
#'#######               LABORATORIO 3                 #######'#
#'#######      CAMPIONAMENTO DI VARIABILI ALEATORIE   #######'#
#'#######         LEGGE DEI GRANDI NUMERI             #######'#
#'#######           INTEGRAZIONE MONTE CARLO          #######'#
# #######        TEOREMA CENTRALE DEL LIMITE          #######'#
#'###########################################################'#

##    0 - CAMPIONAMENTO DI VARIABILI ALEATORIE NOTE  ---------------------------
#    Uniforme, Normale, Esponenziale, Binomiale, Poisson 

rm( list = ls() )
graphics.off()

# Uniforme

# runif(n, a, b)        restituisce n realizzazioni di una U(a,b)
# dunif(x, a, b)        restituisce valore della funzione di Densità di Probabilità 
#                       di una U(a,b) valutata in x
# punif(x, a, b)        restituisce valore della funzione di Ripartizione
#                       di una U(a,b) valutata in x
# qunif(alpha, a, b)    restituisce valore del quantile di ordine alpha
#                       di una U(a,b)

rm(list=ls())
graphics.off()

set.seed(0) # riproducibilità

n = 20
x_unif = runif(n) # n realizzazioni di una U(0,1)
x_ = seq(from=0, to=1, length.out=100)

plot(x_, dunif(x_), type="l", lwd=2, 
     xlab="x",ylab="", main="U(0,1)", ylim=c(0,1.2))
points(x_unif, rep(0,n), pch=16, col="red")

# Normale

# rnorm(n, mu, sd)        restituisce n realizzazioni di una N(mu,sd^2)
# dnorm(x, mu, sd)        restituisce valore della funzione di Densità di Probabilità 
#                       di una N(mu,sd^2) valutata in x
# pnorm(x, mu, sd)        restituisce valore della funzione di Ripartizione
#                       di una N(mu,sd^2) valutata in x
# qnorm(alpha, mu, sd)    restituisce valore del quantile di ordine alpha
#                       di una N(mu,sd^2)

set.seed(0) # riproducibilità

n = 20
x_norm = rnorm(n) # n realizzazioni di una N(0,1)
x_ = seq(from=-5, to=5, length.out=100)

plot(x_, dnorm(x_), type="l", lwd=2, 
     xlab="x",ylab="", main="N(0,1)", ylim=c(0, max(dnorm(x_))))
points(x_norm, rep(0,n), pch=16, col="red")

# Esponenziale

# rexp(n, lambda)        restituisce n realizzazioni di una Exp(lambda)
# dexp(x, lambda)        restituisce valore della funzione di Densità di Probabilità 
#                       di una Exp(lambda) valutata in x
# pexp(x, lambda)        restituisce valore della funzione di Ripartizione
#                       di una Exp(lambda) valutata in x
# qexp(alpha, lambda)    restituisce valore del quantile di ordine alpha
#                       di una Exp(lambda)

set.seed(0) # riproducibilità

n = 20
lambda = 1
x_exp = rexp(n, lambda)     # n realizzazioni di una Exp(lambda)
x_ = seq(from=0, to=10, length.out=100)

plot(x_, dexp(x_,lambda), type="l", lwd=2, 
     xlab="x",ylab="", main="Exp(1)")
points(x_exp, rep(0,n), pch=16, col="red")


# Binomiale
# rbinom(n, N, p)        restituisce n realizzazioni di una Bi(N,p)
# dbinom(x, N, p)        restituisce valore della funzione di Densità di Probabilità 
#                        di una Bi(N,p) valutata in x
# pbinom(x, N, p)         restituisce valore della funzione di Ripartizione
#                         di una Bi(N,p) valutata in x
# qbinom(alpha, mu, sd)   restituisce valore del quantile di ordine alpha
#                         di una Bi(N,p)
# NB. se N == 1 -> Be(p)

set.seed(0) # riproducibilità

n = 20
N = 1
p = 2/3
x_bern = rbinom(n, size=N, prob = p)     # n realizzazioni di una Be(p)

barplot(table(x_bern),
        xlab="x",ylab="", main="Be(2/3)")

# Poisson
# rpois(n, lambda)      restituisce n realizzazioni di una Po(lambda)
# dpois(x, lambda)      restituisce valore della funzione di Densità di Probabilità 
#                        di una Po(lambda) valutata in x
# ppois(x, lambda)         restituisce valore della funzione di Ripartizione
#                         di una Po(lambda) valutata in x
# qpois(alpha, lambda)   restituisce valore del quantile di ordine alpha
#                         di una Po(lambda)

n = 20
lambda = 5/2
x_pois = rpois(n, lambda=lambda)     # n realizzazioni di una Po(lambda)

barplot(table(x_pois),
        xlab="x",ylab="", main="Po(5/2)")

## 1 - CAMPIONAMENTO DI VARIABILI ALEATORIE ------------------------------------
# Metodo della Funzione Inversa & Metodo di Accettazione/Rifiuto

# NB. user-defined function

nome_funzione = function(param1, param2, ...){
  
  #corpo funzione
  
  return(...)
}

somma = function(x = 0, y = 0){
  return(x + y)
}

somma(1,2)


### METODO DELLA FUNZIONE INVERSA ----------------------------------------------

### Esempio 1: Campionamento da v.a. Continua
# Vogliamo campionare da una v.a. che ha funzione di densità
# f(x) = 4x se x in [0, 1/2]; 4(1-x) se x in [0.5,1]

f = function(x){
  risultato = vector(mode="numeric", length = length(x))
  for(i in 1:length(x)){
    if(x[i] >= 0 & x[i] < 0.5) 
      risultato[i] = 4 * x[i]
    else if( x[i] > 0.5 & x[i] <=1) 
      risultato[i] =  4 * (1 - x[i]) 
    }
  return(risultato)
}
# Calcoliamo la funzione di ripartizione
# F(x) = 2x^2 se x in [0, 1/2]; (1-2*(1-x)^2) in (1/2, 1]
F_ = function(x){
  risultato = vector(mode="numeric", length = length(x))
  for(i in 1:length(x)){
    if(x[i] < 0)
      risultato[i] = 0.
    else if(x[i] >= 0 & x[i] < 0.5) 
      risultato[i] = 2 * x[i]^2
    else if( x[i] > 0.5 & x[i] <=1) 
      risultato[i] = 1 - 2 * (1-x[i])^2 
    else if (x[i] > 1)
      risultato[i] =1.
  }
  return(risultato)
} 

x = seq(0,1,length=1000)

dev.new()
par(mfrow=c(2,1))
plot(x, f(x), type="l", lwd=2,
     xlab="", ylab="", main="f(x)")
plot(x, F_(x), type="l", lwd=2,
     xlab="", ylab="", main="F(x)")

# Calcoliamo l' inversa della funzione di ripartizione
# F^-1(x) = sqrt(u/2) se x in [0, 1/2); 1 - sqrt((1-x)/2) se x in [1/2, 1] 
# Se U ~ U[0,1] -> X = F^-1(U) ~ f

F_inv = function(x){
  res = vector(mode="numeric", length = length(x))
  for(i in 1:length(x)){
    if( x[i] >=0 & x[i] < 1/2 ){
      res[i] = sqrt(x[i] / 2)
    }else if( x[i] >1/2 & x[i] <= 1 ){
      res[i] = 1 - sqrt( ( 1 - x[i] ) / 2)
    }
  }
  return(res)
}

# Campioniamo N realizzazioni da una distribuzione uniforme in [0,1]
# e mostriamo che l'istogramma di x = F^-1(u) approssima
# la densita' f.

set.seed(0) # riproducibilità

N = 5000
u = runif(N)

campione = F_inv(u)

# Istogramma realizzazioni con vera Densità
dev.new()
hist(campione, prob=T,
     main="Istogramma realizzazioni", 
     xlab= "realizzazioni", ylab="Densità")
lines(x, f(x), lwd=2, col='red')
# invece di "lines" avremmo potuto utilizzare: 
# points(x_, f, type="l", lwd=2, col="red")

### ACCEPT-REJECT METHOD -------------------------------------------------------

# Esempio 

# Vogliamo campionare da v.a. con densità di probabilità
# g(x) = 2*f(x), dove f(x) è la funzione definita nell'esercizio precedente 
# NB. integrale di g NON è 1
g = function(x){
  return(2*f(x))
}

x = seq(0,1,length=1000)

# Notiamo che g(x) <= M con M = 4.1
M = 4.1

dev.new()
plot(x, g(x), xlab='x', ylab='g(x)', ylim =c(0,4.1),
     main='Distribuzione vera (non normalizzata)', type='l',  lwd=2, col='red')
abline(h=M, lty=2, col='blue')

# Accept-reject loop
# calcoliamo l'efficienza

set.seed(0) # riproducibilità

N = 10000  # numero di realizzazioni
accettati <- vector(mode="numeric", length=N)
rifiutati = 0
for(i in 1:N){
  accetto = FALSE
  while(accetto == FALSE){
    xx = runif(n=1, min=0, max=1) # realizzazione da U[0,1]
    u = runif(n=1, min=0, max=M) # realizzazione da U[0,M]
    if(u <= g(xx) ){
      accettati[i] = xx            # se vale la condizione accetto
      accetto = TRUE               # il valore campionato
    }else{
      rifiutati = rifiutati + 1
    }
  }
}

efficienza = N/(rifiutati + N)
print(efficienza)

# Istogramma delle realizzazioni ottenute 
dev.new()
hist(accettati, ylim=c(0, M), prob=T, ylab="Densità",
     main="Istogramma realizzazioni accettate")
lines(x, g(x), xlab='x', ylab='g(x)', 
      type='l', lwd=2, col='red')
lines(x, f(x), xlab='x', ylab='f(x)', 
      type='l', lwd=2, col='forestgreen')
abline(h=M, lty=2, col='blue')
legend(0.85,3.5, legend = c("g(x)", "f(x)"), 
       lty = 1, lwd=4, 
       col=c("red", "forestgreen"))

# L'istogramma delle realizzazioni ottenute approssima la vera densità
# che non conosciamo in forma chiusa perchè non non sappiamo normalizzare la f(x) 

## 2 - LEGGE DEI GRANDI NUMERI ( LGN ) -----------------------------------------

rm( list = ls())
graphics.off()

# Legge dei grandi numeri:
# Se X1, ..., Xn sono variabili aleatorie indipendenti e identicamente distribuite
# con media mu e varianza finita sigma^2.
# Allora la loro media campionaria ( X1 + ... + Xn )/n
# converge in probabilita' a mu, per n che tende all'infinito.

# Simuliamo n lanci di una moneta truccata in cui la probabilità
# di ottenere testa è 0.7, e studiamo come la proporzione di casi
# in cui si ottiene testa cambi all'aumentare del numero di lanci.
# 
# Per la Legge dei Grandi Numeri ci aspettiamo che la proporzione di lanci
# in cui si ottiene testa (media campionaria degli esiti dei lanci)
# converga al valore teorico 0.7.

# Lancio una moneta: campionamento da una Bernoulli

n = 1000 # Numero di lanci 
p = 0.7  # Probabilita' di successo (successo = esce testa)

set.seed(314159) # riproducibilità risultati

dati.Be = rbinom(n, size = 1, prob = p )
# 'rbinom' genera n osservazioni di una Binom(1, p), quindi di una Be(p).

barplot(table( dati.Be ), main = 
          'Barplot frequenze assolute lanci moneta', 
        ylab = 'Frequenze assolute', 
        names.arg = c( 'Croce', 'Testa' ))

# Calcoliamo la media campionaria delle osservazioni (ovvero la proporzione di successi) 
# dopo 1 lancio, dopo 2 lanci, ... fino a $n$.

#Per calcolare la media campionaria dividiamo la somma cumulata, ottenuta tramite cumsum, 
# per il numero di lanci corrispondente

somma.Be = cumsum(dati.Be)

n.lanci = 1:n
media.camp = somma.Be/n.lanci

# Visualizziamo la media campionaria al variare del numero di lanci
# aggiungiamo una retta orizzontale (rossa) corrispondente al valore teorico di p.
dev.new()
plot(n.lanci, media.camp, type= 'l', lwd= 2,
     main= 'Simulazione: la Legge dei Grandi Numeri', xlab= 'numero lanci',
     ylab= 'proporzione di successi', ylim = c(0,1))
abline(h= p, col= 'red', lty=2, lwd=2)

## 3 - INTEGRAZIONE MONTE-CARLO ----------------------------------------------

# Calcolare integrale di
# f(x) = 4x se x in [0, 1/2]; 4(1-x) se x in [0.5,1] tramite integrazione MC.  

# Si chiede di:
# 1. verificare che l'integrale di $f(x)$ sia approssimativamente uguale a $1$.
# 2. Calcolare approssimativamente media e varianza della variabile aleatoria continua X
#    che ha come funzione di densità di probabilità la funzione f(x).

# Confrontare i risultati ottenuti campioanando i punti per la valutazione 
# da una U(0,1) e da una N(0.5,0.25^2)

# 1. verificare che l'integrale di $f(x)$ sia approssimativamente uguale a $1$.

f = function(x){
  risultato = vector(mode="numeric", length = length(x))
  for(i in 1:length(x)){
    if(x[i] >= 0 & x[i] < 0.5) 
      risultato[i] = 4 * x[i]
    else if( x[i] > 0.5 & x[i] <=1) 
      risultato[i] =  4 * (1 - x[i]) 
  }
  return(risultato)
}

x = seq(0,1,length=1000)
dev.new()
plot(x, f(x), type="l", lwd=2,
     xlab="", ylab="", main="f(x)")

set.seed(123) # riproducibilità
mu = 0.5
sigma = 0.25
n = 100
x_unif = runif(n)                 
x_norm = rnorm(n, mean=mu, sd=sigma)
I_unif = sum( f( x_unif ) )/n
I_norm = sum( f( x_norm ) / dnorm(x_norm, mean=mu, sd=sigma) )/n

I_esatto = 1.

err_unif = abs(I_unif-I_esatto)
err_unif

err_norm = abs(I_norm-I_esatto)
err_norm

# Al variare di n (LGN)
set.seed(123)
n.max = 1000
x_unif = runif(n.max)                 
x_norm = rnorm(n.max, mean=mu, sd=sigma)
n = 1:n.max

I_unif = cumsum(f(x_unif))/n
I_norm = cumsum(f(x_norm)/dnorm(x_norm, mean=mu, sd=sigma)) / n

dev.new()
plot(n, I_unif, type="l", lwd=2, ylab="Approssimazione", xlab='n', 
     col="forestgreen")
points(n, I_norm, type="l", lwd=2, col="blue")
abline(h=I_esatto, col='red', lty=2, lwd=2)
legend(800, 1.2, legend = c("uniforme", "normale"), 
       col=c("forestgreen", "blue"), lty=1, lwd=4)

# 2. Calcolare approssimativamente media e varianza della variabile aleatoria continua X
#    che ha come funzione di densità di probabilità la funzione f(x).
# Nb. f(x) è la densità di una v.a. triangolare di parametri a=0, b=1, c=0.5
#     E[X] = (a+b+c)/3; Var(X) = ((a^2 + b^2 + c^2) - (ac + ab + bc))/18 
a=0; b=1; c=0.5;
media_esatta = (a+b+c)/3
var_esatta = ((a^2 + b^2 + c^2) - (a*c + a*b + b*c))/18

# E[X] = int x f(x) dx
media_unif = cumsum(x_unif * f(x_unif))/n
media_norm = cumsum(x_norm * f(x_norm)/dnorm(x_norm, mean=mu, sd=sigma)) / n

dev.new()
plot(n, media_unif, type="l", lwd=2, col="forestgreen", 
     ylab="Approssimazione", xlab='n', main="Valore Atteso")
points(n, media_norm, type="l", lwd=2, col="blue")
abline(h=media_esatta, col='red', lty=2, lwd=2)
legend(800, 0.6, legend = c("uniforme", "normale"), 
       col=c("forestgreen", "blue"), lty=1, lwd=4)

# Var(X) = E[X^2] -(E[X])^2

# E[X] = int x f(x) dx
momento_secondo_unif = cumsum(x_unif^2 * f(x_unif))/n
momento_secondo_norm = cumsum(x_norm^2 * f(x_norm)/dnorm(x_norm, mean=mu, sd=sigma)) / n

var_unif = momento_secondo_unif - media_unif^2
var_norm = momento_secondo_norm - media_norm^2

dev.new()
plot(n, var_unif, type="l", lwd=2, col="forestgreen", 
     ylab="Approssimazione", xlab='n', main="Varianza")
points(n, var_norm, type="l", lwd=2, col="blue")
abline(h=var_esatta, col='red', lty=2, lwd=2)
legend(800, 0.08, legend = c("uniforme", "normale"), 
       col=c("forestgreen", "blue"), lty=1, lwd=4)

## 5 - TEOREMA CENTRALE DEL LIMITE ( TCL ) -------------------------------------

rm(list = ls())
graphics.off()

# Teorema centrale del limite:
# Se X1, ..., Xn sono variabili aleatorie indipendenti e identicamente distribuite
# con media mu e varianza sigma^2.
# Allora la distribuzione di ( ( X1 + ... + Xn )/n-mu )/( sigma/sqrt( n ) )
# converge alla distribuzione di una normale standard N( 0, 1 ), per n che tende all'infinito.

# Inoltre, per il Teorema Centrale del Limite, la legge della somma di variabili aleatorie 
# indipendenti e identicamente distribuite converge alla distribuzione di una normale 
# di media n*mu e varianza n*sigma^2.
# 
# Studiamo la legge della somma di n variabili aleatorie indipendenti 
# con distribuzione Bernoulli di parametro p=0.7, al variare di n. 
# 
# NB. Si ricordi che nel caso considerato mu=p e sigma^2=p(1-p).
# 
# Per valutare la legge della somma, generiamo un certo numero N 
# di realizzazioni della somma e tracciamo un istogramma che viene confrontato 
# all'andamento della densità di probabilità di una N(np, np(1-p)). 

# NB. Generare $n$ variabili aleatorie Be(p) e calcolarne la somma è equivalente 
#     a generare una Bin(n,p).

# Per ogni valore di n generiamo N=500 realizzazioni di una Bin(n,p)
# in modo da avere N realizzazione della somma di n Bernoulli. 
# I dati generati sono salvati in un'unica matrice N x 4
# in cui la colonna j-esima contiene le N realizzazioni della Bin(n_j,p).

n = c(5, 50, 500, 5000) # Numero di variabili del campione che voglio simulare
p = 0.7 # Probabilita' di successo associata alla Bernoulli
N = 500 # Numero di realizzazioni per ciascuna variabile

set.seed(314159) # riproducibilità risultati

dati= matrix(0, nrow=N, ncol=length(n))
for(i in 1:length(n)){
  dati[,i]= rbinom(N, size= n[i], prob= p)
}

# Verifichiamo graficamente il Teorema Centrale del Limite:
# per ogni n considerato, tracciamo l'istogramma delle realizzazioni ottenute 
# per la somma di Bernoulli e sovrapponiamo la densità 
# di una N(np,np(1-p)).

dev.new()
par(mfrow=c(2,2))

x = seq(min(dati[,1]), max(dati[,1]), .01 )
media = n [ 1 ] * p
devstand = sqrt(n[1]*p*(1-p))
density = dnorm(x, mean= media, sd= devstand )

# Istogramma:
hist( dati[,1], prob = TRUE, main = 'TCL applicato a somma di Bernoulli\n n = 5',
      xlab = 'somma di n bernoulli',
      ylim = c(0,max(max(density), max(hist(dati[ ,1], plot= F)$density))),
      breaks = seq(min(dati[,1])-.5, max(dati[,1])+ .5, by= 1))
lines(x, density, col = 'red', lwd= 2)

x = seq( min(dati[,2]), max(dati[,2]), .01 )
media = n[2]*p
devstand = sqrt(n[2]*p*(1-p))
density = dnorm( x, mean = media, sd = devstand )
hist(dati[,2] , prob = TRUE, main = 'TCL applicato a somma di Bernoulli\n n = 50',
     xlab = 'somma di n bernoulli',
     ylim = c(0, max(max(density),max(hist(dati[,2],plot=F)$density))))
lines( x, density, col= 'red', lwd= 2 )

x = seq(min(dati[,3]), max(dati[,3]), .01 )
media = n[3]*p
devstand= sqrt( n[3]*p*(1-p))
density= dnorm(x, mean = media, sd = devstand)
hist( dati[,3], prob = TRUE, main = 'TCL applicato a somma di Bernoulli\n n = 500',
      xlab = 'somma di n bernoulli',
      ylim = c(0, max( max( density), max(hist(dati[,3], plot=F)$density))))
lines( x, density, col= 'red', lwd= 2)

x = seq(min(dati[,4]), max(dati[,4]), .01 )
media = n[4]*p
devstand = sqrt( n[4]*p*(1-p))
density = dnorm(x, mean = media, sd = devstand )
hist( dati[,4], prob = TRUE, main = 'TCL applicato a somma di Bernoulli\n n = 5000',
      xlab = 'somma di n bernoulli',
      ylim = c(0, max(max(density), max(hist(dati[,4], plot=F)$density))))
lines( x, density, col= 'red', lwd= 2)

# Si noti che per n grande risulta sempre meno evidente 
# la distinzione fra l'andamento dei quantili empirici della somma di Bernoulli 
# e quelli di una distribuzione gaussiana.


# Consideriamo la Media Campionaria delle n Bernoulli.
# Valutiamo al crescere di n l'approssimazione della Media Campionaria 
# con una Normale N(p,p(1-p)/n)

mean_ = matrix(0,nrow=N,ncol=length(n))

for(j in 1:length(n))
  mean_[,j]= dati[,j]/n[j]
mu = p
var = p*(1-p)

x_ = seq(from=0,to=1, length.out=1000)

dev.new()
par(mfrow=c(2,2))
for(i in 1:length(n)){
  density = dnorm(x_, mean=mu, sd=sqrt(var/n[i]))
  hist(mean_[,i], probability = TRUE,
       xlab="p", main=paste('TCL: Media Campionaria di Bernoulli\n','n = ',n[i],sep=""),
       xlim = c(0,1),
       ylim = c( 0, max(max(density),max(hist(mean_[,i],plot=F)$density))))
  lines(x_, density, col="red", lwd=2)
}
