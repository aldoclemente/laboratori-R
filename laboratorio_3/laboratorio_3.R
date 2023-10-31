###############################
##    CORSO DI STATISTICA    ##
## per INGEGNERIA MATEMATICA ##
###############################

###############################################
#                 LABORATORIO 3               #
#    CAMPIONAMENTO DI VARIABILI ALEATORIE     #
#          INTEGRAZIONE MONTE CARLO           #
#      Q-Q PLOT e VERIFICA DI NORMALITA'      #
#           LEGGE DEI GRANDI NUMERI           #
#         TEOREMA CENTRALE DEL LIMITE         #
###############################################

# Argomenti trattati nel laboratorio 3:
# 1- Campionamento di variabili aleatorie
# 2- Approssimazione integrali con metodo Monte-Carlo
# 3- Q-Q plot e verifica della normalita' dei dati
# 4- Verifica sperimentale della legge dei grandi numeri sui dati simulati
# 5- Verifica sperimentale del teorema centrale del limite sui dati simulati

# set working directory
if(!require(pacman, quietly = TRUE)) install.packages("pacman")
pacman::p_load("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))

#####################################################
####  0 - CAMPIONAMENTO DI VARIABILI ALEATORIE   ####
####  Uniforme, Normale, Esponenziale, Binomiale ####
#####################################################

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

##################################################
#### 1 - CAMPIONAMENTO DI VARIABILI ALEATORIE ####
##################################################
rm( list = ls() )
graphics.off()

#------------------------------------#
### METODO DELLA FUNZIONE INVERSA  ###
#------------------------------------#

### Esempio 1: Campionamento da v.a. Continua
# Vogliamo campionare da una v.a. che ha funzione di densità
# f(x) = 1/pi * (1/(1+x^2))
# Calcoliamo la funzione di ripartizione
# F(x) = 1/2 + 1/pi*arctan(x)
# Calcoliamo l' inversa della funzione di ripartizione
# F^-1(u) = tan(pi(u - 1/2)) , u in (0, 1)
# Se U ~ U[0,1] -> X = F^-1(U) ~ f
# Campioniamo N realizzazioni da una distribuzione uniforme in [0,1]
# e mostriamo che l'istogramma di x = F^-1(u) approssima
# la densita' f.

set.seed(0) # riproducibilità

N = 5000
u = runif(N)
x.samples = tan(pi*(u - 0.5))

x_ = seq(-1000,1000,by=0.1)
f = 1/pi*(1/(1+x_^2))

# Istogramma realizzazioni con vera Densità
dev.new()
hist(x.samples, prob=T,
     main="Istogramma realizzazioni", 
     xlab= "realizzazioni", ylab="Densità")
lines(x_, f, lwd=2, col='red')
# invece di "lines" avremmo potuto utilizzare: 
# points(x_, f, type="l", lwd=2, col="red")

# Zoom intorno a 0
x_ = seq(-10,10,by=0.1)
f = 1/pi*(1/(1+x_^2))

dev.new()
hist(x.samples[x.samples>-5 & x.samples<5], prob=T,
     main="Istogramma realizzazioni in [-5,5]",
     xlab= "realizzazioni", ylab="Densità")
lines(x_, f, lwd=2, col='red')

# L'istogramma delle realizzazioni ottenute approssima la densità f(x)

### Esempio 2: Campionamento da v.a. discreta
# Vogliamo campionare X da una distribuzione con funzione di densita' f
# f(x) = 0.5  per x = 1
#        0.2  per x = 2
#        0.3  per x = 3
# La funzione di ripartizione non e' invertibile, poiche' e' costante a tratti,
# e quindi non iniettiva.
# Possiamo comunque costruire l'inversa, limitandadola ai possibili valori assunti da x
# La scriviamo in questo modo:
#F^-1(u) = 1 per u in [0. 0.5)
#        = 2 per u in [0.5, 0.7)
#        = 3 per u in [0.7, 1]
# Formula: x=k se  sum(x_i<k) p(x_i) < u < sum(x_i<=k) p(x_i)
# Campioniamo N realizzazioni da una distribuzione uniforme in [0,1]
# e mostriamo che l'istogramma di x = F^-1(u) approssima
# la densita' f.
rm(list=ls())
graphics.off()

set.seed(0) # riproducibilità

N = 5000
u = runif(N)

inverse_F <- function(u){
  if(u<0.5){
    return(1)
  }else if (u<0.7){
    return(2)
  }else{
    return(3)
  }
}

x = vector(mode="numeric", length= N)
for(i in 1:N){
  x[i] = inverse_F(u[i])
}

freq_relative = table(x)/N
freq_relative

barplot(freq_relative)
lines( x=c(1,2,3), y=c(0.5, 0.2, 0.3), col = 'red', type = 'h', lwd = 2 )

#--------------------------#
### ACCEPT-REJECT METHOD ###
#--------------------------#

# Esempio 

# Vogliamo campionare da v.a. con densità di probabilità
# f(x) = (2 + sin(10*x^2)*exp(x)*sqrt(x))/ integrale_0^1(2 + sin(10*x^2)*exp(x)*sqrt(x) dx)
# definita in [0,1].

f<-function(x){
  return(2 + sin(10*x^2)*exp(x)*sqrt(x))
}

x_ = seq(0,1,by=0.01)
dens_ = f(x_) 
dev.new()
plot(x_, dens_, xlab='x', ylab='f(x)', 
     main='Distribuzione vera (non normalizzata)', type='l', col='red')

# Notiamo che f(x) <= M con M = 4.4
M = 4.4
abline(h=M, lty=2, col='blue')

# Accept-reject loop
# calcoliamo l'efficienza

set.seed(0) # riproducibilità

N = 10000  # numero di realizzazioni
x.accept <- vector(mode="numeric", length=N)
rejected = 0
for(i in 1:N){
  accept = FALSE
  while(accept==FALSE){
    x = runif(n=1, min=0, max=1) # realizzazione da U[0,1]
    u = runif(n=1, min=0, max=M) # realizzazione da U[0,M]
    if(u<=f(x)){
      x.accept[i] = x            # se vale la condizione accetto
      accept = TRUE              # il valore campionato
    }else{
      rejected = rejected + 1
    }
  }
}

efficienza = N/(rejected + N)
print(efficienza)

# Istogramma delle realizzazioni ottenute 
dev.new()
hist(x.accept, ylim=c(0, M), prob=T, 
     main="Istogramma realizzazioni accettate")
lines(x_, dens_, xlab='x', ylab='f(x)', 
      type='l', col='red')
abline(h=M, lty=2, col='blue')

# L'istogramma delle realizzazioni ottenute approssima la vera densità
# che non conosciamo in forma chiusa perchè non non sappiamo normalizzare la f(x) 


######################################
#### 2 - INTEGRAZIONE MONTE-CARLO ####
######################################

rm(list=ls())
graphics.off()

# Data una variabile aleatoria X con densita' f(x),
# vogliamo approssimare il valore atteso della variabile aleatoria Y = h(X).
# Per farlo scegliamo una distribuzione f(x) e generiamo n realizzazioni x_1, ..., x_n da f
# Approssimiamo il valore atteso E[h(X)] = integrale(h(x)*f(x)*dx) con
# h_n = sum( h(x_i) )/n
# che per la legge dei grandi numeri tende a E[h(X)] per n che tende all'infinito.

# Esempio 1
# Calcolare integrale di g(x) = 4/(1+x^2) tramite integrazione MC
# Campionare i punti per la valutazione da una U(0,1)

set.seed(123)

n = 100
x_ = runif(n)                 
g_ = 4.0 / (1.0 + x_^2)        
I_n = sum(g_)/n
I_n

I_esatto = pi # 4*arctg(1)

errore = abs(I_n-I_esatto)
errore

# Al variare di n:
set.seed(123)
n.max = 1000
n.prove = seq(from=1,to=n.max,by=1) # 1:n.max

x_ = runif(n.max)
g_ = 4/(1+x_^2)
I_n = cumsum(g_)/n.prove

dev.new()
plot(n.prove, I_n, type='l', ylab="Approssimazione", xlab='n')
abline(h=pi, col='red', lty=2, lwd=2)

# Esempio 2
# Calcolare la costante di normalizzazione della densita'
# f(x) = 2 + sin(10*x^2)*exp(x)*sqrt(x) definita in [0, 1]
 
rm(list=ls())
graphics.off()

f<-function(x){
  return(2 + sin(10*x^2)*exp(x)*sqrt(x))
}

x_ = seq(0,1,by=0.01)
dens_ = f(x_)
dev.new()
plot(x_, dens_, xlab='x', ylab='f(x)', 
     main='Distribuzione vera (non normalizzata)', type='l', col='red',
     ylim=c(-1,4.5))

# dobbiamo calcolare
# int_0^1 f(x) dx = int_0^1 f(x)/l(x) l(x) dx

# calcoliamo il valore atteso di f/l rispetto alla distribuzione 
# scegliamo l distribuita come U(0, 1), quindi l(x) = 1
# Al variare di n:

set.seed(123) #riproducibilità

n.max = 2500
n.prove = seq(1,n.max,by=1)
x_unif = runif(n.max)
f_unif = f(x_unif)
I_unif = cumsum(f_unif)/n.prove

dev.new()
plot(n.prove, I_unif, type='l', ylab="costante normalizzazione", xlab='n')

# Cosa succede se considero una densità l(x) diversa dalla uniforme 
# da cui campionare i punti utili alla valutazione? 

# Devo scegliere una densità definita in [0, 1]
# Esempio: scegliamo l(x) una particolare distribuzione distribuzione triangolare

install.packages('triangle')
library(triangle)

dev.new()
plot(x_, dtriangle(x_, a=0, b=1, c=0.1),
     xlab='x', ylab='f(x)', main='Distribuzione Triangolare', type='l', col='red')

x_tri = rtriangle(n.max, a=0, b=1, c=0.1)
f_tri = f(x_tri)/dtriangle(x_tri, a=0, b=1, c=0.1)
I_tri = cumsum(f_tri)/n.prove

dev.new()
plot(n.prove, I_tri, type='l', ylab="costante normalizzazione", xlab='n')

# Confrontiamo velocità di convergenza 
dev.new()
plot(n.prove, I_tri, type='l', xlab='n', ylim=c(min(I_unif,I_tri), max(I_unif,I_tri)))
lines(n.prove, I_unif, col='red')
legend("topright", legend=c("Triangolare", "Uniforme"),
       lwd=2, lty=1 , col=c("black", "red"))

# dovremmo scegliere una densità l(x) che sia il più proporzionale possibile
# alla funzione f della quale vogliamo calcolare l'integrale!
# La densita' triangolare centrata in 0.1 non è sicuramente la scelta ottimale.
# Nella pratica, puo' non essere facile trovare una densità simile alla f 
# dalla quale sappiamo campionare

# confrontiamo gli errori
sd(I_unif)/sqrt(n.max)
sd(I_tri)/sqrt(n.max)

# Esempio 3
# Sia X una v.a. continua con densità di probabilità 
# f(x) = 1/sqrt(2*pi)*1/(x*(1-x))*exp(-(log(x/(1-x))-1)^2/2) definita in (0, 1)
# calcolare, tramite approssimazione MC, media, varianza e funzione di ripartizione

rm(list=ls())
graphics.off()

# Data la seguente funzione di densita'
f<-function(x){
  return(1/sqrt(2*pi)*1/(x*(1-x))*exp(-(log(x/(1-x))-0.5)^2/2))
}
x_ = seq(0,1,by=0.01)
dens_ = f(x_)
plot(x_, dens_, xlab='x', ylab='f(x)', main="Densità", type='l', lwd=2, col='red')

# stimiamo la media
# integrale_0^1 (x f(x)/l(x) l(x) dx)
# scegliamo l(x) uniforme quindi l(x) = 1

set.seed(123) #riproducibilità risultati

N= 10000
x_unif = runif(N)
f_unif = f(x_unif)
media = sum(x_unif * f_unif/1)/N
media

errore = sd(x_unif*f_unif/1)/sqrt(N)
errore

# stimiamo la varianza
# calcolata Var[X] = E[X^2] - E[X]^2
momento_secondo = sum(x_unif^2 * f_unif/1)/N
varianza = momento_secondo - media^2
varianza

sqrt(varianza)

# calcoliamo la funzione di ripartizione
# calcolando l'integrale della densita' da 0 a x

num_points = 100
xgrid = seq(0., 1, length.out = num_points)

rip_func = rep(0, num_points)
x_unif = runif(N, min=0, max=1)

for(i in 1:num_points){
  f_unif = f(x_unif[ x_unif<=xgrid[i]])   # valuto la f in tutti i punti x_unif < x_grid_i
  rip_func[i] = sum(f_unif)/N             # Funzione ripartizione Empirica
}

plot(xgrid, rip_func, xlab='x', ylab='F', 
     main='Funzione di ripartizione stimata', type='l', lwd=2, col='red')

###########################################################
#### 3 - Q-Q PLOT E VERIFICA DELLA NORMALITA' DEI DATI ####
###########################################################
graphics.off()
rm(list=ls())

# Il q-q plot ( quantile-quantile plot ) e' un grafico che permette di visualizzare il buon adattamento
# di una serie di dati ad una distribuzione nota. La distibuzione di riferimento piu' utilizzata e'
# quella normale. In questo caso il q-q plot prende anche il nome di normal probability plot ( NPP ).
# Nel q-q plot sono rappresentati in ascissa i quantili teorici di una normale standard
# e in ordinata i quantili campionari ( che corrispondono ai dati osservati ordinati ).

# Il metodo puo' essere applicato a qualsiasi distribuzione continua,
# sostituendo ai quantili teorici della normale standard
# quelli della funzione di ripartizione appropriata.

# Campioniamo da una variabile aletoria Normale di media e varianza nota
# e costruiamo il q-q plot tramite la sua definizione teorica

set.seed(123) #riproducibilità risultati

mu = 0
sigma = 1
N = 10000 # Simulo N realizzazioni della variabile gaussiana

# Campionamento tramite la funzione 'rnorm'
dati.sim = rnorm( N, mean = mu, sd = sigma )
# 'rnorm' genera N osservazioni indipendenti da una normale
# di media mean e deviazione standard sd

# Costruiamo istogramma per valutare l'andamento della distribuzione dei dati
# campionati rispetto al modello teorico

x = seq(min(dati.sim)-.1, max(dati.sim)+.1, length = 100 )
dens = dnorm(x, mean = mu, sd = sigma )

dev.new()
hist(dati.sim, prob = T, main = 'Istogramma dati campionati da N(0,1)',
     xlab="x", ylab="Densità", ylim=c(0, max(dens)))
lines( x, dens, col = 'red', lwd = 2 )

# Costruiamo in automatico del q-q plot con la funzione 'qqnorm'
dev.new()
qqnorm( dati.sim )
# 'qqnorm' di default rappresenta in ascissa i quantili teorici
# di una normale standard e in ordinata i quantili empirici

qqline( dati.sim, col = 'red', lwd = 2 )
# 'qqline' aggiunge al grafico una retta che approssima il grafico
# dei quantili empirici rispetto ai quantili teorici ( di default e' la
# retta che passa per il primo e per il terzo quartile )


# Campioniamo ora da una gaussiana non standard
mu2 = 5
sigma2 = 2
dati.sim2 = rnorm(N, mean = mu2, sd = sigma2)

# Istogramma
x = seq(min(dati.sim2)-.1, max(dati.sim2)+.1, length = 100)
dens = dnorm(x, mean = mu2, sd = sigma2 )

dev.new()
hist(dati.sim2, prob = T, main = 'Istogramma dati campionati da N(5,4)',
     xlab="x", ylab="Densità", ylim=c(0,max(dens)))
lines( x, dens, col = 'red', lwd = 2 )

# q-q plot
dev.new()
qqnorm( dati.sim2 )
qqline( dati.sim2, col = 'red', lwd = 2 )
abline( 0, 1, lwd = 2, col = 'green', lty = 2 )
# qqline non coincide con bisettrice del primo e terzo quadrante


# Standardizziamo i dati: ( dati-media )/devstd

dati.stand = ( dati.sim2 - mean( dati.sim2 ) )/sd( dati.sim2 )

dev.new()
qqnorm( dati.stand )
qqline( dati.stand, col = 'red', lwd = 2 )
abline( 0, 1, col = 'green', lty = 3, lwd = 3 )

# Per verificare la normalita' dei dati possiamo standardizzare i dati e confrontare
# l'andamento del qqplot con la bisettrice, oppure semplicemente verificare che il qqplot
# abbia un andamento lineare


#### Q-Q PLOT DI DATI PROVENIENTE DA DISTRIBUZIONI NOTE NON GAUSSIANE ####

rm(list=ls())
graphics.off()

set.seed(123) #riproducibilità risultati

# Osservazioni iid da una normale standard
z = rnorm( 1000 )

# Osservazioni iid da una U( -2, 2 )
u = runif( 1000, -2, 2 )

# Osservazioni iid da una Exp( 1/3 )
g = rexp( 1000, 1/3 )

x = seq(from=-5, to=5, length=1000)

dev.new()
par(mfrow= c(2,3))
# Normale
plot(x, dnorm(x), type="l", lwd=2, col = 2,
     ylab = "densità", main = 'Normale')

# Uniforme
plot(x, dnorm(x), type="l", lwd=2,
     ylab = "densità", main = 'Uniforme')
lines(x, dunif(x,min=-2,max=2), lwd=2, col = 2)
legend(1, 0.4, c("N(0,1)", "U(-2,2)"), col= c(1, 2), 
       lwd = 4, x.intersp = 0.5, seg.len=0.6)

# Esponenziale
plot(x, dnorm(x), type="l", lwd=2,
     ylab = "densità", main = 'Esponenziale')
lines(x, dexp(x,1/3), lwd=2, col = 2)
legend(1, 0.4, c( "N(0,1)", "Exp(1/3)" ), col = c(1, 2), 
       lwd = 4, x.intersp = 0.5, seg.len=0.6)

# QQ-plot
# Normale
qqnorm(z)
qqline(z, col= 'red', lwd=2)
# Uniforme
qqnorm(u)
qqline(u, col= 'red', lwd=2)
# Esponenziale
qqnorm(g)
qqline(g, col= 'red', lwd=2)

# UNIFORME
# Quando i dati provengono da una distribuzione con code meno pesanti (più basse)
# di quelle della gaussiana otteniamo un qq-plot a forma di S,
# con parte sinistra piegata verso l'alto e parte destra piegata verso il basso

# ESPONENZIALE
# Quando i dati provengono da una distribuzione asimmetrica a dx
# otteniamo un qq-plot a forma di mezza U ( funzione convessa )
# Se i dati provenissero da una distribuzione asimmetrica a sx
# otterremmo un qq-plot a forma di radice quadrata


#############################################
#### 4 - LEGGE DEI GRANDI NUMERI ( LGN ) ####
#############################################
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

#################################################
#### 5 - TEOREMA CENTRALE DEL LIMITE ( TCL ) ####
#################################################
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


# Oltre al confronto qualitativo tra gli istogrammi, la bonta' dell'adattamento
# della legge della somma alla normale puo' essere valutata anche con un q-q plot.
# Tracciamo dunque il q-q plot della somma di n Bernoulli per ciascun valore
# di n considerato, e vediamo come l'adattamento alla normale migliori al crescere di n.

dev.new()
par( mfrow = c( 2, 2 ) )
qqnorm( dati[,1], main = 'TCL applicato a somma di Bernoulli\n n = 5' )
qqline( dati[,1], lwd = 2, col = 'red' )
qqnorm( dati[,2], main = 'TCL applicato a somma di Bernoulli\n n = 50' )
qqline( dati[,2], lwd = 2, col = 'red' )
qqnorm( dati[,3], main = 'TCL applicato a somma di Bernoulli\n n = 500' )
qqline( dati[,3], lwd = 2, col = 'red' )
qqnorm( dati[,4], main = 'TCL applicato a somma di Bernoulli\n n = 5000' )
qqline( dati[,4], lwd = 2, col = 'red' )

# Si noti che per n grande risulta sempre meno evidente 
# la distinzione fra l'andamento dei quantili empirici della somma di Bernoulli 
# e quelli di una distribuzione gaussiana.


# Consideriamo la Media Campionaria delle n Bernoulli.
# Verifichiamo che al crescere di n l'approssimazione della Media Campionaria
# con una Normale N(p,p(1-p)/n).

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
