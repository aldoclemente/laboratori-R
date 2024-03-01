## 1 - CAMPIONAMENTO DI VARIABILI ALEATORIE ------------------------------------

rm( list = ls() )
graphics.off()

### METODO DELLA FUNZIONE INVERSA ----------------------------------------------

### Esempio 1: Campionamento da v.a. Continua
# Vogliamo campionare da una v.a. che ha funzione di densità
# f(x) = 
# Calcoliamo la funzione di ripartizione
# F(x) = 2x^2 se x in [0, 1/2]; (1-2*(1-x)^2) in (1/2, 1]

# Calcoliamo l' inversa della funzione di ripartizione
# F^-1(u) = tan(pi(u - 1/2)) , u in (0, 1)
# Se U ~ U[0,1] -> X = F^-1(U) ~ f
# Campioniamo N realizzazioni da una distribuzione uniforme in [0,1]
# e mostriamo che l'istogramma di x = F^-1(u) approssima
# la densita' f.

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

f = function(x){
  res = vector(mode="numeric", length = length(x))
  for(i in 1:length(x)){
    if( x[i] >=0 & x[i] < 1/2 ){
      res[i] = 4 * x[i]
    }else if( x[i] >1/2 & x[i] <= 1 ){
      res[i] = 4 * ( 1 - x[i] )  
    }
  }
  return(res)
}
set.seed(0) # riproducibilità

N = 5000
u = runif(N)

x.samples = F_inv(u)

x = seq(0,1,length=1000)

# Istogramma realizzazioni con vera Densità
dev.new()
hist(x.samples, prob=T,
     main="Istogramma realizzazioni", 
     xlab= "realizzazioni", ylab="Densità")
lines(x, f(x), lwd=2, col='red')
# invece di "lines" avremmo potuto utilizzare: 
# points(x_, f, type="l", lwd=2, col="red")

### ACCEPT-REJECT METHOD -------------------------------------------------------

# Esempio 

# Vogliamo campionare da v.a. con densità di probabilità
# f(x) = (2 + sin(10*x^2)*exp(x)*sqrt(x))/ integrale_0^1(2 + sin(10*x^2)*exp(x)*sqrt(x) dx)
# definita in [0,1].

f = function(x){
  res = vector(mode="numeric", length = length(x))
  for(i in 1:length(x)){
    if( x[i] >=0 & x[i] < 1/2 ){
      res[i] =  2 * (4 * x[i])
    }else if( x[i] >1/2 & x[i] <= 1 ){
      res[i] = 2 * (4 * ( 1 - x[i] ))  
    }
  }
  return(res)
}

x = seq(0,1,length=1000)

dev.new()
plot(x, f(x), xlab='x', ylab='f(x)', ylim =c(0,4.1),
     main='Distribuzione vera (non normalizzata)', type='l', col='red')

# Notiamo che f(x) <= M con M = 4.1
M = 4.1
abline(h=M, lty=2, col='blue')

# Accept-reject loop
# calcoliamo l'efficienza

set.seed(0) # riproducibilità

N = 10000  # numero di realizzazioni
accettati <- vector(mode="numeric", length=N)
rifiutati = 0
for(i in 1:N){
  accept = FALSE
  while(accept==FALSE){
    xx = runif(n=1, min=0, max=1) # realizzazione da U[0,1]
    u = runif(n=1, min=0, max=M) # realizzazione da U[0,M]
    if(u<=f(xx)){
      accettati[i] = xx            # se vale la condizione accetto
      accept = TRUE                # il valore campionato
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
lines(x, f(x), xlab='x', ylab='f(x)', 
      type='l', col='red')
abline(h=M, lty=2, col='blue')

# L'istogramma delle realizzazioni ottenute approssima la vera densità
# che non conosciamo in forma chiusa perchè non non sappiamo normalizzare la f(x) 

## 2 - INTEGRAZIONE MONTE-CARLO ----------------------------------------------

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
x = runif(n)                 
g = 4.0 / (1.0 + x^2)        
I_n = sum(g)/n
I_n

I_esatto = pi # 4*atan(1)

errore = abs(I_n-I_esatto)
errore

# Al variare di n:
set.seed(123)
n.max = 1000
n.prove = seq(from=1,to=n.max,by=1) # 1:n.max

x = runif(n.max)
g = 4/(1+x^2)
I_n = cumsum(g)/n.prove

dev.new()
plot(n.prove, I_n, type='l', ylab="Approssimazione", xlab='n')
abline(h=pi, col='red', lty=2, lwd=2)

# ESERCIZI ---------------------------------------------------------------------

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

# ----

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