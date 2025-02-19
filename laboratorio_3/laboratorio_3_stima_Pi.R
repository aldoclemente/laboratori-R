{
library(stringr)
library(latex2exp)
rm(list=ls())
graphics.off()
set.seed(314159)
throw_dart = function(player, count, color){

  coords = runif(n=2,min=-1,max=1)

  if( norm(coords, type="2")^2 <=1 ){
    count = count + 1
  }

  points(coords[1], coords[2], col=color, pch="*",cex=2)
  return(count)

}

scores = function(players, counts){

  string_length = apply(as.matrix(players), MARGIN=1, FUN=function(x){return(nchar(x))})

  max_ = max(string_length)

  players_ = str_pad(players, width=max_, side="right")
  for(i in 1:length(players)){
     players_[i] = paste(players_[i], counts[i], sep=" ")
   }
  return(players_)
}
}
xx = seq(from=-1, to=1, by=0.0125)
yy = sqrt(1-xx^2)

players = c("Player 1", "Player 2")
colors = c("red2","blue2")

{
plot(xx, yy, type="l", lwd=2.5, xlab="", ylab="", main="",
     xlim=c(-1,1),ylim=c(-1,1),asp = 1, xaxt="n", yaxt="n", frame.plot = F)
points(xx,-yy, type="l", lwd=2.5)
points(xx,-yy, type="l", lwd=2.5)
points(xx, rep(-1,times=length(xx)), col="red", type="l", lwd=3)
points(xx, rep(1,times=length(xx)), col="red", type="l", lwd=3)
points(rep(-1,times=length(xx)), xx, col="red", type="l", lwd=3)
points(rep(1,times=length(xx)), xx, col="red", type="l", lwd=3)
}

counts = rep(0, length(players))
M = 500  # iterazioni MC

for(i in 1:M){
  cat(paste("##### \t Lancio ",i,"\t #####\n",sep=""))
  for(j in 1:length(players)){

    if(i<4){
      readline(prompt=paste(players[j],", premi [enter] per lanciare", sep=""))
    }
    counts[j] = throw_dart(player = players[j], count=counts[j], color=colors[j])
  }
  cat(paste("\t",scores(players,counts),"\n",sep=""))
  cat("\n")
}

Area_ = 4.            # Area quadrato [-1,1] x [-1,1]
P_ = counts/M * Area_
P_

# errore assoluto
abs(P_-pi)

### Stima Pi al variare delle iterazioni MC ###

rm(list=ls())

M = 2.5e4    # iter. MC
Area_ = 4. # Area quadrato [-1,1] x [-1,1]

{
  start = Sys.time()
  count = rep(0, M)
  
  coords = cbind(runif(M, min=-1, max=1), runif(M,min=-1,max=1))
  for(i in 1:M){
    if(norm(coords[i,],type="2")<=1) count[i] = 1
  }
  
  P_m = cumsum(count)/1:M * Area_
  cat(paste("Esecuzione: ",
            difftime(Sys.time(),start,units="secs"), "secs", sep=" "))
}

x11()
#pdf("Pi_LGN.pdf")
plot(1:M, P_m, type="l", main=TeX("Stima $\\pi$ al variare di $n$"),
     xlab = "numero lanci", yaxt="n", xaxt="n", ylab="",
     cex.lab=2, cex.main=2, lwd=3)
abline(h=pi, col="red", lty=2, lwd=4)
axis(2, at=c(pi), labels = TeX("$\\pi$"), las=2, cex.axis=2)
axis(1, cex.axis=2)
#dev.off()
### Stima Pi al variare per diversi valori di iterazioni MC ###

rm(list=ls())
M = c(1e1, 1e2, 1e3) #, 1e4)    # iter. MC

Area_ = 4. # Area quadrato [-1,1] x [-1,1]

N = 100     # ripetizioni dello stesso esperimento
P_m = matrix(0, nrow=N, ncol=length(M))

for(j in 1:length(M)){
  for(i in 1:N){
    count = rep(0, M[j])
    
    coords = cbind(runif(M[j], min=-1, max=1), runif(M[j],min=-1,max=1))
    
    count = apply(coords, MARGIN=1, FUN=
                    function(x){
                      if(norm(x,type="2")<=1)
                        return(1)
                      else
                        return(0)
                    }
    )
    
    # for(k in 1:M[j]){
    #   if(norm(coords[k,],type="2")<=1) count[k] = 1
    # }
    
    P_m[i,j] = sum(count)/M[j] * Area_
  }
}

{
  x11()
  par(mfrow=c(3,1))
  for( j in 1:length(M)){
    
    x = seq(min(P_m[,j]), max(P_m[,j]), length.out=100)
    
    density = dnorm(x, mean = pi, sd = sd(P_m[,j]))
    hist(P_m[,j], probability=T, plot=T,
         main = paste("TCL applicato a proporzione,","n =",M[j],sep=" "),
         xlab = 'somma di n bernoulli',
         ylim = c(0,max(max(density),max(hist(P_m[, j], plot=F)$density))))
    lines( x, density, col = 'red', lwd = 2 )
  }
}

x11()
plot(1:M, P_m, type="l", main='Simulazione: la legge dei grandi numeri',
     xlab = "numero lanci", yaxt="n", xaxt="n", ylab="",
     cex.lab=2, cex.main=2)
abline(h=pi, col="red", lty=2, lwd=2)
axis(2, at=c(pi), labels = TeX("$\\pi$"), las=2, cex.axis=2)
axis(1, cex.axis=2)

# Ripetere N volte la stima di Pi tramite Metodo MC #

N = 50    # numero ripetizione esperiemento
P_ = matrix(nrow=N,ncol=1)
M = 1e4    # iterazioni MC
Area_ = 4.

for(i in 1:N){
  
  coords = cbind( runif(M, min=-1, max=1), runif(M,min=-1,max=1))
  count = 0
  for(j in 1:M){
    if(norm(coords[j,],type="2")<=1) count = count + 1
  }
  
  # oppure
  
  # count = apply(coords, MARGIN=1, #Righe
  #               FUN= function(x){return (norm(x, type="2")<=1)})
  # count = sum(count)
  
  P_[i] = count/M * Area_
  
}

summary(P_)

# per TCL... :)
qqnorm(P_,cex=1.5)
qqline(P_, col="red",lwd=1.5)
shapiro.test(P_)

alpha = 0.05
Test = t.test(P_, alternative = "two.sided",
              mu=pi, conf.level = 1-alpha )
Test$conf.int


# Esercizio 1: Stima Numero di Nepero con MC

fun = function(x){
  return(exp(x) + 1.)
}
# int_0^1 fun(x) dx = e

M = seq(from=1, to=1e4, by=1)
xx = runif(M[length(M)])

yy = cumsum(fun(xx))/M

plot(M, yy, type="l", ylab="", xlab="Iterazioni")
abline(h=exp(1), col="red", lty=2)

# Esercizio 2: Calcola integrale di g(x) = 1/2 f(x)  dove f(x) è la distribuzione di probabilità di una
#              Normale standard. Campiona i punti utili alla valutazione di g(x) sia da una Uniforme sia da 
#              una Gaussiana N(0,1). Confronta i risultati

g = function(x){
  return(1/2 * dnorm(x) )
}

set.seed(314159)
x = seq(-5,5,length.out=1000)
plot(x,g(x), type="l", lwd=2, main="g(x)", xlab="x", ylab="", 
     ylim=c(0, max( max(g(x)),dnorm(x)) ))
lines(x,dnorm(x), col="red", lwd=2)

n.max = 1000
x_unif = runif(n.max,-5,5)
x_norm = rnorm(n.max)

n.prove = 1:n.max
I_unif = 10 * cumsum(g(x_unif))/n.prove
I_norm = cumsum(g(x_norm)/dnorm(x_norm)) /n.prove

plot(n.prove,I_unif, type="l", lwd=2, xlab="Iterazioni", ylab="Approssimazione",
     main="Campionamento Uniforme vs Campionamento Normale")
points(n.prove, I_norm, type="l", lwd=2, col="red")
legend("topright", c("Uniforme","Normale"), lty=1, lwd=3,col=c("black","red"))

# Esercizio 3: Verifica andamento somma di Unif(0,1) al variare 

# Altro esempio

# Uniforme(0,1)

n = c(5,10,50,100)
N = 500          # Ripeto campionamento N volte per generare osservazioni della media campionaria
mean_ = matrix(0, nrow=N,ncol=length(n))

for(j in 1:length(n)){
  for(i in 1:N){
    x_unif = runif(n[j])
    mean_[i,j] = mean(x_unif)
  }
}

mu = 1/2    # (a+b)/2
var = 1/12  # (b-a)^2 / 12
x_ = seq(from=0,to=1, length.out=1000)

x11()
par(mfrow=c(2,2))
for(i in 1:length(n)){
  density = dnorm(x_, mean=mu, sd=sqrt(var)/sqrt(n[i]))
  hist(mean_[,i], probability = T,
       xlim = c(0,1),
       ylim = c( 0, max(max(density),max(hist(x_mean[,i],plot=F)$density))),
       main=paste('TCL: Media Campionaria di Uniformi\n','n = ', n[i],sep=""))
  lines(x_, density,
        col="red", lwd=2)
}

x11()
par(mfrow=c(2,2))
for(i in 1:length(n)){
  qqnorm(mean_[,i],
         main = paste('TCL: Media Campionaria di Uniformi\n','n = ', n[i],sep=""),
         ylim=c( min(apply(x_mean, MARGIN=2, FUN=function(x){return(quantile(x))})),
                 max(apply(x_mean, MARGIN=2, FUN=function(x){return(quantile(x))}))) )
  qqline(x_mean[,i], lwd = 2, col = 'red' )
}

