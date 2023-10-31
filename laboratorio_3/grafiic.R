
N = 5000
u = runif(N)
x.samples = tan(pi*(u - 0.5))

x_ = seq(-1000,1000,by=0.1)
f = 1/pi*(1/(1+x_^2))

# Zoom intorno a 0
x_ = seq(-10,10,by=0.1)
f = 1/pi*(1/(1+x_^2))

pdf("F_inversa.pdf")
hist(x.samples[x.samples>-5 & x.samples<5], prob=T,
     main="Istogramma dati campionati",
     xlab= "dati campionati", ylab="Densità",yaxt="n", xaxt="n",frame.plot = F)
lines(x_, f, lwd=4, col='red')
axis(2, cex.axis=1.5)
axis(1, cex.axis=1.5)
dev.off()


pdf("unit_circle.pdf")
plot(xx, yy, type="l", lwd=3, xlab="", ylab="", main="",
     xlim=c(-1,1),ylim=c(-1,1),asp = 1, xaxt="n", yaxt="n", frame.plot = F)
points(xx,-yy, type="l", lwd=3)
points(xx,-yy, type="l", lwd=3)
points(xx, rep(-1,times=length(xx)), col="red", type="l", lwd=4)
points(xx, rep(1,times=length(xx)), col="red", type="l", lwd=4)
points(rep(-1,times=length(xx)), xx, col="red", type="l", lwd=4)
points(rep(1,times=length(xx)), xx, col="red", type="l", lwd=4)


plot(xx, yy, type="l", lwd=3, xlab="", ylab="", main="",
     xlim=c(-1,1),ylim=c(-1,1),asp = 1, xaxt="n", yaxt="n", frame.plot = F)
points(xx,-yy, type="l", lwd=3)
points(xx,-yy, type="l", lwd=3)
points(xx, rep(-1,times=length(xx)), col="red", type="l", lwd=4)
points(xx, rep(1,times=length(xx)), col="red", type="l", lwd=4)
points(rep(-1,times=length(xx)), xx, col="red", type="l", lwd=4)
points(rep(1,times=length(xx)), xx, col="red", type="l", lwd=4)

M = 50  # iterazioni MC

for(i in 1:M){
  cat(paste("##### \t Lancio ",i,"\t #####\n",sep=""))
  for(j in 1:1){
    
    counts[j] = throw_dart(player = players[j], count=counts[j], color=colors[j])
  }
  cat(paste("\t",scores(players,counts),"\n",sep=""))
  cat("\n")
}


dev.off()


f<-function(x){
  return(1/sqrt(2*pi)*1/(x*(1-x))*exp(-(log(x/(1-x))-0.5)^2/2))
}

x_ = seq(0,1,by=0.01)
dens_ = f(x_)

#x_corse = runif(N)
x_corse = seq(0.1,1,0.075)
N = length(x_corse)
a = x_corse - 1/(2*N)
b = x_corse + 1/(2*N)
rettangoli = matrix(0,nrow=N, ncol=5) 
for(i in 1:N) rettangoli[i,] = seq(from=a[i], to=b[i], length.out=5)

pdf("funzione.pdf")
plot(x_, dens_, xlab='', ylab='',main="g(x)", type='l', lwd=3, 
     frame.plot =F,yaxt="n", xaxt="n",cex.main=2)
axis(2, cex.axis=1.5)
axis(1, at=c(0,1), labels=c("a","b"),cex.axis=1.5)

plot(x_, dens_, xlab='', ylab='',main="g(x)", type='l', lwd=3, 
     frame.plot =F,yaxt="n", xaxt="n",cex.main=2)
points(x_corse, f(x_corse), pch=16, col="red", cex=1.5)
axis(2, cex.axis=1.5)
axis(1, at=c(0,1), labels=c("a","b"),cex.axis=1.5)

for(i in 1:N){
lines(rettangoli[i,], rep(f(x_corse[i]),times=5), lwd=2, col="red")
lines(rep(a[i], times=5), seq(from=0, to=f(x_corse[i]), length.out=5), lwd=2,col="red")
lines(rep(b[i], times=5), seq(from=0, to=f(x_corse[i]), length.out=5), lwd=2,col="red")
}
dev.off()

pdf("accettazione_rifiuto.pdf")
f<-function(x){
  return(2 + sin(10*x^2)*exp(x)*sqrt(x))
}

set.seed(0)
X = 0.25
U = 1.   
x_ = seq(0,1,by=0.01)
dens_ = f(x_) 
M = 4.4
plot(x_, dens_, xlab='', ylab="", lwd=4,
     main='Densità (non normalizzata)', type='l'
     ,frame.plot =F,
     yaxt="n", xaxt="n")
points(X,U, pch=16, col="red", cex=4)
axis(1, at=c(0,1, X), labels=c("a","b","x"),cex.axis=2)
axis(2, at=c(M,U), labels=c("M","u"),cex.axis=2,las=2)
abline(h=M, lty=2, col='blue',lwd=4)
dev.off()

mu = 0
sigma = 1
N = 10000
pdf("qqplot.pdf")
dati.sim = rnorm( N, mean = mu, sd = sigma )
x = seq(min(dati.sim)-.1, max(dati.sim)+.1, length = 100 )
dens = dnorm(x, mean = mu, sd = sigma )

hist(dati.sim, prob = T, main = 'Istogramma dati campionati da N(0,1)',
     xlab="x", ylab="Densità", ylim=c(0, max(dens)))
lines( x, dens, col = 'red', lwd = 4)


qqnorm( dati.sim )
qqline(dati.sim, lwd=4,col="red")
dev.off()


# campionamento da nota

n = 20
x_unif = runif(n) # n realizzazioni di una U(0,1)
x_ = seq(from=0, to=1, length.out=100)

pdf("Campionamento_Note.pdf")

plot(x_, dunif(x_), type="l", lwd=4, 
     xlab="x",ylab="", main="U(0,1)", ylim=c(0,1.2),cex.lab=2, cex.main=2,
     yaxt="n", xaxt="n",frame.plot = F)
points(x_unif, rep(0,n), pch=16, col="red",cex=2)
axis(1, cex.axis=2)
axis(2, cex.axis=2)

set.seed(0) # riproducibilità

n = 20
x_norm = rnorm(n) # n realizzazioni di una N(0,1)
x_ = seq(from=-5, to=5, length.out=100)

plot(x_, dnorm(x_), type="l", lwd=4, 
     xlab="x",ylab="", main="N(0,1)", ylim=c(0, max(dnorm(x_))),cex.lab=2, cex.main=2,
     yaxt="n", xaxt="n",frame.plot = F)
points(x_norm, rep(0,n), pch=16, col="red",cex=2)
axis(1, cex.axis=2)
axis(2, cex.axis=2)

set.seed(0) # riproducibilità

n = 20
lambda = 1
x_exp = rexp(n, lambda)     # n realizzazioni di una Exp(lambda)
x_ = seq(from=0, to=10, length.out=100)

plot(x_, dexp(x_,lambda), type="l", lwd=2, 
     xlab="x",ylab="", main="Exp(1)",cex.lab=2, 
     cex.main=2,yaxt="n", xaxt="n",frame.plot = F)
points(x_exp, rep(0,n), pch=16, col="red")
axis(1, cex.axis=2)
axis(2, cex.axis=2)
dev.off()

pdf("Pi_LGN.pdf")
plot(1:M, P_m, type="l", main=TeX("Stima $\\pi$ al variare di $n$"),
     xlab = "numero lanci", yaxt="n", xaxt="n", ylab="",
     cex.lab=2, cex.main=2, lwd=3, frame.plot = F)
abline(h=pi, col="red", lty=2, lwd=4)
axis(2, at=c(pi), labels = TeX("$\\pi$"), las=2, cex.axis=2)
axis(2, cex.axis=1.5,las=2)
axis(1, cex.axis=1.5)
dev.off()
