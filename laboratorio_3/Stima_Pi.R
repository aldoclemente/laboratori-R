library(stringr)
rm(list=ls())
graphics.off()
throw_dart = function(player, count, color){
  
  coords = runif(n=2,min=-1,max=1)
  
  if( norm(coords, type="2")^2 <=1 ){
    count = count + 1
  }
  
  points(coords[1], coords[2], col=color, pch="*",cex=1.25)
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

xx = seq(from=-1, to=1, by=0.0125)
yy = sqrt(1-xx^2)

players = c("Aldo", "Andrea", "Michele")
colors = c("red2","green3","blue2")

counts_ = rep(0, length(players))


plot(xx, yy, type="l", lwd=2.5, xlab="x", ylab="y", main="",
     xlim=c(-1,1),ylim=c(-1,1),asp = 1)
points(xx,-yy, type="l", lwd=2.5)
counts = rep(0, length(players))

M = 1000

for(i in 1:M){
  cat(paste("##### \t Lancio ",i,"\t #####\n",sep=""))
  for(j in 1:length(players)){

    if(i<=4){
      readline(prompt=paste(players[j],", premi [enter] per lanciare", sep=""))
    }
    counts[j] = throw_dart(player = players[j], count=counts[j], color=colors[j])
  }
  cat(paste("\t",scores(players,counts),"\n",sep=""))
  cat("\n")
}

Area_ = 4.
P_ = counts/M * Area_
P_

### Stima Pi al variare delle iterazioni MC ###

M = 5e4    # iter. MC
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

plot(1:M, P_m, type="l", xlab = "Iterazioni", ylab="")
abline(h=pi, col="red", lty=2)

### Test t ###

N = 100
P_ = matrix(nrow=N,ncol=1)
M = 1e4

Area_ = 4.

for(i in 1:N){
  coords = cbind( runif(M, min=-1, max=1), runif(M,min=-1,max=1))
  count = apply(coords, MARGIN=1, #Righe
                FUN= function(x){return (norm(x, type="2")<=1)})
  count = sum(count)
  P_[i] = count/M * Area_
  
}

summary(P_)

qqnorm(P_,cex=1.5)
qqline(P_, col="red",lwd=1.5)
shapiro.test(P_)

alpha = 0.05
Test = t.test(P_, alternative = "two.sided", mu=pi, conf.level = 1-alpha )
Test$conf.int


# Esercizio Casa: Stima Numero di Nepero con MC ( e )

fun = function(x){
  return(exp(x) + 1.)
}

M = seq(from=1, to=1e4, by=1)
xx = runif(M[length(M)])

yy = cumsum(fun(xx))/M

plot(M, yy, type="l")
abline(h=exp(1), col="red", lty=2)
