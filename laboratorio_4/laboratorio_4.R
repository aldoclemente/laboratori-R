#'###########################################################'#
#'#####                 LABORATORIO 4                   #####'#
#'#####              VERIFICA di NORMALITA'             #####'#
#'#####    INFERENZA E TEST PER UNA E DUE POPOLAZIONI   #####'#
#'###########################################################'#

# IMPOSTARE LA WORKING DIRECTORY (CARTELLA DI LAVORO): -------------------------
# Da interfaccia:
# 1.
# 'Session' -> 'Set Working Directory' -> 'Choose Directory' -> ...

# 2. 
# 'Session' -> 'Set Working Directory' -> 'To Source File Location'

# Da console:
# setwd( 'path/to/file' )

if(!require(BSDA)) install.packages("BSDA")  
if(!require(EnvStats)) install.packages("EnvStats")

library(BSDA)     
library(EnvStats)

# 1 - VERIFICA di NORMALITA (QQ-plot e Test di Shapiro-Wilk) -------------------
 
set.seed(0) # riproducibilità risultati
mu = 5
sigma = 2
n = 30    

# n realizzazioni della variabile gaussiana
x_norm = rnorm(n, mean = mu, sd = sigma)

# Q-Q plot
#   - in ascissa: quantili teorici di N(0,1)
#                 -> distribuzione cumulata della normale
#   - in ordinata: quantili empirici
#                 -> distribuzione cumulata della variabile osservata
qqnorm(x_norm, pch=16)
qqline(x_norm, lty=2, lwd=2, col="red")

# Test di Shapiro-Wilk 
# H_0: Popolazione Normale vs H_1: Popolazione NON Normale

shapiro.test(x_norm) # p-value >> 0.05 -> NON rifiuto H_0

## DATI PROVENIENTE DA DISTRIBUZIONI NOTE NON GAUSSIANE ------------------------

rm(list=ls())

set.seed(0) #riproducibilità risultati
n = 1000

# Osservazioni da una normale standard
x_norm = rnorm(n)

# Osservazioni da una U( -2, 2 )
x_unif = runif(n, -2, 2 )

# Osservazioni iid da una Exp( 1/3 )
x_exp = rexp(n, 1/3 )

x = seq(from=-5, to=5, length=1000)

{
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
qqnorm(x_norm, pch=16)
qqline(x_norm, col= 'red', lwd=2)
# Uniforme
qqnorm(x_unif, pch=16)
qqline(x_unif, col= 'red', lwd=2)
# Esponenziale
qqnorm(x_exp, pch=16)
qqline(x_exp, col= 'red', lwd=2)
}

shapiro.test(x_norm)
shapiro.test(x_unif) # p-value < 0.05 -> Rifiuto H_0
shapiro.test(x_exp)  

# 2 - IC E TEST PER LA MEDIA DI UNA POPOLAZIONE -------------------------------

# Esercizio 1 
# Carichiamo i dati contenuti nel file lampadine.txt:
# Essi rappresentano il tempo di vita (espresso in ore) di un campione di 
# 20 lampadine da 75 watt. 

# 1. Verificare la normalità dei dati
#
# 2.  Supporre che la deviazione standard sia nota e pari a 25 ore.
#     E' ragionevole supporre che la media di vita delle lampadine sia pari a 1000 ore?
#     Fissare un livello di significatività pari al 5%. 
#     2.1 Costruire il test di ipotesi
#     2.2 Costruire IC al 95%
#     2.3 Calcolare il p-value del test. 

# 3. Costruire i Test Unilateri per la media di vita delle lampadine 
#    al livello di significatività 0.05

# Importiamo il dataset
dati = read.table('lampadine.txt', dec='.', header=T)
head(dati)

dim(dati)
names(dati)

# 1. Verifichiamo Normalità

dev.new()
qqnorm(dati$tempo_vita, pch=16)
qqline(dati$tempo_vita, lty=2, lwd=3, col="red")

# Tramite Shapiro test 
# H_0: Popolazione Normale vs H_1: Popolazione NON Normale
shapiro.test(dati$tempo_vita) 
# p-value >> 0.05 -> E' ragionevole assumere normalità

# 2. 
# 2.1 Consideriamo il test:
# H_0: mu = 1000     vs     H_1: mu != 1000

# Rifiuto H_0 se abs(z_0) = abs( (x_mean - mu0)/(sigma/sqrt(n)) ) > z_{1-alpha/2}
alpha = 0.05
n = nrow(dati)
mu0 = 1000
x_mean = mean(dati$tempo_vita)
sigma = 25
z.quantile = qnorm(1-alpha/2)

z0 = (x_mean - mu0)/(sigma/sqrt(n))  

abs(z0) > z.quantile # RIFIUTO H_0

# Costruiamo IC al livello 95%
IC = c( x_mean - z.quantile * sigma/sqrt(n), x_mean + z.quantile * sigma/sqrt(n))
IC  # mu0 non è all'interno di IC, coerentemente al risultato del test

# Calcoliamo p-value del Test

p.value = 2 * (1 - pnorm( abs(z0) ))
p.value   # Rifiuto H_0 per ogni alpha < p-value 

# BSDA::z.test
z.test(x = dati$tempo_vita, alternative = "two.sided", 
       mu = mu0, sigma.x = sigma, conf.level = 1-alpha)

res = z.test(x = dati$tempo_vita, alternative = "two.sided", 
              mu = mu0, sigma.x = sigma, conf.level = 1-alpha)
names(res)

res$statistic  # z0
res$conf.int   # IC 
res$p.value    # p-value
res$estimate   # x_mean

# 3. 

# Consideriamo il test unilatero:
# H_0: mu = 1000     vs     H_1: mu > 1000
# Rifiuto H_0 se z0 =  (x_mean - mu0)/(sigma/sqrt(n)) > z_{1-alpha}
z.quantile = qnorm(1-alpha)
z0 > z.quantile # Rifiuto H_0

z.test(x = dati$tempo_vita, alternative = "greater",
       mu = mu0, sigma.x = sigma, conf.level = 1-alpha)
# p.value << 0.05 -> Rifiuto H_0

# Consideriamo il test unilatero:
# H_0: mu = 1000     vs     H_1: mu < 1000
# Rifiuto H_0 se z0 = (x_mean - mu0)/(sigma/sqrt(n)) < z_{alpha} = -z_{1-alpha}

z0 < -z.quantile # NON Rifiuto H_0 

z.test(x = dati$tempo_vita, alternative = "less", 
       mu = mu0, sigma.x = sigma, conf.level = 1-alpha)

rm(list=ls())

# Esercizio 2
# Carichiamo i dati contenuti nel file temperatura.txt:
# Essi rappresentano la temperatura corporea (espressa in gradi Fahrenheit)
# di un campione di 130 soggetti, di cui 65 maschi e 65 femmine.
# Si vuole stabilire se la media reale della temperatura corporea della popolazione
# sia 37 gradi Celsius
#  1.1 Costruire il test di ipotesi
#  1.2 Costruire IC al 95%
#  1.3 Calcolare il p-value del test. 

# 2.1 Stabilire se ci sono differenze nella temperatura corporea dovute al
#     sesso del soggetto per mezzo di grafici e di un opportuno test.
# 2.2 Stabilire se la temperatura corporea media delle donne è superiore 
#    alla temperatura corporea media degli uomini (assumendo stessa varianza)


# Importiamo il dataset
dati = read.table('temperatura.txt', dec='.', header=T)
head(dati)

dim(dati)
names(dati)
# Fahrenheit -> Celsius ( C = (F - 32)/1.8 ) 
dati$Temperatura = (dati$Temperatura - 32) / 1.8 

# Verifichiamo la Normalità della v.a. Temperatura

par(mfrow = c(1,2))
hist(dati$Temperatura, prob=TRUE)
qqnorm(dati$Temperatura, pch=16)
qqline(dati$Temperatura, lwd=2, col='red')

shapiro.test(dati$Temperatura)
# p-value > 0.05 -> possiamo assumere che i dati provengano da popolazione normale

# Effettuo un test per verificare l'ipotesi
#       H_0: mu = 37 C     vs     H_1: mu != 37 C

# Varianza incognita -> t-Test

# Rifiuto H_0 se abs(t_0) = abs( (x_mean - mu0)/(s/sqrt(n)) ) > t_{1-alpha/2,n-1}
alpha = 0.05
n = nrow(dati)
mu0 = 37
x_mean = mean(dati$Temperatura)
x_sd = sd(dati$Temperatura)
t.quantile = qt(1-alpha/2, df = n - 1)

t0 = (x_mean - mu0)/(x_sd/sqrt(n))
abs(t0) > t.quantile # RIFIUTO H_0 

# Costruiamo IC al livello 95%
IC = c( x_mean - t.quantile * x_sd/sqrt(n), 
        x_mean + t.quantile * x_sd/sqrt(n))
IC  # mu0 non è all'interno di IC, coerentemente al risultato del test

# Calcoliamo p-value del Test

p.value = 2 * (1 - pt( abs(t0) , df = n - 1))
p.value   # p-value << 0.05 

# Utilizziamo la funzione t.test
help("t.test")
t.test(x = dati$Temperatura, alternative = "two.sided", 
       mu = mu0, conf.level = 1-alpha)

# p-value << 0.05 -> Rifiuto H_0 -> Vera Media != mu0

# 2.1 Rappresentiamo gli istogrammi delle due popolazioni e i boxplot
# e costruiamo un test d'ipotesi per la differenza tra le medie
# delle temperature corporee nelle due sottopopolazioni individuate
# dal sesso.
class(dati$Sesso)
dati$Sesso = as.factor(dati$Sesso)
levels(dati$Sesso)
# esplorazione grafica
dev.new()
par(mfrow=c(1,1))
boxplot(dati$Temperatura ~ dati$Sesso, horizontal=FALSE, 
        main='Boxplot Temperatura', xlab = "Sesso",
        names=c('Donne','Uomini'), col=c('orange','forestgreen'),
        ylab='temperatura corporea [gradi C]')

# Tendenza delle donne ad avere una temperatura corporea più alta rispetto agli uomini.

# verifichiamo la normalità delle due popolazioni
temp.maschi = dati$Temperatura[dati$Sesso == "M"]
temp.femmine = dati$Temperatura[dati$Sesso == "F"]

dev.new()
par(mfrow=c(2,1))
qqnorm(temp.femmine, pch=16, main='Temperatura Femmine')
qqline(temp.femmine, col='orange',lwd=3)
qqnorm(temp.maschi, pch=16, main='Temperatura Maschi')
qqline(temp.maschi, col='forestgreen',lwd=3)

shapiro.test(temp.femmine)  # p-value > 0.05
shapiro.test(temp.maschi)   # p-value > 0.05

# Test d'ipotesi sulla differenza di media tra due popolazioni
#       H_0: mu_F - mu_M = 0    vs     H_1: mu_F - mu_M != 0
# Rifiuto H_0 se abs(t0) = abs( ((mean_F - mean_M))/(sqrt(s2_pool* (1/n_F + 1/n_M))) > t_{1-alpha/2, n_M + n_F -2} 

n_F = length(temp.femmine)
n_M = length(temp.maschi)
mean_F = mean(temp.femmine)
mean_M = mean(temp.maschi)
alpha = 0.05
s2_pool = ((n_F - 1)*var(temp.femmine) + (n_M - 1)*var(temp.maschi))/(n_F + n_M - 2)

t0 = ((mean_F - mean_M))/(sqrt(s2_pool* (1/n_F + 1/n_M)))
t.quantile = qt(1-alpha/2, df = (n_M + n_M - 2))
abs(t0) > t.quantile # Rifiuto H_0

# p.value test
p.value = 2*(1 - pt(abs(t0), df = (n_M + n_M - 2)))
p.value   # coerente risultato test
# utilizziamo la funzione t.test
t.test(temp.femmine, temp.maschi, alternative = "two.sided", 
       paired = FALSE, var.equal = TRUE)

# p-value < 0.05 -> Rifiuto H_0 -> 
# -> al livello 0.05 non c'è evidenza per affermare che esiste una differenza nella
#    media delle due sotto-popolazioni

# 2.2  Test unilatero sulla differenza di media tra due popolazioni
#       H_0: mu_F - mu_M <= 0    vs     H_1: mu_F - mu_M > 0


t.test(temp.femmine, temp.maschi, mu=0, alternative = "greater",
       paired = FALSE, conf.level = 1-alpha, var.equal = TRUE)

# p-value < 0.05 -> Rifiuto H_0

# Esercizio 3
# Carichiamo il dataset funi.txt
# Esso contiene i risultati di 25 prove di rottura (risultati espressi 
# in Newton) di funi prodotte in una fabbrica per mezzo di un nuovo processo 
# produttivo
# Si sa che le funi tradizionali hanno una resistenza di rottura pari a 1730N. 
# 1. Il processo produttivo ha significativamente migliorato la qualità 
# delle funi?
# 2. Si vuole poi verificare con un opportuno test al 5% che la varianza 
# della resistenza delle funi non sia superiore a 28900N^2 
# (deviazione standard 170N) e calcolare poi il p-value di tale test.
# 3. Calcolare un intervallo di confidenza bilatero per la varianza al 95%.

# Importiamo il dataset
dati = read.table('funi.txt', header=T)

dim(dati)
names(dati)
head(dati)

# verifica normalità
par(mfrow = c(1,2))
hist(dati$resistenza,prob=TRUE)
qqnorm(dati$resistenza, pch=16)
qqline(dati$resistenza, lty=2, lwd=3, col='red')

shapiro.test(dati$resistenza)
# p-value > 0.05 -> assumiamo che i dati provengano da popolazione normale

# 1. Vogliamo effettuare un test sulla media 
# H0: mu=1730  contro  H1: mu>1730 

mu0 = 1730
t.test(dati$resistenza, mu=mu0, alternative="greater")

# p-value < 0.05 -> Rifiuto H_0
# Si conclude che il processo produttivo ha migliorato  
# la qualità delle funi.

# 2. Test per la varianza di una popolazione normale.
# H0: sigma2 >= 28900 vs H1: sigma2 < 28900

# Rifiuto H_0 se u0 = (n - 1)*x_var/sigma2_0 < quisq_{alpha, n-1}  

sigma2_0 = 28900
alpha = 0.05
n = nrow(dati)
x_mean = mean(dati$resistenza)
x_var = var(dati$resistenza)
quisq.quantile = qchisq(alpha, df = n - 1)

u0 = (n - 1)*x_var/sigma2_0
u0 < quisq.quantile # Rifiuto H_0
# IC al 95%
IC = c(0, (n-1) * x_var / quisq.quantile)
IC
# p-value del test
p.value = pchisq(u0, df = n - 1)
p.value # coerente al risultato del test

# EnvStats::varTest
help(varTest)
varTest(x = dati$resistenza, alternative = "less", 
        sigma.squared = sigma2_0, conf.level = 1-alpha)

# 3. Intervallo di confidenza bilatero per la varianza al 95%

varTest(x = dati$resistenza, alternative = "two.sided", 
        conf.level = 1-alpha)

# VERIFICA NUMERICA DELLA SIGNIFICATIVITA' DI UN TEST --------------------------

rm(list=ls())
graphics.off()

# Popolazione Gaussiana a varianza nota sigma = 2.5, n = 100, mu0 = 50
# Consideriamo il test:
#       H_0: mu = mu_0     vs     H_1: mu != mu_0

# Stimare alpha
N = 1000     # numero di esperimenti
n = 100      # dimensione del campione
sigma = 2.5  # deviazione standard (nota)
mu0 = 50     # media vera della popolazione da cui provengono i campioni
alpha = 0.05
accetto = rep(0, N) # 1 se accetto H_0, 0 altrimenti 

set.seed(0)
for(i in 1:N){
  # sotto H_0
  dati = rnorm(n, mean = mu0, sd = sigma)
  test = BSDA::z.test(dati, mu = mu0,  sigma.x = sigma)
  if( test$p.value > alpha ){ 
    accetto[i] = 1
  }
}

alpha.camp = 1 - mean(accetto)
alpha.camp

# STIMA DELLA POTENZA DEL TEST 

mu = seq(from = mu0-3, mu0+3, length.out = 30)

potenza = matrix(0, nrow=N, ncol=length(mu)) # 1 se rifiuto H_0, 0 altrimenti 

set.seed(0)
for(j in 1:length(mu)){
  for(i in 1:N){
    # sotto H_1
    dati = rnorm(n, mean = mu[j], sd = sigma)
    test = BSDA::z.test(dati, mu = mu0,  sigma.x = sigma)
    if( test$p.value < alpha ){ 
      potenza[i,j] = 1
    }
  }
}

potenza = colMeans(potenza)
dev.new()
plot((mu-mu0), potenza, type="l", lwd=3,
     xlab=expression(mu-mu[0]), ylab="", main = "Potenza")
abline(h = 1, lwd=2, lty=2, col="red")
abline(h = alpha, lwd=2, lty=2, col="blue")
