###############################################################
##################    CORSO DI STATISTICA    ##################
################## per INGEGNERIA MATEMATICA ##################
###############################################################

###############################################################
#########               LABORATORIO 1               ###########
#########                 ESERCIZI                  ###########
###############################################################

# 1 ESERCIZIO
# Importare i dati di anagrafica.txt.
# I dati consistono in osservazioni di 4 variabili:
#    ETA
#    PESO
#    ALTEZZA
#    SESSO ( 1 = femmina, 2 = maschio )
# che rappresentano l'età, il peso, l'altezza e il sesso di pazienti
# ammessi a diversi reparti di ospedali della Lombardia con una diagnosi di infarto

#  selezionare e condurre un'analisi
#  descrittiva delle variabili quantitative:
#  a) ETA'
#  b) PESO
#  c) ALTEZZA
#  Commentare i risultati alla luce di quanto osservato.

rm(list=ls())
graphics.off()

# importo il dataset
anagrafica = read.table("anagrafica.txt", header=TRUE)

head(anagrafica)

anagrafica = anagrafica[,1:3]

# summary delle variabili di interesse
summary(anagrafica$ETA)

summary(anagrafica$PESO)

summary(anagrafica$ALTEZZA)

# boxplot delle variabili
dev.new()
par(mfrow=c(1,3))
boxplot(anagrafica$ETA,
        main="Età")
boxplot(anagrafica$PESO,
        main="Peso")
boxplot(anagrafica$ALTEZZA,
        main="Altezza")

# istogramma con il numero corretto di intervalli
ndati = nrow(anagrafica)
dev.new()
par(mfrow=c(3,1))
hist(anagrafica$ETA,
     probability = T,
     main="Istogramma Età", xlab = "", 
     breaks = floor(sqrt(ndati)) )
hist(anagrafica$PESO,
     probability = T,
     main="Istogramma Peso", xlab="",
     breaks = floor(sqrt(ndati)))
hist(anagrafica$ALTEZZA,
     probability = T,
     main="Istogramma Altezza", xlab="",
     breaks = floor(sqrt(ndati)))



# 2 ESERCIZIO
# Importare i dati di anagrafica.txt.
# I dati consistono in osservazioni di 4 variabili:
#    ETA
#    PESO
#    ALTEZZA
#    SESSO ( 1 = femmina, 2 = maschio )
# che rappresentano l'età, il peso, l'altezza e il sesso di pazienti
# ammessi a diversi reparti di ospedali della Lombardia con una diagnosi di infarto

# Condurre analisi descrittiva della variabile categorica SESSO.
# Commentare i risultati alla luce di quanto osservato

rm(list=ls())
graphics.off()

# importo il dataset
anagrafica = read.table("anagrafica.txt", header=TRUE)

head(anagrafica)

sesso = anagrafica[,4]
sesso[which(sesso==1)] = "F"
sesso[which(sesso==2)] = "M"
class(sesso)

sesso = factor(sesso)
summary(sesso)

freq_ass = table(sesso)
freq_ass

freq_rel = freq_ass / length(sesso) # oppure prop.table(freq_ass)
freq_rel

dev.new()
barplot(freq_rel, col=c("pink","lightblue"))

dev.new()
pie(freq_rel, col=c("pink","lightblue"))
