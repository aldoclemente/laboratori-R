#Esercitazione 1 - Esercizio 1

# PUNTO A ----------------------------------------------------------------------
freq_ass = c(14, 46, 58, 76, 68, 62, 48, 22, 6)

# vettori degli estremi 
# classe i-esima (a_i, b_i]
a_ = seq(from=300, to=1100, by=100)
b_ = seq(from=400, to=1200, by=100)

# vettore dei punti medi di ogni classe
mids = (a_ + b_)/2
mids

# numero totale di dati
sum(freq_ass)

# frequenze relative
freq_rel = freq_ass / sum(freq_ass)
freq_rel

# frequenze cumulate 
freq_cum = cumsum(freq_rel)
freq_cum

# tabella
cbind(freq_ass, freq_rel, freq_cum)

# Istogramma 
dati = c()
for(i in 1:length(freq_ass)){
  dati = c(dati, rep(mids[i], freq_ass[i]))
}

istogramma <- hist(dati, main="Istogramma Durata", ylab="Densità", xlab="", probability = T)
axis(1, at=seq(from=min(a_),
               to=max(b_),
               by=(b_[1]-a_[1])))

istogramma$counts  # freq_ass
istogramma$density # densita = freq_rel / (b_[1] - a_[1])

# PUNTO B ----------------------------------------------------------------------

freq_cum # Q1 in terza classe, Q2 in quinta classe, Q3 in sesta classe

sum( freq_rel[ a_ >= 900 ] )

# PUNTO C ----------------------------------------------------------------------
# media 
mean_ = sum(mids*freq_ass) / sum(freq_ass)
mean_

# varianza campionaria 
var_ = sum( (mids-mean_)^2*freq_ass ) / (sum(freq_ass) - 1) 
var_

# varianza (NON campionaria)

sum( (mids-mean_)^2*freq_ass ) / (sum(freq_ass) )

(sum(freq_ass) - 1)/sum(freq_ass) * var_ 
