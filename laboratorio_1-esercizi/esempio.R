data = rpois(400, lambda = 4)

data = sort(data)

dev.new()
hist_ =hist(data)
hist_$equidist

freq_ass = hist_$counts
freq_ass

freq_rel = freq_ass / sum(freq_ass)
freq_rel

freq_cum_assoluta = cumsum(freq_ass)
freq_cum_assoluta
# 

freq_cum = cumsum(freq_rel)
freq_cum


hist_$density * 1
c(8, 14, 12, 9, 7)

freq_rel = freq_ass / sum(freq_ass)
freq_rel

freq_cum = cumsum(freq_rel)
freq_cum 

# # # # 
iscritti = read.csv("C:/Users/Aldo/Downloads/courseid_8608_participants.csv", 
                    header = T, sep = ",")
idxs = which( (iscritti$Cognome == "Clemente") | 
              (iscritti$Cognome =="Corso") | 
              (iscritti$Cognome == "Sangalli")) 
iscritti = iscritti[-idxs,]

codice_persona = as.numeric( substr(iscritti$Username,1,8) ) 
head(codice_persona)

num_pari = sum(codice_persona%%2==0) 
num_dispari = sum(codice_persona%%2!=0) 

n_studenti = nrow(iscritti) 
num_dispari/n_studenti
num_pari/n_studenti
