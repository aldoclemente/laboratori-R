#########################################
#############  ESERCIZIO 1  #############
#########################################
# Analisi dei dati contenuti nel file combustione.txt. 
# Il dataset contiene 20 osservazioni del tempo di combustione di due diversi composti:
#    Tempo:  tempo di combustione
#    Formula: tipo di composto ( A o B )
# 
# Si richiede di:
#   1. Importare il dataset e salvarlo in un dataframe. 
#   2. Suddividere i composti in base al tipo (A o B).
#   3. Costruire una tabella delle frequenze relative per ciascun sottogruppo. 
#   4. Costruire gli istogrammi affiancati della variabile \texttt{Tempo} per ciascun sottogruppo. 
#      Costruire i boxplot affiancati della variabile \texttt{Tempo} per ciascun sottogruppo

#########################################
#############  ESERCIZIO 2  #############
#########################################

# Analisi dei dati contenuti nel file 'temperatura.txt'.
# Il dataset contiene 130 osservazioni di 3 variabili:
#   Temperatura: temperatura corporea ( in gradi Fahrenheit )
#   Sesso: sesso del paziente ( U = uomo, D = donna )
#   Freq_cardiaca: frequenza cardiaca ( battiti al minuto )
# 
#   Si richiede di:
# 1. Importare il dataset e salvarlo in un dataframe.
#      [Attenzione: si noti che nel dataset indicato il carattere separatore per le cifre decimali
#                   è la virgola, leggere help("read.table")] 
# 2. Suddividere i pazienti in base al sesso.
# 3. Costruire una tabella delle frequenze relative per ciascun sottocampione individuato dal genere.
# 4. Costruire gli istogrammi affiancati della variabile Temperatura nei due sottocampioni individuati dal genere.
#    Costruire i boxplot affiancati della variabile Temperatura nei due sottocampioni individuati dal genere.
#    Costruire un unico grafico contenente istogrammi e i boxplots, ruotati in orizzontale, per ciascun sottogruppo.
# 5. Commentare i risultati.


#########################################
#############  ESERCIZIO 3  #############
#########################################

# ANALISI DEL COLESTEROLO
# Considerare il livello di colesterolo nel sangue dei primi 100 studenti
# presenti nel file di testo 'studenti.txt'.

# Si richiede di:
# 1. Importare il dataset e salvarlo in un dataframe.
# 2. Suddividere gli studenti in maschi e femmine.
# 3. Costruire una tabella delle frequenze relative per ciascun sottogruppo.
# 4. Costruire in un unico grafico l'istogramma del colesterolo per i maschi e
#    per le femmine.
# 5. Esiste una relazione tra sesso dello studente e livello di colesterolo?

#########################################
#############  ESERCIZIO 4  #############
#########################################

# ANALISI DELLE ABITUDINI DI STUDENTI
# Considerare i dati contenuti nel file di testo 'abitudini_studenti.txt'. Le
# variabili presenti sono le seguenti:
# - Altezza (in cm)
# - Sesso ("F" o "M")
# - Sonno (valutazione del sonno studenti espresso tramite voto /10)
# - Studio (ore di studio effettuate durante lo scorso semestre)
# - Lavoro (ore di lavoro effettuate durante lo scorso semestre)
# - Bevanda ("acqua", "birra" o "vino")
# - Caff? (numero di caff? bevuti al giorno)

# Si richiede di:
# 1. Importare il dataset e salvarlo in un dataframe.
# 2. Calcolare i seguenti indici di posizione per la variabile "Studio": media
#    campionaria, massimo, minimo, mediana, primo e terzo quartile e il quantile
#    di ordine 0.9.
# 3. Calcolare i seguenti indici di dispersione per la variabile "Studio":
#    varianza campionaria, deviazione standard, range e range interquartile.
# 4. Costruire un istogramma che illustri le frequenze relative della variabile
#    "Studio".
#    Ricavare le seguenti informazioni: punti centrali, frequenza relativa e
#    frequenza assoluta di ogni classe.
#    Quali considerazioni si possono trarre dall'istogramma?
# 5. Costruire un box plot con le osservazioni della variabile "Studio".
#    Quali considerazioni si possono trarre dal box plot?
# 6. Calcolare i seguenti indici di posizione per la variabile "Studio" nei due
#    sottocampioni individuati dal genere: media campionaria, massimo, minimo,
#    mediana, primo e terzo quartile e il quantile di ordine 0.9.
# 7. Calcolare i seguenti indici di dispersione per la variabile "Studio" nei
#    due sottocampioni individuati dal genere: varianza campionaria, deviazione
#    standard, range e range interquartile.
# 8. Costruire gli istogrammi che illustrino le frequenze relative della
#    variabile "Studio" nei due sottocampioni individuati dal genere.
#    Quali considerazioni si possono fare confrontando i due istogrammi?
# 9. Costruire i box plot con le osservazioni della variabile "Studio" per i due
#    sottocampioni individuati dal genere.
#    Quali considerazioni si possono fare confrontando i due box plot?
