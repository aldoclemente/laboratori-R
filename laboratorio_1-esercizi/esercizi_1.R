#'#############################################################'#
#'########               LABORATORIO 1               ##########'#
#'########                 ESERCIZI                  ##########'#
#'#############################################################'#

# ESERCIZIO 1 ------------------------------------------------------------------
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


# ESERCIZIO 2  -----------------------------------------------------------------

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



# ESERCIZIO 3 (ESPOSIZIONE A PESTICIDI) ----------------------------------------
# L'esposizione cronica a pesticidi pu? comportare alterazioni di svariati
# organi e sistemi dell'organismo umano, quali ad esempio quello nervoso (NER),
# endocrino (END), immunitario (IMM), riproduttivo (RIP), renale (REN),
# cardiovascolare (CAR) e respiratorio (RES).
# Si consideri la seguente variabile categorica, contenente i sistemi anatomici
# alterati di 60 pazienti a seguito di esposizione ad alcuni pesticidi.

# Sistemi alterati di 60 pazienti
sistemi = c( 'NER', 'NER', 'END', 'NER', 'END', 'NER', 'IMM', 'NER', 'IMM',
             'NER', 'RIP', 'IMM', 'END', 'END', 'IMM', 'END', 'NER', 'END',
             'NER', 'RIP', 'REN', 'NER', 'IMM', 'NER', 'IMM', 'RIP', 'RIP',
             'CAR', 'CAR', 'NER', 'NER', 'RIP', 'CAR', 'RIP', 'CAR', 'REN',
             'IMM', 'END', 'RES', 'NER', 'NER', 'IMM', 'NER', 'NER', 'END',
             'END', 'END', 'NER', 'NER', 'CAR', 'RES', 'RES', 'NER', 'NER',
             'NER', 'CAR', 'RES', 'REN', 'CAR', 'IMM' )

# RICHIESTE:
# 1. Creare le tabelle di frequenze assolute e relative di sistemi. 
# 2. Visualizzare mediante opportuni grafici il dataset.
# 3. Estrarre il sottocampione composto dai sistemi di tipo NER e END.
# 4. Eliminare dal campione tutti i sistemi di tipo RIP e RES.
# 5. Creare le tabelle di frequenze assolute e relative del nuovo campione.

# ESERCIZIO 4 (DELFINI)
# Si consideri il dataset contenuto nel file di testo "delfini.txt" relativo
# a 100 delfini avvistati al largo delle isole Azzorre. Le variabili osservate
# sono le seguenti:
# - freq: frequenza dei fischi emessi dagli esemplari (in kHz)
# - length: lunghezza degli esemplari (in metri)
# - sex: genere degli esemplari (0 = maschi, 1 = femmine)
# - age: eta' degli esemplari (in anni).

# RICHIESTE:
# 1. Creare le tabelle di frequenze assolute e relative per la variabile sex
#    (0 = maschi, 1 = femmine).
# 2. Creare un grafico a torta delle frequenze relative del sesso dei delfini. 
#    Creare un grafico a barre delle frequenze assolute del sesso dei delfini.
# 3. Estrarre i dati relativi ai delfini che hanno piu' di 16 anni (age >= 16).
#    [Suggerimento: utilizzare la funzione which() per selezionare le righe
#    desiderate; se necessario usare help(which) per capirne meglio il
#    funzionamento.]
# 4. Estrarre i dati relativi all'eta' dei delfini di genere femminile e
#    lunghezza maggiore di 1.80. Inoltre, contare quanti esemplari soddisfano
#    queste condizioni.
#    [Suggerimento: utilizzare le funzioni which() e length() per selezionare le
#    righe desiderate.]