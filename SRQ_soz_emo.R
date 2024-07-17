
# umkodierung der SRQ Fragen zu ihrer inhaltlichen Bedeutung
## Info: Bei Kinder ist Item "Geheimnis" zusätzlich vorhanden

KinderSchule <- KinderSchule |>
  rename(netteDinge = SRQ1_6,
         wütend = SRQ2_6,
         teilen = SRQ3_6,
         gemein = SRQ4_6,
         streiten = SRQ5_6,
         Gefühle = SRQ6_6,
         besser = SRQ7_6,
         schimpfen = SRQ8_6,
         Geheimnis = SRQ9_6,
         nerven = SRQ10_6,
         helfen = SRQ11_6,
         Meinung = SRQ12_6,
         Spaß = SRQ13_6,
         Neid = SRQ14_6,
         zusammen = SRQ15_6,
         
  )


ElternSchule <- ElternSchule |>
  rename(
    netteDinge = EF_Sib1_6,
    wütend = EF_Sib2_6, 
    teilen = EF_Sib3_6,
    gemein = EF_Sib4_6,
    streiten = EF_Sib5_6,
    Gefühle = EF_Sib6_6, 
    besser = EF_Sib7_6,
    schimpfen = EF_Sib8_6,
    nerven = EF_Sib9_6,
    helfen = EF_Sib10_6, 
    Meinung = EF_Sib11_6,
    Spaß = EF_Sib12_6,
    Neid = EF_Sib13_6,
    zusammen = EF_Sib14_6
  )  




# Summe aus 'Konflikt' und 'Harmonie', jeweils auch z-standardisiert

SpaltenNamenEltern <- c("netteDinge", "wütend", "teilen", "gemein", "streiten", "Gefühle",
                        "besser", "schimpfen", "nerven", "helfen", "Meinung", "Spaß", "Neid", "zusammen" )
SpaltenNamenKinder <- c("netteDinge", "wütend", "teilen", "gemein", "streiten", "Gefühle",
                        "besser", "schimpfen", "Geheimnis", "nerven", "helfen", "Meinung", "Spaß", "Neid", "zusammen" )

SpaltenHarmonieEltern <- c("netteDinge",  "teilen",  "Gefühle",
                           "helfen", "Spaß", "zusammen" )

SpaltenHarmonieKinder <- c("netteDinge",  "teilen",  "Gefühle", "Geheimnis",
                           "helfen", "Spaß", "zusammen" )
SpaltenKonflikt <- c("wütend", "gemein", "streiten", 
                     "besser", "schimpfen", "nerven", "Meinung",  "Neid")




KinderSchule$HarmonieKinder <- rowSums(KinderSchule[SpaltenHarmonieKinder], na.rm = FALSE )
ElternSchule$HarmonieElternSchule <- rowSums(ElternSchule[SpaltenHarmonieEltern], na.rm = FALSE )


KinderSchule$KonfliktKinder <- rowSums(KinderSchule[SpaltenKonflikt], na.rm = FALSE )
ElternSchule$KonfliktElternSchule <- rowSums(ElternSchule[SpaltenKonflikt],na.rm=FALSE)



# fehlt nur 1 Wert, wird die Summe neu entsprechend gewichtet z.B. wenn 7 von acht
# Fragen beantwortet wurden <- rechne Wert :7*8


SpaltenElternSchuleHarmonie <- ElternSchule[, SpaltenHarmonieEltern]
SpaltenElternSchuleKonflikt <- ElternSchule[, SpaltenKonflikt]

SpaltenKinderSchuleHarmonie <- KinderSchule[, SpaltenHarmonieKinder]
SpaltenKinderSchuleKonflikt <- KinderSchule[, SpaltenKonflikt]


#KeinNAEltern<- which(rowSums(is.na(SpaltenElternSchule))==0)

EinNAElternHarmo <- which(rowSums(is.na(SpaltenElternSchuleHarmonie))==1)
EinNAElternKonflikt <- which(rowSums(is.na(SpaltenElternSchuleKonflikt))==1)


EinNAKinderHarmo <- which(rowSums(is.na(SpaltenKinderSchuleHarmonie))==1)
EinNAKinderKonflikt <- which(rowSums(is.na(SpaltenKinderSchuleKonflikt))==1)


# Harmonie hat nie nur 1 fehlenden Wert
# Konflikt hat bei Kinder und Eltern jeweils 4 mal einen fehlenden Wert


for (i in 1:length(EinNAElternKonflikt)) {
  
  
  ElternSchule[EinNAElternKonflikt[i], "KonfliktElternSchule"] <- rowSums(ElternSchule[EinNAElternKonflikt[i], SpaltenKonflikt ] , na.rm = TRUE ) *8/7 
  
}


for (i in 1:length(EinNAKinderKonflikt)) {
  KinderSchule[EinNAKinderKonflikt[i], "KonfliktKinder"] <- rowSums(KinderSchule[EinNAKinderKonflikt[i], SpaltenKonflikt ] , na.rm = TRUE ) *8/7 
}



# Erstellen eines Faktors

#KinderSchule$SRQ_012_6 <- factor(KinderSchule$SRQ_012_6, levels = c(0, 1, 2, 50))







# Summe soz-emo Fähigkeiten Lehrkräftegroup <- LehrerGesamt(3:23, TRUE)
## Spalten in Numerics umwandeln 

LehrerGesamt$LF_B9_6 <- gsub(",", ".", LehrerGesamt$LF_B9_6)
LehrerGesamt$LF_B10_6 <- gsub(",", ".", LehrerGesamt$LF_B10_6)
LehrerGesamt$LF_B14_6 <- gsub(",", ".", LehrerGesamt$LF_B14_6)
LehrerGesamt$LF_B15_6 <- gsub(",", ".", LehrerGesamt$LF_B15_6)


LehrerGesamt$LF_B1_6 <- as.numeric(LehrerGesamt$LF_B1_6)
LehrerGesamt$LF_B2_6 <- as.numeric(LehrerGesamt$LF_B2_6)
LehrerGesamt$LF_B3_6 <- as.numeric(LehrerGesamt$LF_B3_6)
LehrerGesamt$LF_B4_6 <- as.numeric(LehrerGesamt$LF_B4_6)
LehrerGesamt$LF_B5_6 <- as.numeric(LehrerGesamt$LF_B5_6)
LehrerGesamt$LF_B6_6 <- as.numeric(LehrerGesamt$LF_B6_6)
LehrerGesamt$LF_B7_6 <- as.numeric(LehrerGesamt$LF_B7_6)
LehrerGesamt$LF_B8_6 <- as.numeric(LehrerGesamt$LF_B8_6)
LehrerGesamt$LF_B9_6 <- as.numeric(LehrerGesamt$LF_B9_6)
LehrerGesamt$LF_B10_6 <- as.numeric(LehrerGesamt$LF_B10_6)
LehrerGesamt$LF_B11_6 <- as.numeric(LehrerGesamt$LF_B11_6)
LehrerGesamt$LF_B12_6 <- as.numeric(LehrerGesamt$LF_B12_6)
LehrerGesamt$LF_B13_6 <- as.numeric(LehrerGesamt$LF_B13_6)
LehrerGesamt$LF_B14_6 <- as.numeric(LehrerGesamt$LF_B14_6)
LehrerGesamt$LF_B15_6 <- as.numeric(LehrerGesamt$LF_B15_6)
LehrerGesamt$LF_B16_6 <- as.numeric(LehrerGesamt$LF_B16_6)
LehrerGesamt$LF_B17_6 <- as.numeric(LehrerGesamt$LF_B17_6)
LehrerGesamt$LF_B18_6 <- as.numeric(LehrerGesamt$LF_B18_6)
LehrerGesamt$LF_B19_6 <- as.numeric(LehrerGesamt$LF_B19_6)
LehrerGesamt$LF_B20_6 <- as.numeric(LehrerGesamt$LF_B20_6)
LehrerGesamt$LF_B21_6 <- as.numeric(LehrerGesamt$LF_B21_6)

# bei einem fehlenden Wert -> ersetzen mit Faktor 21/20, ab 2 fehlender Werte, Löschen der Daten
LehrerGesamt$SumSozEmo <-rowSums(LehrerGesamt[, 3:23], na.rm = TRUE)


EinNALehrerGesamt <- which(rowSums(is.na(LehrerGesamt[, 3:23]))==1)

for (i in 1:length(EinNALehrerGesamt)) {
  LehrerGesamt[EinNALehrerGesamt[i], "SumSozEmo"] <- LehrerGesamt[EinNALehrerGesamt[i], "SumSozEmo"] * 21/20
  
  
}


MehrNALehrerGesamt <- which(rowSums(is.na(LehrerGesamt[, 3:23]))>1)

LehrerGesamt <- LehrerGesamt [-MehrNALehrerGesamt,]



# erstellen von neuen Datensätzen

ElternKinderSchule <- full_join(KinderSchule, ElternSchule, by = 'Code', suffix = c(".Kinder", ".Eltern"))
ElternKinderSchule <- ElternKinderSchule[!is.na(ElternKinderSchule$Initialen.Kinder),]

GesamtSchule <- full_join(ElternKinderSchule, LehrerGesamt, by = c( 'Code' = 'Versuchspersonennummer'))
GesamtSchule <- GesamtSchule[!is.na(GesamtSchule$Initialen.Kinder),]


ElternKinderSchuleElternDa <- inner_join(KinderSchule, ElternSchule, by = 'Code', suffix = c(".Kinder", ".Eltern"))



# Datenkontrolle

kontrolle <- ElternKinderSchuleElternDa |>
  select(Code, SRQ_012_6, SibG_6, SibA_6, EF_SRQ_012_6, EF_Sib_SB_6, EF_Sib_Age_6)
kontrolle2 <- GesamtSchule[(GesamtSchule$HarmonieElternSchule < 10 | GesamtSchule$KonfliktElternSchule < 10),]

# Anpassen der Eltern SRQ Angaben an die der Kinder, die von den Eltern sind vermutlich
# korrekter

ElternKinderSchule$SRQ_neu <- ifelse(!is.na(ElternKinderSchule$EF_SRQ_012_6), 
                                       ElternKinderSchule$EF_SRQ_012_6,
                                       ElternKinderSchule$SRQ_012_6)


GesamtSchule$SRQ_neu <- ifelse(!is.na(GesamtSchule$EF_SRQ_012_6), 
                                     GesamtSchule$EF_SRQ_012_6,
                                     GesamtSchule$SRQ_012_6)

## Erstellen der Faktoren


ElternKinderSchule$SibG_6 <- factor(ElternKinderSchule$SibG_6, levels = c(0, 1), labels = c("weiblich", "männlich"))
ElternKinderSchule$Geschlecht.Kinder <- factor(ElternKinderSchule$Geschlecht.Kinder, levels = c(0, 1), labels = c("weiblich", "männlich"))
ElternKinderSchule$SRQ_neu <- factor(ElternKinderSchule$SRQ_neu, levels = c(0, 1, 2, 20, 50))

GesamtSchule$SibG_6 <- factor(GesamtSchule$SibG_6, levels = c(0, 1), labels = c("weiblich", "männlich"))
GesamtSchule$Geschlecht.Kinder <- factor(GesamtSchule$Geschlecht.Kinder, levels = c(0, 1), labels = c("weiblich", "männlich"))
GesamtSchule$SRQ_neu <- factor(GesamtSchule$SRQ_neu, levels = c(0, 1, 2, 20, 50))

