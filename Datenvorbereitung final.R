#libarys
library(tidyverse)
library(psych)
library(sqldf)
library(readxl)

library (openxlsx)
library (compute.es)
library (lawstat)
library(pwr)
library(knitr)
library(kableExtra)
library(car)
library(xtable)
library(effectsize)
library(lmtest) # Homoskedastizitätstest
library(correlation)

setwd("C:/Users/sabri/OneDrive/Desktop/Zulassungsarbeit/Daten")

# Datensatz einlesen

C1t6_Elternfragebogen_1stGrade_überprüft <- read_excel("C1t6_Elternfragebogen_1stGrade_überprüft.xlsx")
C1t6_Elternfragebogen_2ndGrade_überprüft <- read_excel("C1t6_Elternfragebogen_2ndGrade_überprüft.xlsx")
C2t5_Elternfragebogen_Schule_überprüft <- read_excel("C2t5_Elternfragebogen_Schule_überprüft.xlsx")


L4K_Protokollbogen_C1_t6_first_graders_final <- read_excel("L4K_Protokollbogen_C1_t6_first_graders_final.xlsx")
L4K_Protokollbogen_C1_t6_second_graders_final <- read_excel("L4K_Protokollbogen_C1_t6_second_graders_final.xlsx")
L4K_Protokollbogen_C2_t5_first_graders <- read_excel("L4K_Protokollbogen_C2_t5_first_graders.xlsx")


C2t5_Lehrkraftdaten_ohneUngereimtheiten <- read_excel("C2t5_Lehrkraftdaten_ohneUngereimtheiten.xlsx")
Data_Lehrkräfte_C1_t6 <- read_excel("Data_Lehrkräfte_C1_t6.xlsx")

C2t1_Elternfragebogen <- read_excel("C2t1_Elternfragebogen.xlsx")
C1t1_Elternfragebogen <- read_excel("C1t1_Elternfragebogen.xlsx")




# reduzieren der Datensätze auf die benötigten Spalten Spalten 134-150   154-174
## Eltern 
## Achtung: Kein SRQ für Eltern von C2t5-Kindergarten vorhanden, nur Geschwisteranzahl/ -alter

Eltern1C1t6unk <- C1t6_Elternfragebogen_1stGrade_überprüft  |> 
  select(1:3, 124:150)

Eltern2C1t6unk <- C1t6_Elternfragebogen_2ndGrade_überprüft  |> 
  select(1:3, 124:150)

Eltern1C2t5unk <- C2t5_Elternfragebogen_Schule_überprüft |>
  select(1:3, 144:170)


Demogr1 <- C1t1_Elternfragebogen|>
  select(1, 4, 5)

Demogr2 <- C2t1_Elternfragebogen|>
  select(1, 4, 5)

## Kinder 
## Achtung: Kein SRQ für Kinder von C2t5-Kindergarten vorhanden

Kinder1C1t6 <- L4K_Protokollbogen_C1_t6_first_graders_final |>
  select(1:2, 239:256, 278)


Kinder2C1t6 <- L4K_Protokollbogen_C1_t6_second_graders_final |>
  select(1:2, 44:61, 138)

Kinder1C2t5 <-L4K_Protokollbogen_C2_t5_first_graders |>
  select(1:2, 241:258, 280)

## Erzieher*innen und Lehrkräfte
## Achtung: Daten soz. emo. Kompetenzen von Lehrkräften unklar
## Achtung: Lehrkräfte, die zu hohe Werte eingegeben hatten, wurden abgeändert

LehrerC2t5 <- C2t5_Lehrkraftdaten_ohneUngereimtheiten |>
  select (1:2, 21:41)

LehrerC1t6 <- Data_Lehrkräfte_C1_t6 |>
  select (1, 3, 22:42)



#write.xlsx(Eltern1C1t6unk, file="Eltern1C1t6.xlsx")
#write.xlsx(Eltern1C2t5unk, file="Eltern1C2t5.xlsx")
#write.xlsx(Eltern2C1t6unk, file="Eltern2C1t6.xlsx")



# korrigieren von falschen/fehlenden SRQ-Angaben in Excel, aufgrund der vielzahl der fehler

Eltern1C1t6 <- read_excel("Eltern1C1t6unk.xlsx")
Eltern1C2t5 <- read_excel("Eltern1C2t5unk.xlsx")
Eltern2C1t6 <- read_excel("Eltern2C1t6unk.xlsx")


## Umbenennungen zum Zusammenführen der Datensätze

Eltern1C2t5 <- Eltern1C2t5 |>
  rename( EF_K2G_6 = EF_K2G_5,
          EF_K3G_6 = EF_K3G_5,
          EF_K4G_6 = EF_K4G_5,
          EF_K5G_6 = EF_K5G_5,
          EF_K6G_6 = EF_K6G_5,
          EF_K2A_6 = EF_K2A_5,
          EF_K3A_6 = EF_K3A_5,
          EF_K4A_6 = EF_K4A_5,
          EF_K5A_6 = EF_K5A_5,
          EF_K6A_6 = EF_K6A_5,
          
          EF_SRQ_012_6=EF_SRQ_012_5, 
          EF_Sib_SB_6 =EF_Sib_SB_5, 
          EF_Sib_Age_6=EF_Sib_Age_5,
          EF_Sib1_6 = EF_Sib1_5,
          EF_Sib2_6 = EF_Sib2_5, 
          EF_Sib3_6 = EF_Sib3_5,
          EF_Sib4_6 = EF_Sib4_5,
          EF_Sib5_6 = EF_Sib5_5,
          EF_Sib6_6 = EF_Sib6_5, 
          EF_Sib7_6 = EF_Sib7_5,
          EF_Sib8_6 = EF_Sib8_5,
          EF_Sib9_6 = EF_Sib9_5,
          EF_Sib10_6 = EF_Sib10_5, 
          EF_Sib11_6 = EF_Sib11_5,
          EF_Sib12_6 = EF_Sib12_5,
          EF_Sib13_6 = EF_Sib13_5,
          EF_Sib14_6 = EF_Sib14_5
  )




## Kinder


Kinder1C2t5 <- Kinder1C2t5 |>
  rename(SRQ_012_6 = SRQ_012_5,
         SibG_6 = SibG_5,
         SibA_6 = SibA_5,
         SRQ1_6 = SRQ1_5,
         SRQ2_6 = SRQ2_5,
         SRQ3_6 = SRQ3_5,
         SRQ4_6 = SRQ4_5,
         SRQ5_6 = SRQ5_5,
         SRQ6_6 = SRQ6_5,
         SRQ7_6 = SRQ7_5,
         SRQ8_6 = SRQ8_5,
         SRQ9_6 = SRQ9_5,
         SRQ10_6 = SRQ10_5,
         SRQ11_6 = SRQ11_5,
         SRQ12_6 = SRQ12_5,
         SRQ13_6 = SRQ13_5,
         SRQ14_6 = SRQ14_5,
         SRQ15_6 = SRQ15_5
  )

## Lehrer


LehrerC2t5 <- LehrerC2t5 |>
  rename(LF_B1_6= LF_B1_5,
         LF_B2_6= LF_B2_5,
         LF_B3_6= LF_B3_5,
         LF_B4_6= LF_B4_5,
         LF_B5_6= LF_B5_5,
         LF_B6_6= LF_B6_5,
         LF_B7_6= LF_B7_5,
         LF_B8_6= LF_B8_5,
         LF_B9_6= LF_B9_5,
         LF_B10_6= LF_B10_5,
         LF_B11_6= LF_B11_5,
         LF_B12_6= LF_B12_5,
         LF_B13_6= LF_B13_5,
         LF_B14_6= LF_B14_5,
         LF_B15_6= LF_B15_5,
         LF_B16_6= LF_B16_5,
         LF_B17_6= LF_B17_5,
         LF_B18_6= LF_B18_5,
         LF_B19_6= LF_B19_5,
         LF_B20_6= LF_B20_5,
         LF_B21_6= LF_B21_5,
  )





# bearbeiten von Datensätzen


## Alter der Geschwister in Jahre umwandeln und als Numeric abspeichern


Eltern2C1t6$EF_K2A_6 <- gsub("Jahre", "", Eltern2C1t6$EF_K2A_6)
Eltern2C1t6$EF_K3A_6 <- gsub("Jahre", "", Eltern2C1t6$EF_K3A_6)
Eltern2C1t6$EF_K4A_6 <- gsub("Jahre", "", Eltern2C1t6$EF_K4A_6)
Eltern2C1t6$EF_K2A_6 <- gsub("6  Monate", 0.5, Eltern2C1t6$EF_K2A_6)
Eltern2C1t6$EF_K2A_6 <- gsub("8 M.", 8/12, Eltern2C1t6$EF_K2A_6)


Eltern1C2t5$EF_K2A_6 <- gsub(",", ".", Eltern1C2t5$EF_K2A_6)
Eltern1C2t5$EF_K3A_6 <- gsub(",", ".", Eltern1C2t5$EF_K3A_6)
Eltern1C2t5$EF_K4A_6 <- gsub(",", ".", Eltern1C2t5$EF_K4A_6)
Eltern1C2t5$EF_K2A_6 <- as.numeric(Eltern1C2t5$EF_K2A_6)
Eltern1C2t5$EF_K3A_6 <- as.numeric(Eltern1C2t5$EF_K3A_6)
Eltern1C2t5$EF_K4A_6 <- as.numeric(Eltern1C2t5$EF_K4A_6)

Eltern2C1t6$EF_K2A_6 <- gsub(",", ".", Eltern2C1t6$EF_K2A_6)
Eltern2C1t6$EF_K3A_6 <- gsub(",", ".", Eltern2C1t6$EF_K3A_6)
Eltern2C1t6$EF_K4A_6 <- gsub(",", ".", Eltern2C1t6$EF_K4A_6)
Eltern2C1t6$EF_K2A_6 <- as.numeric(Eltern2C1t6$EF_K2A_6)
Eltern2C1t6$EF_K3A_6 <- as.numeric(Eltern2C1t6$EF_K3A_6)
Eltern2C1t6$EF_K4A_6 <- as.numeric(Eltern2C1t6$EF_K4A_6)


Eltern1C2t5$EF_K2A_6 <- Eltern1C2t5$EF_K2A_6 / 12
Eltern1C2t5$EF_K3A_6 <- Eltern1C2t5$EF_K3A_6 / 12
Eltern1C2t5$EF_K4A_6 <- Eltern1C2t5$EF_K4A_6 / 12
Eltern1C2t5$EF_K5A_6 <- Eltern1C2t5$EF_K5A_6 / 12
Eltern1C2t5$EF_K6A_6 <- Eltern1C2t5$EF_K6A_6 / 12



## Kinder

## fehlerhafte Angaben bei SRQ korrigieren
## Geschwisterkind ist unter 3, daher 50 bei SRQ
## geändert bei 2630311, 2831129


Kinder2C1t6[110, "SRQ_012_6"] <- 50
Kinder2C1t6[123, "SRQ_012_6"] <- 50




Kinder1C2t5$SibG_6 <- ifelse(Kinder1C2t5$SibG_6 == "weiblich", 0, Kinder1C2t5$SibG_6)
Kinder1C2t5$SibG_6 <- ifelse(Kinder1C2t5$SibG_6 == "männlich", 1, Kinder1C2t5$SibG_6)




## Vorbereiten des Alter des Kindes zu Numeric machen 
Kinder1C2t5$SibA_6 <- gsub("Jahre", "", Kinder1C2t5$SibA_6)
Kinder2C1t6$SibA_6 <- gsub("Jahre", "", Kinder2C1t6$SibA_6)


## Eltern
Eltern2C1t6[Eltern2C1t6 == 1731201] <- 1732201
Eltern1C2t5[Eltern1C2t5 == 99152218] <- 991522118
Eltern1C2t5[Eltern1C2t5 == 992110703] <- 992110705
Eltern1C2t5[Eltern1C2t5 == 991431408] <- 991431418
Eltern1C2t5[Eltern1C2t5 == 993730906] <- 990730906




# Lehrer

LehrerC1t6[LehrerC1t6 == 1731201] <- 1732201
LehrerC2t5[LehrerC2t5 == 990341111] <- 99034111

# Demografische Daten
#  1732201 JW, 991632195 PS  990141804 AH 991431418 LR
Demogr1[Demogr1 == 1732201] <- 1731201
Demogr2[Demogr2 == 991632105] <- 991632195
Demogr2[Demogr2 == 990141802] <- 990141804
Demogr2[Demogr2 == 991431408] <- 991431418

# bei Demografischen Daten gilt: Mädchen 1 Junge 0, dies muss umkodiert werden
# zu Mädchen 0, Junge 1, damit es einheitlich ist

Demogr1$Geschlecht <- ifelse(Demogr1$Geschlecht == 0, 1, 0) 
Demogr2$Geschlecht <- ifelse(Demogr2$Geschlecht == 0, 1, 0)   




# Alter bei Erhebungszeitraum ergänzen

Demogr1$AlterTage <- round(as.numeric(difftime(as.Date("2023-01-31"), as.Date(Demogr1$Geburtsdatum), units = "days")))
Demogr1$AlterMonate <- Demogr1$AlterTage / 30.44
Demogr1$AlterJahre <-Demogr1$AlterTage/ 365.25


Demogr2$AlterTage <- round(as.numeric(difftime(as.Date("2023-02-15"), as.Date(Demogr2$Geburtsdatum), units = "days")))
Demogr2$AlterMonate <- Demogr2$AlterTage / 30.44
Demogr2$AlterJahre <-Demogr2$AlterTage/ 365.25


# zusammenfügen der Datensätze

ElternSchule <- rbind.data.frame(Eltern1C1t6, Eltern2C1t6, Eltern1C2t5)


Demogr <- rbind.data.frame(Demogr1, Demogr2)


ElternSchule <- left_join(ElternSchule, Demogr, by = c("Code" = "Code"))


Demogr[Demogr == 1731201] <- 1732201
Demogr[Demogr == 991632195] <- 991632105
Demogr[Demogr == 990341111] <- 99034111

KinderSchule <- rbind.data.frame(Kinder1C1t6, Kinder1C2t5, Kinder2C1t6)
KinderSchule$SibA_6 <- as.numeric(KinderSchule$SibA_6)
KinderSchule <- left_join(KinderSchule, Demogr, by = c("Code" = "Code"))


LehrerGesamt <- rbind.data.frame(LehrerC1t6, LehrerC2t5)



