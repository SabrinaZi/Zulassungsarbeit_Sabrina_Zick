# Hypothese 3


## Benötigter Datensatz: Fragebögen der Eltern, da nur dort alle Angaben zu Geschwistern
## vorhanden sind und Fragebögen der Lehrkräfte


Hypo3 <- GesamtSchule
Hypo3 <- Hypo3[Hypo3$SRQ_neu == 1 | Hypo3$SRQ_neu == 20, ]
Hypo3 <- Hypo3[!is.na(Hypo3$SRQ_neu), ]
Hypo3 <- Hypo3[!is.na(Hypo3$SumSozEmo), ]

# Ausschließen von Zwillingen
Hypo3sub <- Hypo3[ , c("AlterJahre.Kinder", "EF_K2A_6", "Code", "Geburtsdatum.Kinder")]

# auszuschließen sind  990321803, 991321807, 992110704, 992110705

Hypo3 <- Hypo3[!(Hypo3$Code %in% c("990321803", "991321807", "992110704", "992110705")), ]

# verbesserte Angabe des Alters
# liegt eine Angabe bei EF_K2A vor, wird diese genommen, ansonsten die Angabe im SibA_6
# gibt es beides nicht -> Ausschluss
# ebenso wird bei Geschlecht vorgegangen

Hypo3$AlterGesch <- ifelse(!is.na(Hypo3$EF_K2A_6), Hypo3$EF_K2A_6, Hypo3$SibA_6 ) 
Hypo3$GeschJungAlt <- ifelse(Hypo3$AlterGesch > Hypo3$AlterJahre.Kinder, 1, 0)
Hypo3$GeschJungAlt <- factor(Hypo3$GeschJungAlt, levels = c(0, 1), labels = c("Jünger", "älter"))


Hypo3$HV <- ifelse(Hypo3$SibG_6 == "männlich", 1, NA)
Hypo3$HV <- ifelse(Hypo3$SibG_6 == "weiblich", 0, Hypo3$HV)

Hypo3$GeschlechtGesch <- ifelse(!is.na(Hypo3$EF_K2G_6), Hypo3$EF_K2G_6, Hypo3$HV)
Hypo3$GeschlechtGesch <- factor(Hypo3$GeschlechtGesch, levels = c(0, 1), labels = c("weiblich", "männlich"))

#Kontrolle
Hypo3sub <- Hypo3[ , c("Geschlecht.Kinder", "GeschlechtGesch", "GeschJungAlt", "SumSozEmo")]











# Beschreibung der Daten mit 8 Untergruppen
Uebersicht_Hypo3 <- as.data.frame(table(Hypo3$Geschlecht.Kinder, Hypo3$GeschlechtGesch, Hypo3$GeschJungAlt))


Variablenübersicht8Gruppen <- describeBy(Hypo3$SumSozEmo, list(Hypo3$Geschlecht.Kinder, Hypo3$GeschlechtGesch, Hypo3$GeschJungAlt))

## Boxplots der Werte
DeskrHypo3 <- describeBy(Hypo3$SumSozEmo, group = list(Hypo3$Geschlecht.Kinder, Hypo3$GeschlechtGesch, Hypo3$GeschJungAlt))
HVHypo3 <- do.call(rbind, lapply(DeskrHypo3, as.data.frame))
HV2Hypo3 <- xtable(HVHypo3)
print(HV2Hypo3)

par(mfrow = c(1, 1))
Boxplot(SumSozEmo ~ Geschlecht.Kinder + GeschlechtGesch + GeschJungAlt, data = Hypo3,
        xlab = "",
        ylab = "Summe sozial-emotionaler Fähigkeiten",
        names = c("1", "2", "3", "4", "5", "6", "7", "8"))

# Beschreibung der Daten mit 4 Untergruppen
Uebersicht_Hypo3_4Gruppen <- as.data.frame(table(Hypo3$Geschlecht.Kinder, Hypo3$GeschlechtGesch))


## Tabelle anzeigen
ergebnisse_tabelle_kable <- kable(
  Uebersicht_Hypo3_4Gruppen,
  format = "latex", # Wähle das gewünschte Ausgabeformat (z. B. "html" oder "latex")
  caption = "Uebersicht Häufigkeiten Hypothese 3",
  align = "c",     # Ausrichtung der Spalten (c = zentriert)
  col.names = c( "Geschlecht Kinder", "Geschlecht Geschwisterkind", "Häufigkeit"),
  escape = FALSE    # Erlaube HTML-Tags in den Zellen
) %>%
  kable_styling(
    full_width = FALSE, # Breite der Tabelle anpassen
    bootstrap_options = "striped", # Verwende gestreifte Tabellen (optional)
    position = "center" # Tabellenposition (optional)
  )

ergebnisse_tabelle_kable


mittelwerte <- as.data.frame(aggregate(SumSozEmo ~ Geschlecht.Kinder + GeschlechtGesch , data = Hypo3, FUN = mean))
sd <- as.data.frame(aggregate(SumSozEmo ~ Geschlecht.Kinder + GeschlechtGesch , data = Hypo3, FUN = sd))

Mittelwerte_Tabelle <- kable(
  mittelwerte,
  format = "html", # Wähle das gewünschte Ausgabeformat (z. B. "html" oder "latex")
  caption = "Uebersicht Mittelwerte der Faktoren",
  align = "c",     # Ausrichtung der Spalten (c = zentriert)
  col.names = c( "Geschlecht Kind", "Geschlecht Geschwisterkind", "Sozial-emotionale Fähigkeiten"),
  escape = FALSE    # Erlaube HTML-Tags in den Zellen
) %>%
  kable_styling(
    full_width = FALSE, # Breite der Tabelle anpassen
    bootstrap_options = "striped", # Verwende gestreifte Tabellen (optional)
    position = "center" # Tabellenposition (optional)
  )
Mittelwerte_Tabelle


# erstellen einer gemeinsamen Tabelle wie bei 8 Gruppen

# Boxplots der Werte
DeskrHypo3_4Gruppen <- describeBy(Hypo3$SumSozEmo, group = list(Hypo3$Geschlecht.Kinder, Hypo3$GeschlechtGesch))
HVHypo3_4Gruppen <- do.call(rbind, lapply(DeskrHypo3_4Gruppen, as.data.frame))
HV2Hypo3_4Gruppen <- xtable(HVHypo3_4Gruppen)
print(HV2Hypo3_4Gruppen)

Boxplot(SumSozEmo ~ Geschlecht.Kinder + GeschlechtGesch, data = Hypo3)


# prüfen der Voraussetzungen
## Homogenität der Varianzen

leveneTest(SumSozEmo ~ Geschlecht.Kinder * GeschlechtGesch , data = Hypo3)
#leveneTest(SumSozEmo ~ Geschlecht.Kinder * GeschlechtGesch , data = Hypo3, center = "mean")




# durchführen der ANOVA
ANOVA4 <- aov(SumSozEmo ~ Geschlecht.Kinder * GeschlechtGesch, data = Hypo3)
summary(ANOVA4)
# Normalverteilung der Residuen -> nicht nötig, nur bei Regression
plot(ANOVA4, 2) # in etwa erfüllt
shapiro.test(residuals(ANOVA4))  # nicht erfüllt

# Normalverteilung der Stichprobe
shapiro.test(Hypo3$SumSozEmo)
hist(Hypo3$SumSozEmo)

# Normalverteilung der einzelnen Stichproben

Hypo3_Gruppen_4Gruppen <- Hypo3 |>
  group_by(Geschlecht.Kinder, GeschlechtGesch) |>
  summarise()

plot(ANOVA4)
# ohne for Schleife

aktuelle_gruppe <- filter(Hypo3, Geschlecht.Kinder == "weiblich" &
                            GeschlechtGesch == "weiblich")
hist(aktuelle_gruppe$SumSozEmo, breaks = 10)
shapiro.test(aktuelle_gruppe$SumSozEmo)

aktuelle_gruppe2 <- filter(Hypo3, Geschlecht.Kinder == "weiblich" &
                             GeschlechtGesch == "männlich" )
shapiro.test(aktuelle_gruppe2$SumSozEmo)
hist(aktuelle_gruppe2$SumSozEmo, breaks = 15)

aktuelle_gruppe3 <- filter(Hypo3, Geschlecht.Kinder == "männlich" &
                             GeschlechtGesch == "weiblich" )
shapiro.test(aktuelle_gruppe3$SumSozEmo)
hist(aktuelle_gruppe3$SumSozEmo)

aktuelle_gruppe4 <- filter(Hypo3, Geschlecht.Kinder == "männlich" &
                             GeschlechtGesch == "männlich" )

shapiro.test(aktuelle_gruppe4$SumSozEmo)
hist(aktuelle_gruppe4$SumSozEmo)


# Power
#pwr.f2.test(sig.level = .05)


# interpretation der ANOVA


resultANOVA4 <- Anova(ANOVA4, type = 3) # ANOVA mit Typ 3 Quadratsummen
summary(resultANOVA4)

TukeyHSD(ANOVA4, "Geschlecht.Kinder") # Analysieren des Haupteffekts
TukeyHSD(ANOVA4) # analysieren aller Unterschiede

# Effektstärke partielles eta^2, gibt anteil der korrigierten Varianzaufklären der UV
eta_squared(resultANOVA4, partial = TRUE)

WelchANOVA4 <- oneway.test(SumSozEmo ~ Geschlecht.Kinder * GeschlechtGesch , data = Hypo3)
summary(WelchANOVA4)




## Keine Interaktionseffekte

# grafische Darstellung der Interaktion
interaction.plot(x.factor = Hypo3$Geschlecht.Kinder,
                 trace.factor = Hypo3$GeschlechtGesch,
                 response = Hypo3$SumSozEmo,
                 legend = FALSE,
                 
                 xlab = "Geschlecht des Kindes",
                 ylab = "Sozial-emotionale Fähigkeiten")
legend("topright",
       legend = c("männlich", "weiblich"),
       lty = c(1, 2),
       title = "Geschlecht des Geschwisterkindes"
)
title(main ="Interaktionseffekte" )

# Standardabweichung einzeichnen klappt noch nicht
for (i in 1:2) {
  for (j in 1:2) {
    x <- interaction(Hypo3$Geschlecht.Kinder, Hypo3$GeschlechtGesch)[(Hypo3$Geschlecht.Kinder == levels(Hypo3$Geschlecht.Kinder)[i] & Hypo3$GeschlechtGesch == levels(Hypo3$GeschlechtGesch)[j])]
    y <- Hypo3$SumSozEmo[(Hypo3$Geschlecht.Kinder == levels(Hypo3$Geschlecht.Kinder)[i] & Hypo3$GeschlechtGesch == levels(Hypo3$GeschlechtGesch)[j])]
    arrows(x0 = x, y0 = y - sd(y), x1 = x, y1 = y + sd(y), angle = 90, code = 3, length = 0.05)
  }
}











# Hypothese 3.2, 3.3, 3.4

# Überprüfen der Voraussetzungen
## Normalverteilung der Werte

## Normalverteilung
hist(Hypo3$SumSozEmo)
shapiro.test(Hypo3$SumSozEmo)

par(mfrow = c(1, 2))

hist(Hypo3[Hypo3$GeschlechtGesch == "weiblich", ]$SumSozEmo,
     main = "",
     xlab = "sozial-emotionale Fähigkeiten",
     ylab = "Häufigkeit bei weiblich")
hist(Hypo3[Hypo3$GeschlechtGesch == "männlich", ]$SumSozEmo,
     main = "",
     xlab = "sozial-emotionale Fähigkeiten",
     ylab = "Häufigkeit bei männlich")

par(mfrow = c(1, 2))

hist(Hypo3[Hypo3$GeschJungAlt == "Jünger", ]$SumSozEmo,
     main = "",
     xlab = "sozial-emotionale Fähigkeiten",
     ylab = "Häufigkeit bei jüngeren Geschwistern",
     )
abline(v = mean(Hypo3[Hypo3$GeschJungAlt == "Jünger", ]$SumSozEmo), 
       col = "blue")
abline(v= median(Hypo3[Hypo3$GeschJungAlt == "Jünger", ]$SumSozEmo), 
       col = "red")

hist(Hypo3[Hypo3$GeschJungAlt == "älter", ]$SumSozEmo,
     main = "",
     xlab = "sozial-emotionale Fähigkeiten",
     ylab = "Häufigkeit bei älteren Geschwistern")

abline(v = mean(Hypo3[Hypo3$GeschJungAlt == "älter", ]$SumSozEmo), 
       col = "blue")
abline(v = median(Hypo3[Hypo3$GeschJungAlt == "älter", ]$SumSozEmo), 
       col = "red")



shapiro.test(Hypo3[Hypo3$GeschlechtGesch == "weiblich", ]$SumSozEmo)
shapiro.test(Hypo3[Hypo3$GeschlechtGesch == "männlich", ]$SumSozEmo)

shapiro.test(Hypo3[Hypo3$Geschlecht.Kinder == "weiblich", ]$SumSozEmo)
shapiro.test(Hypo3[Hypo3$Geschlecht.Kinder == "männlich", ]$SumSozEmo)

shapiro.test(Hypo3[Hypo3$GeschJungAlt == "Jünger", ]$SumSozEmo)
shapiro.test(Hypo3[Hypo3$GeschJungAlt == "älter", ]$SumSozEmo)
        

# Varianzhomogenität
leveneTest(Hypo3$SumSozEmo, Hypo3$GeschlechtGesch)
leveneTest(Hypo3$SumSozEmo, Hypo3$Geschlecht.Kinder)
leveneTest(Hypo3$SumSozEmo, Hypo3$GeschJungAlt)
             

# t-Tests
#H3.2
t.test(SumSozEmo ~ GeschJungAlt, Hypo3, alternative = "two.sided", var.equal = TRUE)

#H3.3
t.test(SumSozEmo ~ GeschlechtGesch, Hypo3, alternative = "two.sided", var.equal = TRUE)

#H3.4
t.test(SumSozEmo ~ Geschlecht.Kinder, Hypo3, alternative = "two.sided", var.equal = FALSE)

