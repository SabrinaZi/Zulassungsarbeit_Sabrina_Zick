
# Hypothese 2.1
# Kinder mit Geschwister haben bessere soz.-emo. Fähigkeiten als Kinder mit Geschwister
# benötigte Daten: Kinder, bei denen eine Einschätzung der Lehrkräfte vorliegt

## überprüfen der internen Konsistenz des Fragebogens für soz.emo. Fähigkeiten

alpha_soz_emo <- alpha(LehrerGesamt[, 3:23])
alpha_soz_emo
#-> Cohens alpha liegt bei 0.95


# nur Kinder, bei denen LKS vorliegt

Hypo21 <- GesamtSchule[!is.na(GesamtSchule$SumSozEmo),]

# Hypo21 enthält alle Daten, wo Daten von Lehrer und Kinder (gibt es also Geschwister) vorliegen, unabhängig davon, ob
# Daten der Eltern vorliegen

# Unterscheidung zwischen SRQ 1, 20 oder 2 und SRQ 0, SRQ 50 wird ausgeschlossen

Hypo21$Geschwister <- ifelse(Hypo21$SRQ_neu == 0, "0", NA)
Hypo21$Geschwister <- ifelse(Hypo21$SRQ_neu == 1 | Hypo21$SRQ_neu ==2 | Hypo21$SRQ_neu == 20, "1", Hypo21$Geschwister)


Hypo21$Geschwister <- factor(Hypo21$Geschwister, levels = c("0", "1"), labels = c("Nein", "Ja"))

Hypo21 |>
  group_by(Geschwister) |>
  summarise(mean(SumSozEmo), sd(SumSozEmo), n())

# Deskriptive Beschreibung von SumSozEmo abhängig von Geschwisterstatus
DeskrHypo21 <- describeBy(Hypo21$SumSozEmo, group = Hypo21$Geschwister)
xtable(DeskrHypo21$Nein)
xtable(DeskrHypo21$Ja)
plot(SumSozEmo ~ Geschwister, data = Hypo21)


# Voraussetzungen prüfen

## Normalverteilung
hist(Hypo21$SumSozEmo)
shapiro.test(Hypo21$SumSozEmo)

## Normalverteilung in den beiden Gruppen
shapiro.test(Hypo21[Hypo21$Geschwister == "Ja", ]$SumSozEmo)
shapiro.test(Hypo21[Hypo21$Geschwister == "Nein", ]$SumSozEmo)
par(mfrow = c(1, 2))
hist(Hypo21[Hypo21$Geschwister == "Ja", ]$SumSozEmo, xlab = "hat Geschwister", ylab = "Häufigkeit", main = "")
hist(Hypo21[Hypo21$Geschwister == "Nein", ]$SumSozEmo,  xlab = "hat keine Geschwister", ylab = "Häufigkeit", main = "")

## Varianzhomogenität

leveneTest(Hypo21$SumSozEmo, Hypo21$Geschwister)


t.test(SumSozEmo ~ Geschwister, Hypo21, alternative = "less", var.equal = TRUE)

# Test wird nicht signifikant, p-Wert bei 0.126, Mittelwert ist "nur" um 3 höher


# Power Analyse
mes(m.1 = 48, m.2 = 45.4, sd.1 =  11.5, sd.2 = 11.7, n.1 = 129, n.2= 41)
pwr.t2n.test(n1 = 41, n2= 129, d = .23, sig.level = .05, power = NULL,
             alternative = "greater")


pwr.2p.test(n = NULL, h = .23, sig.level = .05, power = 0.8,
             alternative = "greater")

# Hypo 2.2, 2.3
## Datensatz erstellen

Hypo22 <- Hypo21[Hypo21$SRQ_neu==1 | Hypo21$SRQ_neu==2 | Hypo21$SRQ_neu == 20,]
Hypo22 <- Hypo22[!is.na(Hypo22$SRQ_neu),]

## TODO: Daten beschreiben
describe(Hypo22)
describe(Hypo22$SumSozEmo)
describe(Hypo22$HarmonieElternSchule)



## Graphische Analyse

ggplot(Hypo22, aes( x=HarmonieKinder, y = SumSozEmo)) +
  geom_count() +
  labs(x = "Summe Harmonie Kinder", y = "sozial-Emotionale Fähigkeiten", 
       title = "Streudiagramm Harmonie Kinder vs. sozial-emotionale Fähigkeiten")

ggplot(Hypo22, aes( x=KonfliktKinder, y = SumSozEmo)) +
  geom_count()   +
  labs(x = "Summe Konflikt Kinder", y = "sozial-Emotionale Fähigkeiten", 
       title = "Streudiagramm Konflikt Kinder vs. sozial-emotionale Fähigkeiten")
ggplot(Hypo22, aes( x=HarmonieElternSchule, y = SumSozEmo)) +
  geom_count()   +
  labs(x = "Summe Harmonie Eltern", y = "sozial-Emotionale Fähigkeiten", 
       title = "Streudiagramm Harmonie Eltern vs. sozial-emotionale Fähigkeiten")


ggplot(Hypo22, aes( x=KonfliktElternSchule, y = SumSozEmo)) +
  geom_count()   +
  labs(x = "Summe Konflikt Eltern", y = "sozial-Emotionale Fähigkeiten", 
       title = "Streudiagramm Konflikt Eltern vs. sozial-emotionale Fähigkeiten")

# berechnen der Korrelation mit Ausreißern

### Berechnen von Korrelationen
cor.test(Hypo22$SumSozEmo, Hypo22$HarmonieKinder, alternativ = "greater")
cor.test(Hypo22$SumSozEmo, Hypo22$KonfliktKinder, alternativ = "less")
cor.test(Hypo22$SumSozEmo, Hypo22$HarmonieElternSchule, alternativ = "greater")
cor.test(Hypo22$SumSozEmo, Hypo22$KonfliktElternSchule, alternativ = "less")


## Hypo 2.2.1, 2.2.2, 2.3.1, 2.3.2 ohne Außreißer


##  Hypo 2.2.1 ermitteln von konditionallen Ausreißern mit Cooks Distance
regression221 <- lm(SumSozEmo ~ HarmonieKinder, data = Hypo22,  na.action = na.exclude)
cook221 <-as.data.frame( cooks.distance(regression221))
Hypo221ohneAusreiser <- Hypo22[cook221 < 4/123, ]
par(mfrow=c(2,2))
plot(regression221, 4, main = "Hypothese 2.2.1")
abline(h = 4/123, col = "red")
view(cook221<4/123)

cor.test(Hypo221ohneAusreiser$SumSozEmo, Hypo221ohneAusreiser$HarmonieKinder, alternativ = "greater")

##  Hypo 2.2.2 ermitteln von konditionallen Ausreißern mit Cooks Distance
regression222 <- lm(SumSozEmo ~ KonfliktKinder, data = Hypo22,  na.action = na.exclude)
cook222 <-as.data.frame( cooks.distance(regression222))
Hypo222ohneAusreiser <- Hypo22[cook222 < 4/122, ]
plot(regression222, 4, main = "Hypothese 2.2.2")
view(cook222<4/122)
abline(h = 4/122, col = "red")
cor.test(Hypo222ohneAusreiser$SumSozEmo, Hypo222ohneAusreiser$KonfliktKinder, alternativ = "less")

##  Hypo 2.3.1 ermitteln von konditionallen Ausreißern mit Cooks Distance
regression231 <- lm(SumSozEmo ~ HarmonieElternSchule, data = Hypo22,  na.action = na.exclude)
#influencePlot(regression, id.method = "identitfy", sub = "Circle size is proportional to Cook's Distance")
cook231 <-as.data.frame( cooks.distance(regression231))
#ausreiser <- Hypo22[cook > 4/99, ] # 4/n als Cooks Distance, dabei 8 potentielle Ausreiser identifiziert
Hypo231ohneAusreiser <- Hypo22[cook231 < 4/99, ]


plot(regression231, 4, main = "Hypothese 2.3.1")
abline(h = 4/99, col = "red")
view(cook231<4/99)


cor.test(Hypo231ohneAusreiser$SumSozEmo, Hypo231ohneAusreiser$HarmonieElternSchule, alternativ = "greater")

#ggplot(Hypo231ohneAusreiser, aes( x=HarmonieElternSchule, y = SumSozEmo)) +
 # geom_count()   +
  #labs(x = "Summe Harmonie Eltern", y = "sozial-Emotionale Fähigkeiten", 
   #    title = "Streudiagramm Harmonie Kinder vs. sozial-emotionale Fähigkeiten")


##  Hypo 2.3.2 ermitteln von konditionallen Ausreißern mit Cooks Distance
regression232 <- lm(SumSozEmo ~ KonfliktElternSchule, data = Hypo22,  na.action = na.exclude)
cook232 <-as.data.frame( cooks.distance(regression232))
Hypo232ohneAusreiser <- Hypo22[cook222 < 4/99, ]
plot(regression232, 4, main = "Hypothese 2.3.2")
abline(h = 4/99, col = "red")
view(cook232<4/99)
cor.test(Hypo232ohneAusreiser$SumSozEmo, Hypo232ohneAusreiser$KonfliktElternSchule, alternativ = "less")





## Regression von Hypo 2.2.1

### Prüfen der Voraussetzungen
regression <- lm(SumSozEmo ~ HarmonieKinder + AlterJahre.Kinder, data = Hypo22)

plot(fitted(regression), resid(regression), main = "Hypothese 2.2.1") # Homoskedastizität
bptest(regression)

qqnorm(resid(regression),  main = "Hypothese 2.2.1") # Normalität
qqline(resid(regression))

hist(rstandard(regression),     
     freq=T,     
     breaks=20,      
     main="Hypothese 2.2.1",     
     ylab="Häufigkeiten",     
     xlab="standardisierte Residuen",     
     col="darkgreen",     xlim=c(-5,5),     ylim=c(0,40))
# wäre nicht erfüllt

durbinWatsonTest(regression) # Unabhängigkeit der Residuen
summary (regression)


## Regression von Hypo 2.2.2

### Prüfen der Voraussetzungen
regression <- lm(SumSozEmo ~ KonfliktKinder + AlterJahre.Kinder, data = Hypo22)

plot(fitted(regression), resid(regression), main = "Hypothese 2.2.2") # Homoskedastizität
bptest(regression)

qqnorm(resid(regression), main = "Hypothese 2.2.2") # Normalität
qqline(resid(regression))

hist(rstandard(regression),     
     freq=T,     
     breaks=20,      
     main="Hypothese 2.2.2",     
     ylab="Häufigkeiten",     
     xlab="standardisierte Residuen",     
     col="darkgreen",     xlim=c(-5,5),     ylim=c(0,40))
# wäre nicht erfüllt

durbinWatsonTest(regression) # Unabhängigkeit der Residuen
summary (regression)


## Regression von Hypo 2.3.1

### Prüfen der Voraussetzungen für Regression, ohne Entfernen von Ausreisern

regression <- lm(SumSozEmo ~ HarmonieElternSchule + AlterJahre.Kinder, data = Hypo22)

plot(fitted(regression), resid(regression), main = "Hypothese 2.3.1") # Homoskedastizität
bptest(regression)

qqnorm(resid(regression), main = "Hypothese 2.3.1") # Normalität
qqline(resid(regression))

hist(rstandard(regression),     
     freq=T,     
     breaks=20,      
     main="Hypothese 2.3.1",     
     ylab="Häufigkeiten",     
     xlab="standardisierte Residuen",     
     col="darkgreen",     xlim=c(-5,5),     ylim=c(0,40))
# wäre nicht erfüllt

durbinWatsonTest(regression) # Unabhängigkeit der Residuen
summary (regression)


## Regression von Hypo 2.3.2

### regression test
regression <- lm(SumSozEmo ~ KonfliktElternSchule + AlterJahre.Kinder, data = Hypo22)
summary(regression)

### Prüfen der Voraussetzungen
regression <- lm(SumSozEmo ~ KonfliktElternSchule, data = Hypo22)

plot(fitted(regression), resid(regression), main = "Hypothese 2.3.2") # Homoskedastizität
bptest(regression)

qqnorm(resid(regression), main = "Hypothese 2.3.2") # Normalität
qqline(resid(regression))

hist(rstandard(regression),     
     freq=T,     
     breaks=20,      
     main="Hypothese 2.3.2",     
     ylab="Häufigkeiten",     
     xlab="standardisierte Residuen",     
     col="darkgreen",     xlim=c(-5,5),     ylim=c(0,40))
# wäre nicht erfüllt

durbinWatsonTest(regression) # Unabhängigkeit der Residuen
summary (regression)






### Prüfen der Voraussetzungen
regression221 <- lm(SumSozEmo ~ HarmonieKinder + AlterJahre.Kinder, data = Hypo22)
regression222 <- lm(SumSozEmo ~ KonfliktKinder + AlterJahre.Kinder, data = Hypo22)
regression231 <- lm(SumSozEmo ~ HarmonieElternSchule + AlterJahre.Kinder, data = Hypo22)
regression232 <- lm(SumSozEmo ~ KonfliktElternSchule + AlterJahre.Kinder, data = Hypo22)


### schöne Grafiken
par(mfrow = c(2,2))
plot(fitted(regression221), resid(regression221), main = "Hypothese 2.2.1") # Homoskedastizität
plot(fitted(regression222), resid(regression222), main = "Hypothese 2.2.2") # Homoskedastizität
plot(fitted(regression231), resid(regression231), main = "Hypothese 2.3.1") # Homoskedastizität
plot(fitted(regression232), resid(regression232), main = "Hypothese 2.3.2") # Homoskedastizität


par(mfrow = c(2,2))
qqnorm(resid(regression221),  main = "Hypothese 2.2.1") # Normalität
qqline(resid(regression221), col = "red")
qqnorm(resid(regression222),  main = "Hypothese 2.2.2") # Normalität
qqline(resid(regression222), col = "red")
qqnorm(resid(regression231),  main = "Hypothese 2.3.1") # Normalität
qqline(resid(regression231), col = "red")
qqnorm(resid(regression232),  main = "Hypothese 2.3.2") # Normalität
qqline(resid(regression232), col = "red")

par(mfrow = c(2,2))
hist(rstandard(regression221),     
     freq=T,     
     breaks=20,      
     main="Hypothese 2.2.1",     
     ylab="Häufigkeiten",     
     xlab="standardisierte Residuen",     
     col="darkgreen"
     )
hist(rstandard(regression222),     
     freq=T,     
     breaks=20,      
     main="Hypothese 2.2.2",     
     ylab="Häufigkeiten",     
     xlab="standardisierte Residuen",     
     col="darkgreen"
)
hist(rstandard(regression231),     
     freq=T,     
     breaks=20,      
     main="Hypothese 2.3.1",     
     ylab="Häufigkeiten",     
     xlab="standardisierte Residuen",     
     col="darkgreen"
)
hist(rstandard(regression232),     
      freq=T,     
      breaks=20,      
      main="Hypothese 2.3.2",     
      ylab="Häufigkeiten",     
      xlab="standardisierte Residuen",     
      col="darkgreen"
)

durbinWatsonTest(regression221) # Unabhängigkeit der Residuen
durbinWatsonTest(regression222)
durbinWatsonTest(regression231)
durbinWatsonTest(regression232)

### Ergebnisse

summary (regression221)
summary (regression222)
summary (regression231)
summary (regression232)



## Regression neu ohne Ausreißer


regression221oA <- lm(SumSozEmo ~ HarmonieKinder + AlterJahre.Kinder, data = Hypo221ohneAusreiser)
regression222oA <- lm(SumSozEmo ~ KonfliktKinder + AlterJahre.Kinder, data = Hypo222ohneAusreiser)
regression231oA <- lm(SumSozEmo ~ HarmonieElternSchule + AlterJahre.Kinder, data = Hypo231ohneAusreiser)
regression232oA <- lm(SumSozEmo ~ KonfliktElternSchule + AlterJahre.Kinder, data = Hypo232ohneAusreiser)
summary(regression221oA)
summary(regression222oA)
summary(regression231oA)
summary(regression232oA)

