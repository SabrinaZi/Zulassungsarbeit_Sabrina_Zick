# Deskriptive Beschreibung der Daten  

## Variablenbeschreibung der benötigten Variablen von Eltern und Kindern

DeskrHypo1 <- describe(select(GesamtSchule, AlterJahre.Kinder, Geschlecht.Kinder, SRQ_neu, SibG_6, SibA_6, HarmonieKinder, KonfliktKinder, HarmonieElternSchule, KonfliktElternSchule, SumSozEmo))

HVHypo1 <- xtable(DeskrHypo1)
print(HVHypo1)


## Anzahl Kinder je SRQ Angabe
table(ElternKinderSchule$SRQ_neu)

## Geschlecht der Kinder
table(GesamtSchule$Geschlecht.Kinder)




# Zusammenhang von Harmonie und Konflikt
cor.test(GesamtSchule$HarmonieKinder, GesamtSchule$KonfliktKinder)
cor.test(GesamtSchule$HarmonieElternSchule, GesamtSchule$KonfliktElternSchule)

par(mfrow = c(1, 2))

plot(GesamtSchule$HarmonieKinder, GesamtSchule$KonfliktKinder, 
     xlab = "Summe Harmonie", ylab = "Summe Konflikt",    
     main = "Streudiagramm Harmonie und Konflikt Kinder",
     pch = 16)

plot(GesamtSchule$HarmonieElternSchule, GesamtSchule$KonfliktElternSchule, 
     xlab = "Summe Harmonie", ylab = "Summe Konflikt", 
     main = "Streudiagramm Harmonie und Konflikt Eltern",
     pch = 16)

## alternative Darstellung


ggplot(GesamtSchule, aes( x=HarmonieKinder, y = KonfliktKinder)) +
  geom_count() + scale_size_continuous(range = c(3, 8))  +
  labs(x = "Summe Harmonie", y = "Summe Konflikt", 
       title = "Streudiagramm Harmonie und Konflikt Kinder")

ggplot(GesamtSchule, aes( x=HarmonieElternSchule, y = KonfliktElternSchule)) +
  geom_count() + scale_size_continuous(range = c(3, 8))  +
  labs(x = "Summe Harmonie", y = "Summe Konflikt", 
       title = "Streudiagramm Harmonie und Konflikt Kinder")






# Hypothese 1
## festlegen des entsprechenden Datensatzes für Hpyothese 1
## eingschlossen wird, wenn sowohl für Kind als auch für Eltern ein Datensatz vorliegt
## und SRQ = 1 oder SRQ = 2 oder SRQ =20 gilt

## TODO: klären, woher die kompletten NA Zeilen in Hypo1 kommen

Hypo1 <- ElternKinderSchuleElternDa [ElternKinderSchuleElternDa$EF_SRQ_012_6 == "1" | 
                                     ElternKinderSchuleElternDa$EF_SRQ_012_6 == "2" |
                                       ElternKinderSchuleElternDa$EF_SRQ_012_6 == "20"  , ]
Hypo1 <- Hypo1[!is.na(Hypo1$EF_SRQ_012_6) &
               !is.na(Hypo1$HarmonieKinder) &
               !is.na(Hypo1$KonfliktKinder) &
               !is.na(Hypo1$HarmonieElternSchule) &
               !is.na(Hypo1$KonfliktElternSchule), ]

Ausschuss <-  Hypo1 |>
  filter(abs(SibA_6 - EF_Sib_Age_6) > 0.5) |>
  select(SibA_6, EF_Sib_Age_6)

# 225 Daten + 32 Ausschuss
Hypo1 <- Hypo1 |>
  filter(abs(SibA_6 - EF_Sib_Age_6) <= 0.5)


## jetzt noch 191 Daten da


## TODO: Visualisierung der Daten







## überprüfen der internen Konsistenz
alpha(subset(Hypo1, select = c(netteDinge.Kinder, teilen.Kinder, 
                             Gefühle.Kinder, Geheimnis, helfen.Kinder, Spaß.Kinder, zusammen.Kinder)),
      check.keys = TRUE)

alpha(subset(Hypo1, select = c(wütend.Kinder, gemein.Kinder, streiten.Kinder, schimpfen.Kinder, 
                             besser.Kinder, Meinung.Kinder, Neid.Kinder)),
      check.keys = TRUE)

## Harmonie: Alpha = .79 -> gut
## Konflikt: Alpha = .72 -> gut

alpha(subset(Hypo1, select = c(netteDinge.Eltern, teilen.Eltern,
                             Gefühle.Eltern, helfen.Eltern, Spaß.Eltern, zusammen.Eltern)))
alpha(subset(Hypo1, select = c(wütend.Eltern, gemein.Eltern, streiten.Eltern, schimpfen.Eltern,
                             besser.Eltern, Meinung.Eltern, Neid.Eltern)))

## Harmonie: Alpha = .84 -> sehr gut
## Konflikt: Alpha = .88 -> sehr gut




describe(select(Hypo1, c(HarmonieKinder, HarmonieElternSchule)))



ggplot(Hypo1, aes( x=HarmonieKinder, y = HarmonieElternSchule)) +
  geom_count() + scale_size_continuous(range = c(3, 8))   +
  labs(x = "Summe Harmonie Kinder", y = "Summe Harmonie Eltern", 
       title = "Streudiagramm Harmonie Eltern vs Kinder")

ggplot(Hypo1, aes( x=KonfliktKinder, y = KonfliktElternSchule)) +
  geom_count() + scale_size_continuous(range = c(3, 8))   +
  labs(x = "Summe Konfikt Kinder", y = "Summe Konflikt Eltern", 
       title = "Streudiagramm Konflikt Eltern vs Kinder")

## alternative Darstellung

par(mfrow = c(1, 2))

plot(GesamtSchule$HarmonieKinder, GesamtSchule$HarmonieElternSchule, 
     xlab = "Summe Harmonie Kinder", ylab = "Summe Harmonie Eltern",    
     main = "Streudiagramm Harmonie Eltern vs. Kinder",
     pch = 16)

abline(lm (GesamtSchule$HarmonieElternSchule ~ GesamtSchule$HarmonieKinder))

plot(GesamtSchule$KonfliktKinder, GesamtSchule$KonfliktElternSchule, 
     xlab = "Summe Konflikt Kinder", ylab = "Summe Konflikt Eltern", 
     main = "Streudiagramm Konflikt Eltern vs. Kinder",
     pch = 16)

abline(lm (GesamtSchule$KonfliktElternSchule ~ GesamtSchule$KonfliktKinder))

# Berechnen der Korrelation und überprüfen der Signifikanz

cor.test(Hypo1$HarmonieKinder, Hypo1$HarmonieElternSchule, "greater")

cor.test(Hypo1$KonfliktKinder, Hypo1$KonfliktElternSchule, "greater")



# Ausreißeranalyse
regression11 <- lm(HarmonieKinder ~ HarmonieElternSchule, data = Hypo1,  na.action = na.exclude)
cook11 <-as.data.frame( cooks.distance(regression11))
Hypo11ohneAusreiser <- Hypo1[cook11 < 4/191, ]
plot(regression11, 4)
view(cook11<4/191)
cor.test(Hypo11ohneAusreiser$HarmonieKinder, Hypo11ohneAusreiser$HarmonieElternSchule, alternativ = "greater")


regression12 <- lm(KonfliktKinder ~ KonfliktElternSchule, data = Hypo1,  na.action = na.exclude)
cook12 <-as.data.frame( cooks.distance(regression12))
Hypo12ohneAusreiser <- Hypo1[cook12 < 4/191, ]
plot(regression12, 4)
view(cook12<4/191)
cor.test(Hypo12ohneAusreiser$KonfliktKinder, Hypo12ohneAusreiser$KonfliktElternSchule, alternativ = "greater")



# Korrelationstabelle aller Studienvariablen
GesamtSchule |>
cor(AlterJahre.Kinder, Geschlecht.Kinder, HarmonieKinder, KonfliktKinder, HarmonieElternSchule, KonfliktElternSchule, SumSozEmo)

CorVariablen <- select(GesamtSchule, AlterJahre.Kinder, SibA_6, HarmonieKinder, KonfliktKinder, HarmonieElternSchule, KonfliktElternSchule, SumSozEmo)

cor(CorVariablen, use = "pairwise.complete.obs")
cor.test(CorVariablen, use = "pairwise.complete.obs")
xtable(summary(correlation(CorVariablen), p_adjust = "none"))









