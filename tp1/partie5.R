hist(DB_TER$Nombre.de.trains.programmés)
hist(RA_TER$Nombre.de.trains.programmés)
hist(subset(DB_TER, Annee=="2018")$Nombre.de.trains.programmés)
plot(Taux.de.régularité ~ Annee, data = DB_TER)
boxplot(Taux.de.régularité ~ Annee, data = DB_TER)

boxplot(Taux.de.régularité ~ Mois, data = RA_TER)
boxplot(Nombre.de.trains.programmés ~ Mois, data = RA_TER)

RA_Juin = subset(RA_TER, Mois=="06")
boxplot(Nombre.de.trains.programmés ~ Annee, data = RA_Juin)
plot(Nombre.de.trains.programmés ~ Annee, data = RA_Juin)

RA_Mai = subset(RA_TER, Mois=="05")
boxplot(Nombre.de.trains.programmés ~ Annee, data = RA_Mai)
plot(Nombre.de.trains.programmés ~ Annee, data = RA_Mai)