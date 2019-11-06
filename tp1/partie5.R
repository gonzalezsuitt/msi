hist(DB_TER$Nombre.de.trains.programm�s)
hist(RA_TER$Nombre.de.trains.programm�s)
hist(subset(DB_TER, Annee=="2018")$Nombre.de.trains.programm�s)
plot(Taux.de.r�gularit� ~ Annee, data = DB_TER)
boxplot(Taux.de.r�gularit� ~ Annee, data = DB_TER)

boxplot(Taux.de.r�gularit� ~ Mois, data = RA_TER)
boxplot(Nombre.de.trains.programm�s ~ Mois, data = RA_TER)

RA_Juin = subset(RA_TER, Mois=="06")
boxplot(Nombre.de.trains.programm�s ~ Annee, data = RA_Juin)
plot(Nombre.de.trains.programm�s ~ Annee, data = RA_Juin)

RA_Mai = subset(RA_TER, Mois=="05")
boxplot(Nombre.de.trains.programm�s ~ Annee, data = RA_Mai)
plot(Nombre.de.trains.programm�s ~ Annee, data = RA_Mai)