boxplot(Nombre.de.trains.programmés ~ Région,
        data = DB_TER,
        subset=Région %in% c("Nord Pas de Calais","Alsace"),
        drop=TRUE,
        main="Nombre de trains TER programmés par mois",
        col=c("blue", "green"),
        xlab="Région",
        ylab="Nombre de TER programmés") 

var.test(Nombre.de.trains.programmés ~ Région, data= subset(DB_TER, Région=="Nord Pas de Calais" | Région=="Alsace"))
t.test(Nombre.de.trains.programmés ~ Région, data= subset(DB_TER, Région=="Nord Pas de Calais" | Région=="Alsace"), var.equal=TRUE)

boxplot(Nombre.de.trains.programmés ~ Région,
        data = DB_TER,
        subset=Région %in% c("Picardie","Midi Pyrénées"),
        drop=TRUE,
        main="Nombre de trains TER programmés par mois",
        col=c("blue", "green"),
        xlab="Région",
        ylab="Nombre de TER programmés") 
		
var.test(Nombre.de.trains.programmés ~ Région, data= subset(DB_TER, Région=="Picardie" | Région=="Midi Pyrénées"))
t.test(Nombre.de.trains.programmés ~ Région, data= subset(DB_TER, Région=="Picardie" | Région=="Midi Pyrénées"), var.equal=TRUE)


AOV <- aov(Taux.de.régularité ~ Mois, data= DB_TER)
summary(AOV)

summary(DB_TER$Région)