############# TP 2 - MSI ###############


#### Partie 1. Présentation de l'étude

### Q1.	Chargez le jeu de données avec la commande  
data(swiss)

### Q2.	Assignez-le à un objet de votre choix, par exemple  DB_swiss=swiss.
DB_swiss = swiss


#### Partie 2. Analyse descriptive

### Q3.	Quelle est la variable la plus dispersée ? Pour quelle variable les provinces sont-elles les plus homogènes ? 
### (suggestion: tracer une boîte à moustaches pour l'ensemble des variables considérées et utiliser IQR pour mesurer 
### l'écart inter-quartile, on pourra utiliser la fonction apply pour répéter une même opération).
boxplot(DB_swiss,col="lightblue", las = 3, cex = 0.5)

IQR_DB_swiss = apply(X = DB_swiss, MARGIN = 2, FUN = IQR)
barplot(IQR_DB_swiss,main="Ecarts interquartiles", las = 3)


### Q4.	Déterminez le pourcentage moyen d'hommes travaillant dans l'agriculture pour les provinces où au moins 4/5 
### de la population est catholique.  Comparez cette valeur à la moyenne de l'échantillon.
DB_swiss$Bool_Catho80 = DB_swiss$Catholic>=(4/5)*100

mean(DB_swiss$Agriculture[DB_swiss$Bool_Catho80=="TRUE"])

mean(DB_swiss$Agriculture)-mean(DB_swiss$Agriculture[DB_swiss$Bool_Catho80=="TRUE"])


#### Partie 3. Tests

### Q5.	On suppose que la fertilité moyenne en France est de 80%, peut-on dire avec un risque à 1% que la fertilité 
### moyenne suisse est inférieure à celle française ?

# Cas 1 : test unilatéral à gauche sur m avec sigma inconnu, l'écart centré réduit suit une loi de student à n-1 
# degrés de libertÃ©
m0 = 80
n = nrow(DB_swiss)
t = qt(0.99, n-1)

Xbar=mean(DB_swiss$Fertility)
sigmabar=sd(DB_swiss$Fertility)

SeuilHmoins = m0 - t*sigmabar/sqrt(n) # sigmabar est s* et pas s, on utilise n et pas n-1
Xbar >= SeuilHmoins

# ou directement avec le test de student implémenté sous R :
alpha = 0.01
TestFrCh = t.test(DB_swiss$Fertility, mu = m0, conf.level = 1-alpha, alternative = "less")
if (TestFrCh$p.value > alpha){
  print("on garde H0")
} else {
  print("on rejette H0")
}
  
### Q6.	Peut-on affirmer avec un risque de 5% que la moyenne du taux de fertilité des provinces avec une majorité de 
### catholiques est égale à celle des cantons avec une majorité de protestants ?

DB_swiss$Bool_Catho50 = DB_swiss$Catholic>=50
  
boxplot(Fertility ~ Bool_Catho50, data = DB_swiss, names = c("Majorité protestante","Majorité catholique"), col = c("blue", "green"))

# comparaison des variances 
var.test(Fertility ~ Bool_Catho50, data = DB_swiss)

# Comparaison des moyennes 
t.test(Fertility ~ Bool_Catho50, data = DB_swiss)

#Rejet de H0


#### Partie 4. Régression linéaire simple

### Q7.	Existe-t-il un lien entre le pourcentage de catholiques et le taux de fertilité ?
cov(DB_swiss$Fertility, DB_swiss$Catholic)
cor(DB_swiss$Fertility, DB_swiss$Catholic, method = "pearson")

### Q8.	Peut-on expliquer la fertilité par l'appartenance religieuse ? 
Reg1=lm(Fertility ~ Catholic, data = DB_swiss)
str(Reg1)
summary(Reg1)

## a. Tracer le nuage de points associé, puis y superposer la droite de régression linéaire.

install.packages("ggplot2")
library("ggplot2")

ggplot(data = DB_swiss, aes(y = Fertility, x = Catholic))+
  geom_point()+
  theme_classic()
  
ggplot(data = DB_swiss, aes(y = Fertility, x = Catholic))+
  geom_point()+
  theme_classic()+
  geom_smooth(colour="red", method="lm", se = FALSE)+
  annotate("text", x = 25, y = 90, label = "Fertility (%) = 64.43 + 0.14 x Catholic (%)")

## b. Ajoutez l'intervalle de confiance à 95% de l'estimation. 

ggplot(data = DB_swiss, aes(y = Fertility, x = Catholic))+
  geom_point()+
  theme_classic()+
  geom_smooth(colour="red", method="lm", se=TRUE, level = 0.95 , fill="red")+
  annotate("text", x = 25, y = 90, label = "Fertility (%) = 64.43 + 0.14 x Catholic (%)")

## c. Ecrire l'équation du modèle et interpréter les coefficients estimés.


### Q9.	Analyser les résidus du modèle de régression préalablement produit : 
### vérifier que les résidus se distribuent selon une loi normale de moyenne nulle. 

## a.	Représenter graphiquement la distribution des résidus
plot(DB_swiss$Agriculture, resid(Reg1), ylab="Residus", xlab="taux d'agriculture", 
     main = "Graphique des résidus", abline(0, 0) )

mean(resid(Reg1))

## b.	Exécuter le code : plot(Reg1) et interprétez les graphiques affichés.

# help : https://stat.ethz.ch/R-manual/R-devel/library/stats/html/plot.lm.html 
# et https://data.library.virginia.edu/diagnostic-plots/ 

plot(Reg1)

### Q10.	Utilisez vos résultats pour prédire le taux de fertilité d'une province avec 53% de catholiques 
predict(Reg1, data.frame(Catholic = c(0.53)), level = 0.95, interval = "confidence")

### Q11.	Peut-on expliquer la fertilité par le taux d'éducation ? Effectuez la régression permettant de 
### répondre à cette question (Reg2).

Reg2=lm(Fertility ~ Education, data = DB_swiss)
summary(Reg2)

### Q12.	Tracez le nuage de points correspondant à la question ci-dessous. En utilisant le package et 
### la commande ggplot, ajoutez successivement

ggplot(data = DB_swiss, aes(y = Fertility, x = Education))+
  geom_point()+
  theme_classic()

## a.	La droite de régression (affichez l'équation du modèle sur le graphique)

ggplot(data = DB_swiss, aes(y = Fertility, x = Education))+
  geom_point()+
  theme_classic()+
  geom_smooth(colour="red", method="lm", se=FALSE, level = 0.95 , fill="red")+
  annotate("text", x = 25, y = 90, label = "Fertility (%) = 79.61 - 0.86 x Education (%)")+

## b.	l'intervalle de confiance à 95% de la droite de régression
  
  ggplot(data = DB_swiss, aes(y = Fertility, x = Education))+
  geom_point()+
  theme_classic()+
  geom_smooth(colour="red", method="lm", se=TRUE, level = 0.95 , fill="red")+
  annotate("text", x = 25, y = 90, label = "Fertility (%) = 79.61 - 0.86 x Education (%)")

## c.	l'intervalle de confiance à 95% de prévision.

int_pred<- predict(Reg2, interval = "prediction")
DB_swiss <-cbind(DB_swiss, int_pred)

colnames(DB_swiss)[colnames(DB_swiss)=="fit"]<-"fit_Reg2"
colnames(DB_swiss)[colnames(DB_swiss)=="lwr"]<-"lwr_Reg2"
colnames(DB_swiss)[colnames(DB_swiss)=="upr"]<-"upr_Reg2"
head(DB_swiss)

ggplot(data = DB_swiss, aes(y = Fertility, x = Education))+
  geom_point()+
  theme_classic()+
  geom_smooth(colour="red", method="lm", se=TRUE, level = 0.95 , fill="red")+
  annotate("text", x = 25, y = 90, label = "Fertility (%) = 79.61 - 0.86 x Education (%)")+
  geom_line(aes(y=lwr_Reg2), color = "green")+
  geom_line(aes(y=upr_Reg2), color = "blue")

### Q13.	Comparez la puissance explicative des deux modèles ci-dessus. Quel modèle privilégier ?
summary(Reg1)$r.squared
summary(Reg1)$adj.r.squared

summary(Reg2)$r.squared
summary(Reg2)$adj.r.squared

if (summary(Reg1)$adj.r.squared > summary(Reg2)$adj.r.squared){
  print("on privilégie Reg1")
} else {
  print("on privilégie Reg2")
}

### Q14.	Selon vous, quelle variable est susceptible d'expliquer la variable « Examination » ?

cor(DB_swiss)

cor(DB_swiss$Examination, DB_swiss$Education)

### Q15.	Ecrivez l'équation associée à votre modèle, et réaliser une analyse complète de ce modèle 
### (régression, analyse des résidus, représentation graphique des résultats).

Reg3=lm(Examination ~ Education, data = DB_swiss)
summary(Reg3)

plot(DB_swiss$Education, resid(Reg3), ylab="Residus", xlab="taux d'éducation", 
     main = "Graphique des résidus", abline(0, 0) )
mean(resid(Reg3))

plot(Reg3)

int_pred<- predict(Reg3, interval = "prediction")
DB_swiss <-cbind(DB_swiss, int_pred)

colnames(DB_swiss)[colnames(DB_swiss)=="fit"]<-"fit_Reg3"
colnames(DB_swiss)[colnames(DB_swiss)=="lwr"]<-"lwr_Reg3"
colnames(DB_swiss)[colnames(DB_swiss)=="upr"]<-"upr_Reg3"
head(DB_swiss)

ggplot(data = DB_swiss, aes(y = Examination, x = Education))+
  geom_point()+
  theme_classic()+
  geom_smooth(colour="red", method="lm", se=TRUE, level = 0.95 , fill="red")+
  annotate("text", x = 20, y = 50, label = "Examination (%) = 10.13 - 0.58 x Education (%)")+
  geom_line(aes(y=lwr_Reg3), color = "green")+
  geom_line(aes(y=upr_Reg3), color = "blue")


#### Partie 4. Régression linéaire multiple

### Q16.	Nous souhaitons essayer d'expliquer le taux de fertilité simultanément par deux variables, 
### le pourcentage de catholiques et le taux d'éducation.

## a.	Ecrivez le modèle théorique que l'on souhaite estimer.

# Fertility = beta_0 + beta_1 x Catholic 6 beta_2 x Education

## b. Estimez ce modèle.
Reg4=lm(Fertility ~ Catholic + Education, data = DB_swiss)
summary(Reg4)

## c. Discutez la qualité du modèle (significativité globale du modèle, puissance explicative, distribution des résidus).

## d. Interprétez les estimateurs de beta_0, beta_1, et beta_2) ^. 
