# load the library
library(mlbench)

# load the dataset
data(PimaIndiansDiabetes)

# variables
colnames(PimaIndiansDiabetes)

# pregnant	Number of times pregnant
# glucose	Plasma glucose concentration (glucose tolerance test)
# pressure	Diastolic blood pressure (mm Hg)
# triceps	Triceps skin fold thickness (mm)
# insulin	2-Hour serum insulin (mu U/ml)
# mass	Body mass index (weight in kg/(height in m)\^2)
# pedigree	Diabetes pedigree function
# age	Age (years)
# diabetes	Class variable (test for diabetes)

# display first 20 rows of data
head(PimaIndiansDiabetes, n=20)

# show dimensions
dim(PimaIndiansDiabetes)

# show data types
sapply(PimaIndiansDiabetes, class)

# distribution of class variable
y <- PimaIndiansDiabetes$diabetes
cbind(freq=table(y), percentage=prop.table(table(y))*100)

summary(PimaIndiansDiabetes)

# Scatterplot - "das mit den Punkten"
glucose <- PimaIndiansDiabetes$glucose
plot(glucose)
mean(glucose)
median(glucose)
abline(h = mean(glucose), col = "red", lwd = 3)
abline(h = median(glucose), col = "blue", lwd = 3)
legend(x = 190, legend = c ("Mittelwert", "Median"), col = c("red","blue"),lty=1)

# H?ufigkeiten als Historgramm plotten
hist(glucose, breaks=seq(0,199,length=200))
abline(v = mean(glucose), col = "red", lwd = 3)
abline(v = median(glucose), col = "blue", lwd = 3)
legend(x = 15, legend = c ("Mittelwert", "Median"), col = c("red","blue"),lty=1)


# Modus: H?ufigkeitstabelle und hieraus die Zahl mit der gr??ten H?ufigkeit ablesen.
t <- table(PimaIndiansDiabetes$glucose)
head(t)
names(t[t == max(t)])

#Quartile
q25 <- quantile(glucose, 0.25)
q75 <- quantile(glucose, 0.75)
plot(glucose)
abline(h = q25, col = "red", lwd = 3)   
abline(h = mean(glucose), col = "blue", lwd = 3)   
abline(h = q75, col = "gold4", lwd = 3)        
legend(x = 40, legend = c ("Q25", "Q50", "Q75"), col = c("red","blue","gold4"),lty=1)

#Nochmal in der H?ufigkeitsverteilung plotten
# H?ufigkeiten als Historgramm plotten
hist(glucose, breaks=seq(0,199,length=200))
abline(v = q25, col = "red", lwd = 3)   
abline(v = mean(glucose), col = "blue", lwd = 3)   
abline(v = q75, col = "gold4", lwd = 3)        
legend(x = 15, legend = c ("Q25", "Q50", "Q75"), col = c("red","blue","gold4"),lty=1)


#Andere Plotm?glichkeit: Boxplot
boxplot(glucose)
abline(h = q25, col = "red", lty = 2, lwd = 3)   
abline(h = median(glucose), col = "blue",lty = 2, lwd = 3)   
abline(h = q75, col = "gold4",lty = 2, lwd = 3)   
legend("bottomleft", inset = .02, legend = c ("Q25", "Q50=Median", "Q75"), col = c("red","blue","gold4"),lty=1)

#Boxplot nach einem Kenwert aufsplitten
boxplot(glucose~PimaIndiansDiabetes$diabetes)

#Standardabweichung
sd(glucose)

#SD f?r die Komplette Tabelle 
sapply(PimaIndiansDiabetes[,1:8],sd)

#Boxplot f?r pedigree im Vergleich (geringere Streuung)
boxplot(PimaIndiansDiabetes$pedigree)

#sind die beiden vergleichbar? Nein! 
boxplot(PimaIndiansDiabetes[,1:8])

#Standardisierung erforderlich
PimaIndiansDiabetes$Zpedigree <- scale(PimaIndiansDiabetes$pedigree)
PimaIndiansDiabetes$Zglucose <- scale(PimaIndiansDiabetes$glucose)
PimaIndiansDiabetes$Zpregnant <- scale(PimaIndiansDiabetes$pregnant)
PimaIndiansDiabetes$Zpressure <- scale(PimaIndiansDiabetes$pressure)
summary(PimaIndiansDiabetes)

#SD f?r die standardisierten Werte 
sapply(PimaIndiansDiabetes[,10:13],sd)

#Boxplot f?r die Standardisierten Werte
boxplot(PimaIndiansDiabetes[,10:13])

#Schiefe
#rechtsschiefe Verteilung
hist(PimaIndiansDiabetes$Zpedigree)
abline(v = mean(PimaIndiansDiabetes$Zpedigree), col = "blue", lwd = 3) 


#Korrelationsmatrix
cor(PimaIndiansDiabetes[,10:13])

#Plots bzgl. zwei Variablen
ggplot(aes(x=pregnant, y=age), data=PimaIndiansDiabetes)+geom_point()
ggplot(aes(x=insulin, y=triceps), data=PimaIndiansDiabetes)+geom_point()
ggplot(data=PimaIndiansDiabetes, aes(x=pregnant, y=age, fill=as.factor(pregnant)))+
  geom_boxplot()
boxplot(age~pregnant,data = PimaIndiansDiabetes)


library(GGally)
ggpairs(PimaIndiansDiabetes[,1:9], aes(color = diabetes))

library(ggplot2)
ggplot(PimaIndiansDiabetes, aes(triceps, mass)) + geom_point() + geom_smooth(method=lm)

#Verteilung approximieren
hist(PimaIndiansDiabetes$glucose, breaks = 100, prob = TRUE)
lines(density(PimaIndiansDiabetes$glucose), col="red")
