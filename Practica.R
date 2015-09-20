library(MASS);
library(psych);
library(nortest);
data_axia=anorexia;
head(data_axia)

data_axia_cont = subset(data_axia , Treat  == 'Cont');
data_axia_cbt = subset(data_axia , Treat  == 'CBT');
data_axia_ft = subset(data_axia , Treat  == 'FT');

#Estadistica Control
describe(data_axia_cont,skew=FALSE)

#Estadistica CBT
describe(data_axia_cbt,skew=FALSE)

#Estadistica
describe(data_axia_ft,skew=FALSE) 

par(mfrow=c(1,3)); #Define numero de graficos a imprimir en pantalla
boxplot(data_axia_cont[,2:3], main="Cont", ylab="Weight[lbs]", col="cyan");
boxplot(data_axia_cbt[,2:3], main="Cbt", ylab="Weight[lbs]", col="green");
boxplot(data_axia_ft[,2:3], main="Ft", ylab="Weight[lbs]", col="gold");

par(mfrow=c(1,2));
plot(density(data_axia$Prewt), main="Prewt");
plot(density(data_axia$Postwt), main="Postwt");