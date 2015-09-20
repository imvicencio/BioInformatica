library(multtest)
library(psych)
data(golub) # Datos de pacientes con leucemia 3051 genes
expresion=golub
genes=golub.gnames
clase=golub.cl

genes[2000,3]

expresion[2000,]

expresion[2000,clase == 0]   # pacientes de tipo ALL
expresion[2000,clase == 1]   # pacientes de tipo AML

describe(expresion[2000, clase == 0], skew=FALSE)
describe(expresion[2000, clase == 1], skew=FALSE)



par(mfrow=c(1, 2));
plot(1:27,expresion[2000,clase == 0],xlim=c(1,40),col=2, ylab="Expresión",
     xlab="Probes", main=genes[2000,3])
points(28:38,expresion[2000,clase == 1],pch=2,col=3) #pch = figuras, col = color 
boxplot(expresion[2000,clase == 0],expresion[2000,clase == 1], ylab="Expresion",
col=c("red","green"), names =c("ALL","AML"),main=genes[2000,3])


var_cont<- var.test(expresion[2000,clase == 0],expresion[2000,clase == 1])
var_cont