############################################################################
############################################################################
#####                                                               ########
##### Laboratorio 1 - Bioinform?tica                                ########
#####                                                               ########
##### Desarrollo primera parte                                      ########
#####                                                               ########
##### Daniel Vega Araya                                             ########
##### Fecha: 01-10-2015                                             ########
#####                                                               ########
############################################################################
############################################################################

## Limpiamos la consola
cat("\014")

## Defino las secuencias w1 y w2 a comparar 
## descomentar seg?n el grupo a revisar

##
w1 <- "GAATTCCTACTACGAAGAATTCCTACTACGAAACTACGAAAATTCCTACTACGA"
w2 <- "GAATTCCTACTACGGAATTCCCCTCCCATAATTCCTACTACGA"

## Primer grupo
#w1="AGCACACA"
#w2="AGCACACA"

## Segundo grupo
#w1="ACACACTA"
#w2="ACA"

## Tercer grupo
#w1="CGTGAATTCAT"
#w2="AGCACACA"

## Cuarto grupo
#w1="CGTGAATTCAT"
#w2="GACTTAC"

## Quinto grupo
#w1="GAATTCCTACTACGAAGAATTCCTACTACGAAACTACGAAAATTCCTACTACGA"
#w2="GAATTCCTACTACGGAATTCCCCTCCCATAATTCCTACTACGA"


cat("\nSecuencias:\n\n")
cat("   seq1: ", w1,"\n")
cat("   seq2: ", w2,"\n\n")

## Obtengo el numero de filas y columnas de la matriz a crear,
columnas=nchar(w1)+1;
filas=nchar(w2)+1;

## genero la matriz, le add un '+1' para generar la primera 
## fila y columna con ceros
m=matrix(0,filas,columnas,byrow=T)

## Obtengo cada caracter de las secuencias para determinar el llenado de
## la matriz
sW1<-c(substring(w1, seq(1,columnas,1), seq(1,columnas,1)))
sW2<-c(substring(w2, seq(1,filas,1), seq(1,filas,1)))

## Se definen los siguientes valores para el algoritmo
##
##  1 match
## -2 mismatch
## -2 gap

##Comienzo a desarrollar el algoritmo
for (i in 2:filas) { 
  for (j in 2:columnas) {
    if(as.character(sW2[i-1])==as.character(sW1[j-1])){
      # match
      suma=1
    }
    else{
      # mismatch
      suma=-2
    }
    # gap
    w=-2
    m[i,j]=max(m[i-1,j-1]+suma,m[i-1,j]+w,m[i,j-1]+w,0)
  }
}

m

## Obtengo el m?ximo valor de la matriz generada y su posici?n
puntero=which(m==max(m),arr.ind = T)
nPuntero=length(puntero)/2
while(nPuntero>0){
  ## Recorro los caminos generados
  fi=puntero[nPuntero,1]
  col=puntero[nPuntero,2]
  chaz<-c(0)
  if(as.character(sW2[fi])!=as.character(sW1[col])){
    reverA <- c(sW1[col])
    reverB <- c("-")
  }
  else{
    reverA <- c("")
    reverB <- c("")
  }
  valor=0
  while(fi-1>0 && col-1>0){
    # veo si donde estoy parado es un match
    if(as.character(sW2[fi-1])==as.character(sW1[col-1])){
      # me muevo en diagonal
      reverA <- c(reverA, sW1[col-1])
      reverB <- c(reverB, sW2[fi-1])
      valor=valor+1
      fi=fi-1
      col=col-1
    }
    # si no es un match
    else{
      chaz<-c(chaz,abs(valor))
      valor=0
      # me muevo primero en diagonal
      if(m[fi-1,col-1]==max(m[fi-1,col-1],m[fi-1,col],m[fi,col-1]) && m[fi-1,col]==m[fi,col-1]){
        reverA <- c(reverA, sW1[col-1])
        reverB <- c(reverB, "-")
        valor=0
        fi=fi-1
        col=col-1
      }
      else{
      # me muevo a la izquierda
      if(m[fi,col-1]==max(m[fi-1,col-1],m[fi-1,col],m[fi,col-1])){
        reverA <- c(reverA, sW1[col-1])
        reverB <- c(reverB, "-")
        valor=0
        col=col-1
        fi=fi
      }
      # me muevo hacia arriba
      else{
        reverA <- c(reverA, "-")
        reverB <- c(reverB, sW2[fi-1])
        valor=0
        fi=fi-1
        col=col
      }
      }
    }
  }
  if(max(chaz)==0){
    valor=length(reverA)
    valor=valor-1
  }
  else{
    chaz<-c(chaz,abs(valor))
    valor=max(chaz)
  }
  ## Muestro el resultado
  cat("Traza alineamiento:\n\n")
  cat("  ",rev(reverA),"\n")
  cat("  ",rev(reverB),"\n\n")
  cat("Score: ",valor,"\n\n")
  
  #reviso otro camino en caso de existir
  nPuntero=nPuntero-1
}
cat("Nota: se ha generado un archivo con extensi?n csv, llamado 'salidaMatrizSmithWaterman.csv' \n      para revisar la matriz con los valores del algoritmo Smith-Waterman.\n\n")
## Para generar un excel y ver la matriz de manera 'legible'
write.csv(m, "./salidaMatrizSmithWaterman.csv")