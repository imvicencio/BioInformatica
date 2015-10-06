# Ismael Vicencio
# BioInformatica 2-2015



# Funcion que calcula el max vecino para el algoritmo
maximo <- function(a,b,c){
  # a valor diagonal
  # b valor upper
  # c valor left
  
  max_valor = a;
  dir_valor = 1; #diagonal
  
  if(a < b){
    max_valor = b;
    dir_valor = 2;
    if(b < c){
      max_valor = c;
      dir_valor = 3;
    }
  }else{
    if(a < c){
      max_valor = c;
      dir_valor = 3;
    }
  }
  
  
  if(max_valor < 0){
    max_valor = 0;
    dir_valor = 1;
  }
  
  result <- c(max_valor, dir_valor) 
  return(result)
}

####################

#seq1 = "GAATTCCTACTACGAAGAATTCCTACTACGAAACTACGAAAATTCCTACTACGA"
#seq2 = "GAATTCCTACTACGGAATTCCCCTCCCATAATTCCTACTACGA"
seq1 = "CGTGAATTCAT"
seq2 = "GACTTAC"

#seq1 <- "ATCG"
#seq2 <- "TCC"


# Obtiene el tamaño de los vectores
largo_1 = nchar(seq1);
largo_2 = nchar(seq2);

# Master contiene el peso de cada accion
master = matrix(0, nrow = largo_2+1, ncol = largo_1+1);

# Direccion contiene el camino a seguir mediante el siguiente patron
#
#   1 2 
#   3 x  
#   
#   Donde X es el elemento seleccionado
direccion = matrix(0, nrow = largo_2+1, ncol = largo_1+1);

match_valor = 5;
mismatch_valor = 3;
gap_valor = 4;
contador = 0;


for(i in 1:largo_1+1){
  
  for(j in 1:largo_2+1){
    
    # comparacion
    valor1 = i-1
    valor2 = j-1
    tempo1 <-unlist(strsplit(seq1,""))[valor1:valor1] 
    tempo2 <- unlist(strsplit(seq2,""))[valor2:valor2]
    if( identical( tempo1,tempo2 )){
      #Match
      res <- maximo(master[j-1,i-1] + match_valor , master[j-1,i] - gap_valor , master[j,i-1] - gap_valor );
      
    }else{
      #Mistmatch
      res <- maximo(master[j-1,i-1] - mismatch_valor , master[j-1,i] - gap_valor , master[j,i-1] - gap_valor );
    }
    #message("_____")
    #message("La ",tempo2, " y " , tempo1, " valor:  " , j,":",i ," valor maximo: ", res[1])
    #message("_____")
    #contador = contador+1;
    
    #completa los valores en las matrices
    master[j,i] = res[1]
    direccion[j,i] = res[2];
    
  }
  
}

# Busca el mayor elemento en toda la matriz, fila por fila
maximo_i = 1;
maximo_j = 1;
maximo_valor = 0;
maximo_temporal = 0;
iterador = 1;

for (i in 1:largo_2+1) {
  maximo_temporal = which.max( master[i,] )
  if(master[i,maximo_temporal] > maximo_valor){
    maximo_i = i;
    maximo_j = maximo_temporal;
    maximo_valor = master[maximo_i,maximo_j];
  }
}

maximo_valor
maximo_i
maximo_j

vector1 <- ""
vector2 <- ""
posicion = 1;

salir = 0;
while(salir == 0 ){
  #message("dir: ", direccion[maximo_i, maximo_j])
  if(direccion[maximo_i, maximo_j] == 1 ){ # match o dismatch diagonal
    temporal <- unlist(strsplit(seq1,""))[maximo_j:maximo_j-1]
    vector1 <- paste(substr(vector1, 1, nchar(vector1)), temporal, sep = "");
    
    temporal2 <- unlist(strsplit(seq2,""))[maximo_i:maximo_i-1]
    vector2 <- paste(substr(vector2, 1, nchar(vector2)), temporal2, sep = "");
    
    #message("v1-> ",temporal , " --  v2->", temporal2 )
    temporal <- ""
    temporal2 <- ""
    
    posicion = posicion + 1;
    maximo_i = maximo_i - 1;
    maximo_j = maximo_j - 1;
  }else{
    if(direccion[maximo_i, maximo_j] == 2 ){ # UP GAP
      
      temporal <- "_"
      vector1 <- paste(substr(vector1, 1, nchar(vector1)), temporal, sep = "");
      temporal2 <- unlist(strsplit(seq2,""))[maximo_i:maximo_i-1]
      vector2 <- paste(substr(vector2, 1, nchar(vector2)), temporal2, sep = "");
      posicion = posicion + 1;
      maximo_i = maximo_i - 1;
      
      #message("v1-> ",temporal , " --  v2->", temporal2 )
      temporal <- ""
      temporal2 <- ""
      
    }else{
      if(direccion[maximo_i, maximo_j] == 3 ){ # Left GAP
        temporal <- unlist(strsplit(seq1,""))[maximo_j:maximo_j-1]
        vector1 <- paste(substr(vector1, 1, nchar(vector1)), temporal, sep = "");
        temporal2 <- "_"
        vector2 <- paste(substr(vector2, 1, nchar(vector2)), temporal2, sep = "");
        posicion = posicion + 1;
        maximo_j = maximo_j - 1;
        
        #message("v1-> ",temporal , " --  v2->", temporal2 )
        temporal <- ""
        temporal2 <- ""
      }
    }
  }
  
  if(maximo_i == 1){
    salir = 1;
    temporal2 <- unlist(strsplit(seq2,""))[maximo_i:maximo_i-1]
    vector2 <- paste(substr(vector2, 1, nchar(vector2)), temporal2, sep = "");
  }else{
    if(maximo_j == 1){
      salir = 1;
      temporal <- unlist(strsplit(seq1,""))[maximo_j:maximo_j-1]
      vector1 <- paste(substr(vector1, 1, nchar(vector1)), temporal, sep = "");
      
    }
  }
}

vector1
vector2

largo_1 = nchar(vector1)
largo_2 = nchar(vector2)
largo_1
largo_2

valor = 0;

#aplica penalidad
if(largo_1 == largo_2){
  for(i in 1:largo_1+1){
    
    temporal1 <- unlist(strsplit(vector1,""))[i:i-1]
    temporal2 <- unlist(strsplit(vector2,""))[i:i-1]
    
    #message("t1 -> ", temporal1, " t2-> ", temporal2)
    
    if(temporal1 == temporal2){
      valor = valor + match_valor;
    }else{
      if(temporal1 != "_" && temporal2 != "_"){
        valor = valor - mismatch_valor;
      }else{
        valor = valor - gap_valor;
      }
    }
    
  }
}else{
  message("Error...")
}

message("Valor: ", valor)
message("Secuencia 1: ", vector1)
message("Secuencia 2: ", vector2)
