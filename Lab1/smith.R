library(Biostrings)

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

#seq1 = DNAString("GAATTCCTACTACGAAGAATTCCTACTACGAAACTACGAAAATTCCTACTACGA");
#seq2 = DNAString("GAATTCCTACTACGGAATTCCCCTCCCATAATTCCTACTACGA");
seq1 = "CGTGAATTCAT"
seq2 = "GACTTAC"

#seq1 = DNAString("ATCG");
#seq2 = DNAString("TCC");

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


for(i in 1:largo_2+1){
  
  for(j in 1:largo_1+1){
    
    # comparacion
    if( identical(seq2[i-1], seq1[j-1])){
      #Match
      res <- maximo(master[i-1,j-1] + match_valor , master[i-1,j] - gap_valor , master[i,j-1] - gap_valor );
      
    }else{
      #Mistmatch
      res <- maximo(master[i-1,j-1] - mismatch_valor , master[i-1,j] - gap_valor , master[i,j-1] - gap_valor );
    }
    #message("_____")
    #message("" ,seq2[i-1], seq1[j-1], " valor:  " , i,":",j ," valor maximo: ", res[1])
    #message("_____")
    contador = contador+1;
    
    #completa los valores en las matrices
    master[i,j] = res[1];
    direccion[i,j] = res[2];
    
  }
  
}


master

direccion

contador


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
    maximo_j = maximo_temporal
    maximo_valor = master[maximo_i,maximo_j];
  }
}

maximo_valor
maximo_i
maximo_j

#construye un camino con la posicion mas alta

vector1 = "P"
vector2 = ""
posicion = 1;
while(maximo_i > 1 || maximo_j > 1){
  
  if(direccion[maximo_i, maximo_j] == 1 ){ # match o dismatch diagonal
    paste(seq1[maximo_j],vector1)
    vector2[posicion] = seq2[maximo_i];
    posicion = posicion + 1;
    maximo_i = maximo_i - 1;
    maximo_j = maximo_j - 1;
  }else{
    if(direccion[maximo_i, maximo_j] == 2 ){ # match o dismatch diagonal
      vector1[posicion] = seq1[maximo_j];
      
      vector2[posicion] = " ";
      posicion = posicion + 1;
      maximo_i = maximo_i - 1;
    }else{
      if(direccion[maximo_i, maximo_j] == 3 ){ # match o dismatch diagonal
        vector1[posicion] = " ";
        vector2[posicion] = seq2[maximo_i];
        posicion = posicion + 1;
        maximo_j = maximo_j - 1;
      }
    }
  }
  
}

vector1[0]
vector2