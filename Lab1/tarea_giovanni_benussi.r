#seq1<-"GAAGAATTCC"
#seq2<-"GGAATTCCCC"

#seq1 <- "GAAGA"
#seq2 <- "GGAAT"

seq1 <- "GAATTCCTACTACGAAGAATTCCTACTACGAAACTACGAAAATTCCTACTACGA"
seq2 <- "GAATTCCTACTACGGAATTCCCCTCCCATAATTCCTACTACGA"

cat(paste("Secuencia 1:", seq1,"\n"))
cat(paste("Secuencia 2:", seq2,"\n\n"))

largo_seq1 <- nchar(seq1)
largo_seq2 <- nchar(seq2)

matriz <- matrix(0, largo_seq1, largo_seq2)

i <- 1
j <- 1

valor_match <- 1
valor_mismatch <- -2
valor_gap <- -2

while(i <= largo_seq1) {
  j <- 1
  while(j <= largo_seq2) {
    # print(matriz)
    # print(paste(i, j))
    char1 <- substr(seq1, i , i)
    char2 <- substr(seq2, j , j)

    puntaje <- 0

    if(char1 != char2) {
      #print(paste(char1, " distinto de", char2))
      puntaje <- valor_mismatch
    } else {
      puntaje <- valor_match
    }

    puntaje_arriba           <- if( i > 1) matriz[ i - 1, j ] + valor_gap else  valor_gap
    puntaje_izquierda        <- if( j > 1) matriz[ i, j - 1 ] + valor_gap else  valor_gap
    puntaje_arriba_izquierda <- if( i > 1 && j > 1) matriz[ i - 1, j - 1 ]  + puntaje else puntaje
    # print("============")
    # print(puntaje_arriba)
    # print(puntaje_izquierda)
    # print(puntaje_arriba_izquierda)
    puntaje_maximo <- max(puntaje_arriba, puntaje_izquierda, puntaje_arriba_izquierda, 0)

    matriz[ i, j ] <- puntaje_maximo
    # print(puntaje)
    j <- j + 1
  }

  i <- i + 1
}

cat("Matriz:\n\n")
print(matriz)
cat("\n")

# salida <- paste(salida, 'a', sep='')

i <- largo_seq1
j <- largo_seq2

# print("===================================")

maximum_value_i <- 1
maximum_value_j <- 1
maximum_value   <- 0

i_aux <- 1
while( i_aux <= largo_seq1) {
    j_aux <- 1
    while(j_aux <= largo_seq2) {
        if( matriz[ i_aux, j_aux ] >= maximum_value) {
            maximum_value_i <- i_aux
            maximum_value_j <- j_aux
            maximum_value <- matriz[ i_aux, j_aux ]
        }
        j_aux <- j_aux + 1
    }
    i_aux <- i_aux + 1
}

# print("Maximum: ")
# print(maximum_value_i)
# print(maximum_value_j)
# print(maximum_value)

i <- maximum_value_i
j <- maximum_value_j

posiciones_maximo <- c()
i_aux <- 1
while( i_aux <= largo_seq1) {
    j_aux <- 1
    while(j_aux <= largo_seq2) {
        if( matriz[ i_aux, j_aux ] == maximum_value) {
            posiciones_maximo <- rbind(posiciones_maximo, c(i_aux, j_aux))
        }
        j_aux <- j_aux + 1
    }
    i_aux <- i_aux + 1
}

# print(posiciones_maximo)
posiciones_maximo_length <- nrow(posiciones_maximo)
i_aux <- 1
numero_secuencia <- 1
# print("i_aux")
# print(i_aux)
# print("posiciones_maximo_length")
# print(posiciones_maximo_length)
mejor_puntaje <- 0
mejor_salida_arriba <- ""
mejor_salida_abajo  <- ""
while(i_aux <= posiciones_maximo_length) {
    salida_arriba <- ""
    salida_abajo  <- ""
    i_maximo <- posiciones_maximo[ i_aux, 1]
    j_maximo <- posiciones_maximo[ i_aux, 2]
    i_aux <- i_aux + 1

    i <- i_maximo
    j <- j_maximo
    score <- 0
    repeat {
        puntaje_arriba           <- if( i > 1) matriz[ i - 1, j ] else  0
        puntaje_izquierda        <- if( j > 1) matriz[ i, j - 1 ] else  0
        puntaje_arriba_izquierda <- if( i > 1 && j > 1) matriz[ i - 1, j - 1 ] else 0
        # puntaje_arriba           <- if( i > 1) matriz[ i - 1, j ] + valor_gap else  valor_gap
        # puntaje_izquierda        <- if( j > 1) matriz[ i, j - 1 ] + valor_gap else  valor_gap
        # puntaje_arriba_izquierda <- if( i > 1 && j > 1) matriz[ i - 1, j - 1 ]  + puntaje else puntaje

        puntaje_maximo <- max(puntaje_arriba, puntaje_izquierda, puntaje_arriba_izquierda, 0)
        print(paste(i, j))
        # print(paste("Puntaje Arriba",puntaje_arriba))
        # print(paste("Puntaje Izquierda",puntaje_izquierda))
        # print(paste("Diagonal",puntaje_arriba_izquierda))
        # print(paste("Maximo",puntaje_maximo))

        char1 <- substr(seq1, i, i)
        char2 <- substr(seq2, j, j)

        # print(paste(i,j))

        i_new <- i
        j_new <- j

        if(char1 == char2){
            salida_arriba <- paste(char2, salida_arriba, sep='')
            salida_abajo <- paste(char1, salida_abajo, sep='')
            print(char2)
            print(char1)
            i_new <- i - 1
            j_new <- j - 1
            print("DIAGONAL (MATCH)")
            score <- score + valor_match
        } else {
            # me voy por arriba
            if(puntaje_maximo == puntaje_arriba) {
                score <- score + valor_gap
                print("ARRIBA")
                salida_arriba <- paste('-', salida_arriba, sep='')
                salida_abajo <- paste(char1, salida_abajo, sep='')
                print('-')
                print(char1)
                i_new <- i - 1
            } else if(puntaje_maximo == puntaje_arriba_izquierda) {
                score <- score + valor_mismatch
                print("DIAGONAL")
                # salida_arriba <- paste(char2, salida_arriba, sep='')
                # salida_abajo <- paste(char1, salida_abajo, sep='')
                salida_arriba <- paste(char2, salida_arriba, sep='')
                salida_abajo <- paste('-', salida_abajo, sep='')
                print(char2)
                print('-')
                i_new <- i - 1
                j_new <- j - 1
            } else if(puntaje_maximo == puntaje_izquierda) {
                score <- score + valor_gap
                print("IZQUIERDA")
                salida_arriba <- paste(char2, salida_arriba, sep='')
                salida_abajo <- paste('-', salida_abajo, sep='')
                print(char2)
                print('-')
                j_new <- j - 1
            }
        }


        if(i == 1 || j == 1){
            break
        }
        i <- i_new
        j <- j_new
    }

    # print("================")
    #
    # cat(paste("Maximo ", numero_secuencia,":", sep=''))
    #
    # cat("\n")
    # cat("\t")
    # cat(salida_arriba)
    # cat("\n")
    # cat("\t")
    # cat(salida_abajo)
    # cat("\n")
    # cat("\t")
    # cat(paste("Puntaje:", score))
    # cat("\n")
    # cat("\n")
    numero_secuencia <- numero_secuencia + 1
    if(score > mejor_puntaje) {
        mejor_puntaje <- score
        mejor_salida_arriba <- salida_arriba
        mejor_salida_abajo  <- salida_abajo
    }
}

cat("Salida:\n")
cat(paste("\t", mejor_salida_arriba,"\n"))
cat(paste("\t", mejor_salida_abajo,"\n"))
cat(paste("\t Puntaje:", mejor_puntaje,"\n"))
