# Analisis de smith -  Waterman utilizando la version que incluye el paquete Biostring de Bioconductor
library(Biostrings)

#seq1 <- "CGTGAATTCAT"
#seq2 <- "GACTTAC"
seq1 = "GAATTCCTACTACGAAGAATTCCTACTACGAAACTACGAAAATTCCTACTACGA"
seq2 = "GAATTCCTACTACGGAATTCCCCTCCCATAATTCCTACTACGA"

mat<-nucleotideSubstitutionMatrix(match = 5, mismatch = -3, baseOnly = TRUE)
mat
pairwiseAlignment(pattern = seq2, subject = seq1, type="local",substitutionMatrix=mat, gapOpening=0, gapExtension=-4)

mat