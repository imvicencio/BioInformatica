setwd("~/Proyectos/BIOLAB1/Lab1");
library(multtest)
library(Biostrings)


seq = readDNAStringSet("Archivos/secuencia.fasta")
length(seq)
reverseComplement(seq)
alphabetFrequency(seq)
dinucleotideFrequency(seq)

secuencia <- as.character(seq[[1]])
nchar(secuencia)
longestConsecutive(secuencia, "A")
longestConsecutive(secuencia, "C")
longestConsecutive(secuencia, "G")
longestConsecutive(secuencia, "T")

