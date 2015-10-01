vector1 <- ""
vector2 <- "alabra"

unlist(strsplit(vector2,""))[2:2]
paste(substr(vector1, 1, nchar(vector1)), unlist(strsplit(vector2,""))[2:2], sep = "");


