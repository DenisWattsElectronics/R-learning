# steps 2 - 3 for vs apply 
library(ggplot2)

data(diamonds)
str(diamonds)

min_size <- numeric(nrow(diamonds))
for (i in 1:nrow(diamonds)){
  min_size[i] <-  min(diamonds[i, 8:10])
}

min_size <- apply(X = diamonds[,8:10], FUN = min, MARGIN = 1)
min_size













