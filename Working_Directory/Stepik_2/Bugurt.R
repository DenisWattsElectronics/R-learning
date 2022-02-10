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

find_negative <- function(x){
  x[x < 0]
}

test_data <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))
test_data <- as.data.frame(list(V1 = c(NA, 0.5, 0.7, 8), V2 = c(-0.3, NA, 2, 1.2), V3 = c(2, -1, -5, -1.2)))
test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))

test_data

l <- apply(test_data, 2, function(x) x[!is.na(x) & x < 0])
return(l[sapply(l,length)>0])


l[sapply(l,length)>0]


test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), 
                                V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), 
                                V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), 
                                V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))

test_data
apply(test_data,2, mean,na.rm = T)
sapply(test_data, function(x) x[is.na(x)])

na_rm  <- function(y) {
  return(y[is.na(y)] <- mean(y[!na.rm=T]))
}

nnna <- function(y) {
  return(y[is.na(y)] <- mean(y[!is.na(y)]))
}
apply(test_data, 2, nnna)

test_data[] <- lapply(test_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
test_data

na_rm  <- function(x){
  return(as.data.frame(x[] <- lapply(x, function(y) ifelse(is.na(y), mean(y, na.rm = TRUE), y))))
}

na_rm(test_data)


d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
d <- lapply(d, function(x) x[x<0 & !is.na(x)] <- 0)
d

lapply(d, function(x) sum(x[x>0 & !is.na(x)]))

lapply(d, mean)
lapply(d, mean, na.rm = T)
lapply(d, function(x) x * 2)

lapply(d, function(x) x[x<0 & !is.na(x)])


lapply(d, function(x) x[x<0])
lapply(d, function(x) x[!is.na(x)])

lapply(d, function(y) ifelse(is.na(y), sum(y, na.rm = TRUE), y))





































