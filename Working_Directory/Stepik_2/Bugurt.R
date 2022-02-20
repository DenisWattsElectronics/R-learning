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

dataset <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", 
                                         "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), 
                                expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 
                                               108.03, 111.83)))
test_data
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
names
str(dataset)
dataset
my_names <- function (dataset, names){
  return(sapply(names, function(x) dataset[grepl(x, dataset$name),]))
}

my_names <- function (dataset, names){
  return(as.data.frame(t(sapply(names, function(x) dataset[grepl(x, dataset$name),]))))
}

my_names <- function (dataset, names){
  z <- as.data.frame(t(sapply(names, function(x) dataset[grepl(x, dataset$name),])))
  z[, 2] <- as.numeric(z[, 2])
  return(z)
  }


my_names(test_data, names)
grepl(names[1], dataset$name)
x[, 2] <- as.numeric(x[, 2])
x

###
set.seed(0)
y <- rnorm(100)
factor1 <- rep(1:2, each=50)
factor2 <- rep(3:4, 50)
factors <- c("factor1", "factor2")
aaa <- lm(as.formula(paste("y~", paste(factors, collapse="+"))))
aaa$coefficients

str(swiss)
shares <- sapply(swiss[-1], function(x) shapiro.test(x)$p.value)
shares <- sapply(swiss[-1], function(x) shapiro.test(x))
swi <- swiss[-1]
swi <- swi[shares > 0.05]
swi
swi <- cbind(swiss[1], swi)

lm(swi[ ,1] ~ ., swi[-1])

smart_lm <- function(x){
  shares <- sapply(x[-1], function(y) shapiro.test(y)$p.value)
  swi <- x[-1]
  swi <- swi[shares > 0.05]
  if (ncol(swi) > 0){
    swi <- cbind(x[1], swi)
    lm(swi[ ,1] ~ ., swi[-1])$coefficients
  }
  else {
    print("There are no normal variables in the data")
  }
}

smart_lm(swiss)

test_data <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")
smart_lm(test_data)

test_data <- data.frame(x = 1:100, y = 1:100, z = 1:100)
smart_lm(test_data)
#####################################################################################
str(t.test(iris[, 1:4], mu=0))
lapply(iris[, 1:4], function(x) str(t.test(x, mu = 0)))

str(t.test(iris[,1], mu = 4))
t.test(iris[,1], mu = 4)$statistic
t.test(iris[,1], mu = 4)$parameter
t.test(iris[,1], mu = 4)$p.value

lapply(iris[, 1:4], function(x) c(t.test(iris[,1:4], mu = 4)$statistic, t.test(iris[,1:4], mu = 4)$parameter, t.test(iris[,1:4], mu = 4)$p.value))

one_sample_t <- function(test_data, general_mean){
  test_data_num <- test_data[unlist(lapply(test_data, is.numeric))] 
  return(lapply(test_data_num, function(x) c(t.test(x, mu = general_mean)$statistic, t.test(x, mu = general_mean)$parameter, t.test(x, mu = general_mean)$p.value)))
  }

general_mean <- 4
test_data <- iris[, 1:4]
test_data_num <- test_data[unlist(lapply(test_data, is.numeric))] 
test_data
test_data_num
lapply(test_data_num, function(x) c(t.test(x, mu = general_mean)$statistic, t.test(x, mu = general_mean)$parameter, t.test(x, mu = general_mean)$p.value))

one_sample_t(iris[, 1:4], 4)

test_data <- as.data.frame(list(V1 = c(28, 43, 24, 29, 26, 28, 56, 38, 41, 49), V2 = c("A", "A", "B", "A", "A", "A", "B", "A", "A", "A"), V3 = c(51, 39, 27, 48, 49, 36, 38, 21, 32, 38), V4 = c(27, 38, 33, 50, 50, 40, 25, 36, 49, 45), V5 = c(30, 32, 47, 47, 46, 40, 31, 37, 44, 37), V6 = c(34, 29, 51, 42, 30, 44, 50, 37, 54, 44), V7 = c(43, 33, 38, 42, 30, 35, 26, 40, 31, 33), V8 = c("A", "B", "B", "A", "B", "B", "B", "B", "B", "B")))
test_data <- as.data.frame(list(V1 = c(33, 46, 40, 29, 37, 37, 44, 46, 41, 29), V2 = c(47, 38, 43, 34, 45, 39, 21, 49, 31, 26), V3 = c(38, 47, 39, 47, 44, 23, 41, 31, 47, 36), V4 = c("A", "B", "A", "B", "B", "A", "A", "A", "A", "A"), V5 = c(36, 35, 56, 25, 33, 26, 35, 38, 46, 36), V6 = c(39, 47, 45, 35, 53, 35, 24, 32, 38, 42), V7 = c(43, 35, 45, 37, 36, 35, 40, 36, 31, 52), V8 = c(34, 56, 44, 31, 38, 32, 29, 25, 24, 39), V9 = c(29, 21, 45, 27, 36, 43, 44, 26, 32, 35), V10 = c("B", "A", "A", "B", "B", "A", "B", "A", "A", "B"
)))
nnn <- test_data[unlist(lapply(test_data, is.numeric))]
nnn
one_sample_t(test_data, 4)
test_data

list(V1 = c(15.48, 9, 0), V2 = c(10.36, 9, 0), V3 = c(13.06, 9, 0), V5 = c(10.38, 9, 0), V6 = c(12.27, 9, 0), V7 = c(16.46, 9, 0), V8 = c(9.29, 9, 0), V9 = c(10.23, 9, 0))

normality_tests <- lapply(iris[, 1:4], shapiro.test)
normality_tests

lapply(normality_tests, function(x) x$p.value)
normality_tests$Sepal.Width$p.value

str(normality_tests[])


get_p_value <- function(test_list){
  lapply(test_list, function(x) x$p.value)
}



library(ggplot2)
library(stats)
library(dplyr)

seq(0, 1, length.out = 11)
seq(stats::rnorm(20)) # effectively 'along'
seq(1, 9, by = 2)     # matches 'end'
seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by = 0.05)
seq(17) # same as 1:17, or even better seq_len(17)

seq(1,length(diamonds[[1]]),2)
seq(2,nrow(diamonds),2)

diamonds = tibble(diamonds)

filter(diamonds, carat > 0.3 | color == "J")
rownames(diamonds)

filter(diamonds, as.integer(rownames(diamonds))%%2!=0)
slice(diamonds, seq(1, nrow(diamonds), 2))

select
slice
filter
arrange
mutate
rename

my_df <- mtcars %>% 
  select(mpg, hp, am, vs) %>% 
  filter(mpg > 14 & hp > 100) %>% 
  arrange(-mpg) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon" = mpg, "Gross horsepower" = hp)


my_data <- data_frame(x = rnorm(10000), y = rnorm(10000), 
                      f = factor(rep(1:2, 5000)))

log_transform <- function(test_data){
  mutate_each(test_data, funs((. - max(.))/(max(.) - min(.)) + 1))
  return(mutate_each(test_data, funs(log(.))))
}

log_transform(my_data)
























