# library(grid)
# install.packages("xts", dependencies = TRUE)
# update.packages()

#set.seed(1337)
#x <- runif(1e6, min = -1, max = 1)
#y = NaN
# for (i in 1:length(x)) {
#   if ((x[i] > -0.2)&&(x[i] < 0.3)) {
#     y <- c(y, x[i])
#   }
# }

# sum(x >= -0.2 & x <= 0.3)
# length(x)
# length(y)


# D = 1:6
# arr = sample(D, 1)
# for (i in 1:1e6 - 1) {
#   arr <- c(arr, sample(D, 1))
# }
# sample(D, 100, replace = TRUE)

a <- c(0, 0, 0, 0, 0)

is_monotone <- function(x) {
  ifelse (all(x[-1] - x[-length(x)] >= 0) == T || all(x[-1] - x[-length(x)] <= 0) == T , print(TRUE), print(FALSE))
}

 is_monotone(a)

 # is_monotone <- function(x) {
 #   s <- x[-1] - x[-length(x)] >= 0
 #   n <- x[-1] - x[-length(x)] <= 0
 #   if (all(s) == T || all(n) == T) {
 #     print("TRUE")
 #   } else {
 #     print("FALSE")
 #   }
 # }

choose()




