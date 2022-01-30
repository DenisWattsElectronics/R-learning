# RPF

?attitude
attitude
str(attitude)
sorted_l <- sort(attitude$learning, decreasing = T)
sorted_l
att_sorted <- head(attitude[attitude$learning >= 71, ], 5)
att_sorted
max(att_sorted$complaints + att_sorted$raises + att_sorted$advance)
att_sorted[att_sorted$complaints + att_sorted$raises + att_sorted$advance ==
           max(att_sorted$complaints + att_sorted$raises + att_sorted$advance), ]


subset(attitude, rating < 50, -rating)
attitude[attitude$rating < 50, -"rating"]
attitude[attitude$rating < 50, names(attitude) != "rating"]
attitude[rating < 50, names(attitude) != "rating"]
subset(sel = -rating, sub = rating < 50, attitude)


?quakes
str(quakes)
summary(quakes)
tail(quakes)

evian0 <- read.csv(file = "https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat2.csv")
evian1 <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
evian2 <- read.csv(file = "https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat2.csv",
                   sep = ";", quote = '.', skip = 5, header = T, comment.char = "%", na.strings = "Don't remember")

evian2$Observer <- as.factor("CL")
head(evian0)
head(evian1)
head(evian2)

any(!complete.cases(evian1))
any(!complete.cases(evian2))

evian <- rbind(evian1, evian2)

str(evian)
str(evian1)
str(evian2)

summary(evian)
summary(evian1)
summary(evian2)

tail(evian)
evian

coverage_variables <- names(evian)[-(1:4)][c(T,F)]
evian$total_coverage <- rowSums(evian[, coverage_variables])
summary(evian$total_coverage)

my_var <- "Site"; evian$my_var
my_var <- "Site"; evian[[my_var]]
my_var


height_variables <- names(evian1)[-(1:4)][c(F,T)]
height_variables


max(evian1[[height_variables[1]]])
max(evian1[[height_variables[2]]])
max(evian1[[height_variables[3]]])
max(evian1[[height_variables[4]]])
max(evian1[[height_variables[5]]])
max(evian1[[height_variables[6]]])

height_variables <- names(evian1)[-(1:5)][c(T, F)]
height_variables
sort(sapply(evian1[height_variables], max), decreasing = T)








