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


library(stringr)

hamlet <- "To be, or not to be: that is the question:
Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune,
Or to take arms against a sea of troubles,
And by opposing end them?"

hamlet <- str_replace_all(hamlet, "[:punct:]", "")
hamlet <- tolower(unlist(str_split(hamlet, "[:space:]")))
hamlet

sum(str_count(string = hamlet, pattern = )) 
5 + 1 + 2
sum(hamlet=="to")
sum(grepl("[fqw]", hamlet))
sum(grepl("b.", hamlet))
sum(nchar(hamlet)==7)

sum(str_detect(hamlet,"to"))
sum(str_detect(hamlet,"[fqw]"))
sum(str_detect(hamlet,"b."))
sum(str_length(hamlet)==7)

?quakes
str(quakes)
summary(quakes$mag)
cut(quakes$mag, breaks = 10, right = T)
table(cut(quakes$mag, breaks = c(4, 4.5, 5, 5.5, 6, 6.5), left = T))
table(quakes$mag)

sort(table(cut(quakes$mag, seq(4, 6.5, by = 0.5), r = F)), d = T)
cat(names(sort(table(cut(quakes$mag, seq(4, 6.5, by = 0.5), right = F)), decreasing = T)))
sort(table(cut(quakes$mag, seq(4, 6.5, 0.5), right = F)), decreasing = T)
  


#install.packages('stringr')
library(stringr)
df <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
#отбираем только те столбцы, у которых встречается P
df2 <- df[names(df)[str_detect(names(df),"^P")]]
#добавляем суммы по строчкам
df2$total_coverage <- apply(df2,1,sum)
#удаляем цифры из значений и даем новой переменной
df2$site_name <- str_replace(df$Site, "[:digit:]+", "")
#считаем среднее для видов
df2$site_name <- factor(df2$site_name)
sort(tapply(df2$total_coverage, df2$site_name, mean))

x <- read.csv("https://raw.githubusercontent.com/tonytonov/Rcourse/master/R%20programming/avianHabitat.csv")
x$total <- rowSums(x[grepl("^P.", names(x))])
x$sname <- factor(gsub("[0-9]", "", as.character(x$Site)))
which.min(tapply(x$total, x$sname, mean))



sapply (names(df)[str_detect(names(df), "Ht")], function(x) tapply(x, df$Observer, max))
tapply(Ht, Observer, max)


sapply(avian[, grepl("Ht", names(avian))], function(name) {tapply(name, avian$Observer, max)})


height_variables <- names(avian)[str_detect(names(avian), "Ht$")]
sapply(height_variables, function(x) names(which.max(tapply(avian[[x]], avian$Observer, max))))

"%+%" <- function(x, y) {
  l <- c(length(x), length(y))
  length(x) <- length(y) <- min(l)
  s <- x + y
  return(s[1:max(l)])
  }
1:5 %+% 1:2











# Random walk with absorption
simulate_walk <- function(maxr = 6, n_max = 100, p = 1e-2) {
  current_position <- c(0, 0)
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(list(status = "Absorbed"))
    current_position <- c(current_position[1] + rnorm(1), current_position[2] + rnorm(1))
    if (sqrt(current_position[1]^2 + current_position[2]^2) > maxr) return(list(status = "Breach"))
  }
  return(list(status = "Max steps reached", 
              position = current_position,
              steps = n_max))
}

# Simulate results
simulate_walk()
result <- replicate(1000000, simulate_walk(), simplify = FALSE)
result <- data.frame(
  status = sapply(result, function(x) x$status)
)


head(result)

# Inspect results
tapply(result$status)
tapply(result$steps, result$status, mean)

summary(result)


funs <- c("print","summary","plot")
meths <- lapply(funs, methods)
grepl("matrix", meths)
grepl("function", meths)
grepl("default", meths)



