my_var1 <-  42 
my_var2 <- 35.25
my_var1 + my_var1
my_var3 = my_var1^2 + my_var2^2
my_var3

my_new_var <- my_var3 == 200
my_new_var 

my_vector1 <- 1:67
my_vector2 <- c(-32, 45, 67, 12.78, 129, 0, -65)

v0 <- c("A", "B", "C", "D", "E")
v1 <- c(1, 2, 3, 4, 5)
v2 <- c(T, T, T, F, F)
data <- list(v0, v1, v2)

df <- data.frame(AAA = v0, BBB = v1, CCC = v2)

typeof(data)


?read.table
my_data <- read.csv('https://stepik.org/media/attachments/lesson/11481/evals.csv' )
my_data = read.csv('evals.csv')
head(my_data)
View(my_data)
str(my_data)
summary(my_data)
names(my_d)

my_data$score
mean(my_data$score)
nrow(my_data)
ncol(my_data)

my_data[1,1]
my_data[c(1, 100, 200), 5]
my_data[101:200, 1]
my_data[5,]

my_data[,2:5]
head(my_data[,2:5])

my_data$gender
my_data$gender == 'female'
mean(my_data$score[my_data$gender == 'female'])
mean(my_data$score[my_data$gender == 'male'])

my_data[my_data$gender == 'female',1:3]
subset(my_data, gender == 'female')

# rbind cbind - bind by rows or colunms
 

mtcars <- mtcars
# even_gear <- ifelse(mtcars$gear %% 2 == 1, 0, 1)
# cbind(mtcars, even_gear)
mtcars$mpg_4 <- mtcars$mpg*(mtcars$cyl == 4)
mpg_4 <- mtcars$mpg[mtcars$cyl == 4]
mtcars
mpg_4

mini_mtcars <- subset(mtcars[c(3, 7, 10, 12, 32),])

mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)

AirPassengers$good_months <- ifelse(AirPassengers[i] >= AirPassengers[i-1])

ap <- as.vector(AirPassengers)

ap <- AirPassengers

good_months <- NA
good_months <- c()
for (i in 1:(length(AirPassengers)-1)) {
  if (AirPassengers[i+1] > AirPassengers[i]) {
    good_months <- c(good_months, AirPassengers[i+1])
  }
}
good_months

moving_average <- numeric(135)

AirPassengers[1:11]
cumsum(AirPassengers[1:11])
1402 - 112
c(cumsum(AirPassengers[1:10])[10], cumsum(AirPassengers[2:11])[10])

(cumsum(AirPassengers[1:11]) - c(rep(0, 10), cumsum(AirPassengers[1])))/10

((cumsum(AirPassengers[1:144]) - c(rep(0, 10), cumsum(AirPassengers[1:134])))/10)[10:144]


df <- mtcars
df$vs <- factor(df$vs, labels = c("V", "S"))
df
df$am <- factor(df$am, labels = c("Auto", "Manual"))
median(df$mpg)
mean(df$mpg)
sd(df$mpg)
range(df$mpg)

mean_disp <- mean(df$disp)
mean(df$mpg[df$cyl==6])
mean(df$mpg[df$cyl==6 & df$vs == "V"])
df$mpg[df$cyl==6 & df$vs == "V"]

mean(mtcars$qsec[mtcars$cyl!=3 & mtcars$mpg>20])

mean_hp_vs <- aggregate(x=df$hp, by=list(df$vs), FUN=mean)
colnames(mean_hp_vs) <- c("VS", "HP mean")
mean_hp_vs
aggregate(hp ~ vs, df, mean)
aggregate(hp ~ vs + am, df, mean)
my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)
my_stats

descriptions_stat <- aggregate(cbind(hp, disp) ~ am, df, sd)
descriptions_stat                               

library(psych)

?describe

describe(x = df)
describeBy(x = df, group = df$vs, mat = T, digits = 2, fast = T)
describeBy(df$qsec, group = list(df$vs, df$am), mat = T, fast = F)

sum(is.na(df$mpg))
any(is.na(df$mpg))
mean(df$mpg, na.rm = T)
aggregate(mpg ~am, df, sd)
describe(na.rm = )

airquality$Month

mma <- airquality
mya <- subset(airquality, (Month==7|Month==8|Month==9))
aggregate(Ozone ~ Month, mya, length)

describeBy(x = airquality$Wind, group = airquality$Month, mat = F, digits = 2, fast = F)
describeBy(x = iris, group = iris$Species, mat = F, digits = 2, fast = F)



my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
mean(my_vector, na.rm = T)
fixed_vector <- my_vector
fixed_vector[is.na(my_vector)] <- mean(my_vector, na.rm = T)
fixed_vector





hist(mtcars$mpg,breaks = 10, xlab = 'mpg', ylab = 'OLOLOLO!!!!1111')
boxplot(mpg ~ am, mtcars)

plot(mtcars$mpg, mtcars$hp)
?barplot
#barplot(mtcars, height = 1)

barplot(GNP ~ Year, data = longley)
barplot(cbind(Employed, Unemployed) ~ Year, data = longley)

library(ggplot2)

?ggplot
?aes

ggplot(mtcars, aes(x=mpg)) + geom_histogram(fill='white', col='black', binwidth = 2)
ggplot(mtcars, aes(x=mpg, fill=am)) + geom_dotplot()

ggplot(mtcars, aes(x = mpg, fill = am))+
  geom_dotplot()+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG dotplot")

#rgb(r,g,b,alpha)

#https://ggplot2.tidyverse.org/

ggplot(airquality, aes(x=as.factor(Month), y=Ozone)) + geom_boxplot()

ggplot(mtcars, aes(x=mpg, y=disp, col=hp)) + geom_point()


library(ggplot2)
ggplot(iris, aes(Sepal.Length)) + geom_histogram(fill = Species)
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(col = Species))
ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()































