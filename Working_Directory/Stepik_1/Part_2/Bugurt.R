setwd("/home/dwe/R/Working_Directory/Stepik_1/Part_2")
df <- read.csv("grants.csv")
str(df)
#df$status <- as.factor(df$status)
levels(df$status)
#levels(df$status) <- c('Not funded', 'Funded')
df$status <- factor(df$status, levels = c(0, 1), labels = c("Not Funded", "Funded"))

# 1d Table 
t1 <- table(df$status)
t1
dim(t1)

# 2d Table
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)

dim(t2)

prop.table(t2)

prop.table(t2, 1)
prop.table(t2, 2)
prop.table(t2, 2)*100

# 3d Table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
prop.table(t3, 2)
t3

dim(t3)

###
str(HairEyeColor)
dimnames(HairEyeColor)
HairEyeColor[ , ,'Male']
prop.table(HairEyeColor[ ,, 'Male'], 2)
prop.table(HairEyeColor[ , , 'Male'], 2)['Red', 'Blue']
sum(HairEyeColor[ , "Green",'Female'])
###

# plots

barplot(t1)
barplot(t2)
barplot(t3)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = T)
mosaicplot(t2)

###
library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
str(mydata)
obj <- ggplot(data = subset(mydata, Sex == 'Female'), aes(x = Hair, y = Freq)) + 
  geom_bar(stat="identity", aes(fill = Eye), position = position_dodge()) + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj
###

# Binomial Test
binom.test(x = 9, n = 20, p = 0.5)
binom.test(t1, conf.level = 0.95)


# Chi-Square
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs


t2
chisq.test(t2)



# Fisher's Exact Test

fisher.test(t2)


###
HairEyeColor
twsh <- HairEyeColor['Brown', ,'Female']
twsh
chisq.test(twsh)

unlist(cut ~ color, diamonds, chisq.test())
chisq.test()
main_stat <- unlist()

main_stat <- unlist(chisq.test(xtabs(~cut+color,data = diamonds)))
main_stat

main_stat <- chisq.test(diamonds$cut, diamonds$color)$statistic
str(diamonds)
my_diamonds <- diamonds

my_diamonds$factor_price <- ifelse(diamonds$price >= mean(diamonds$price), 1, 0)
str(my_diamonds)
Ololo <- my_diamonds$factor_price
my_diamonds$price 
my_diamonds$factor_carat <- ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)
my_diamonds$factor_carat
chi <- chisq.test(my_diamonds$factor_carat, my_diamonds$factor_price)
main_stat <- chi['statistic']
main_stat

fisher_test <- fisher.test(mtcars$am, mtcars$vs)$p.value
str(fisher_test)
fisher_test$p.value


# T_Student

?iris

df <- iris
View(df)

str(df)
df1 <- subset(df, df$Species != "setosa")
str(df1)
table(df1$Species)
hist(df1$Sepal.Length)  
library(ggplot2)
ggplot(df1, aes(x = Sepal.Length)) + 
  geom_histogram(fill = "white", col = "black", binwidth = 0.4) + 
  facet_grid(Species ~ .)
ggplot(df1, aes(Sepal.Length, fill = Species, alpha = 0.5)) + 
  geom_density()
ggplot(df1, aes(Species, Sepal.Length)) + 
  geom_boxplot()

shapiro.test(df1$Sepal.Length)
shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

bartlett.test(Sepal.Length ~ Species, df1)

t.test(Sepal.Length ~ Species, df1)
ttt <- t.test(Sepal.Length ~ Species, df1)
str(ttt)
ttt$p.value

t.test(df$Sepal.Length, mu = 6, conf.level = 0.1)
t.test(df$Sepal.Length, mu = 6, conf.level = 0.99)

t.test(df1$Sepal.Length, df1$Petal.Length, paired = T)

#####

?ToothGrowth

chisq.test(twsh)


mean(ToothGrowth$len[ToothGrowth$supp == 'OJ' & ToothGrowth$dose == 0.5])
mean(ToothGrowth$len[ToothGrowth$supp == 'VC' & ToothGrowth$dose == 2])

chisq.test(mean(ToothGrowth$len[ToothGrowth$supp == 'OJ' & ToothGrowth$dose == 0.5]), +
         mean(ToothGrowth$len[ToothGrowth$supp == 'VC' & ToothGrowth$dose == 2]))

chisq.test(10, 11)

?chisq.test


t.test(ToothGrowth$len[ToothGrowth$supp == 'OJ' & ToothGrowth$dose == 0.5], +
             ToothGrowth$len[ToothGrowth$supp == 'VC' & ToothGrowth$dose == 2])["statistic"]

t_stat <- t.test(ToothGrowth$len[ToothGrowth$supp == 'OJ' & ToothGrowth$dose == 0.5], +
                       ToothGrowth$len[ToothGrowth$supp == 'VC' & ToothGrowth$dose == 2])["statistic"]
t_stat
   
lek <- read.csv(file = "lekarstva.csv")

t.test(lek$Pressure_before, lek$Pressure_after, paired = F)["p.value"]
t.test(lek$Pressure_before, lek$Pressure_after, paired = T)["p.value"]

### 
df  <- iris
df1  <- subset(df, Species != "setosa")
ggplot(df1, aes(Species, Sepal.Length)) + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", 
                 width=0.1) + 
  stat_summary(fun = mean, geom = "point", size = 2)

ggplot(df1, aes(Species, Sepal.Length)) + 
  stat_summary(fun.data=mean_cl_normal, geom="pointrange" )

t.test(df1$Sepal.Length)$p.value
wilcox.test(Sepal.Length ~ Species, df1)$p.value

ggplot(df1, aes(Species, Petal.Length)) + 
  geom_boxplot()


wilcox.test(df1$Sepal.Length, mu=6, paired = F)
wilcox.test(df1$Sepal.Length, df1$Sepal.Width, paired = T)

#####
ttt <- read.table("dataset_11504_15.txt")
str(ttt)
bartlett.test(ttt$V1, ttt$V2)
t.test(V1~V2, ttt, var.equal = T)$p.value
wilcox.test(V1~V2, ttt)$p.value
###

#####
df1 <- npk
summary(df1)
t <- aov(yield ~ P + N + P:N, data = df1)
summary(t)

summary(aov(yield ~ N + P + K, data = df1))

df1 <-iris

t <- aov(Sepal.Width ~ Species, df1)
TukeyHSD(t)
###

md2 <- read.csv("therapy_data.csv")
summary(md2)
str(md2)

t <- aov(well_being ~ therapy, data = md2)
summary(t)
t <- aov(well_being ~ therapy + Error(subject/therapy), data = md2)
summary(t)
t <- aov(well_being ~ therapy + price, data = md2)
summary(t)
t <- aov(well_being ~ therapy * price, data = md2)
summary(t)
t <- aov(well_being ~ therapy * price + Error(subject/(therapy*price)), data = md2)
summary(t)

###
pil1 <- read.csv("Pillulkin.csv")
summary(pil1)
str(pil1)
pil1
pil1$patient <- as.factor(pil1$patient)

t <- aov(temperature ~ pill + Error(patient/pill), data = pil1)
summary(t)

t <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = pil1)
summary(t)


library(ggplot2)
ggplot(pil1, aes(x = pill, y = temperature)) + 
  geom_boxplot()


str(ToothGrowth)
library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj
###


my_calc <- function(x, y){
  s <- x + y
  return(s)
}
my_calc(2, 3)

my_calc <- function(x, y){
  s <- x + y
  d <- x - y
  return(c(s, d))
}
my_calc(2, 3)[1:2]


distr1 <- rnorm(100)
distr1[1:30] <- NA
distr1[is.na(distr1)] <- mean(distr1, na.rm = T)

my_na_rm <- function(x){
  x[is.na(x)] <- mean(x, na.rm = T)
  return(x)
}
hist(distr1)
distr1 <- my_na_rm(distr1)


distr1  <- rnorm(100)
distr1[1:30]  <- NA
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T)

my_na_rm  <- function(x){
  if (is.numeric(x)){
    stat_test  <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

d1  <- rnorm(2000)
d2  <- runif(2000)

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)

#####
my_vector <- c(1, 2, 3, NA, NA)
which(is.na(my_vector))
NA.position <- function(vec){
  return(which(is.na(vec)))
}

NA.counter <- function(vec){
  return(length((which(is.na(vec)))))
}
NA.counter(my_vector)



dir(pattern = "*.csv", path = "Grants data/")
grants <- data.frame()

for (i in dir(pattern = "*.csv", path = "Grants data/")) {
  print(i)
  temp_df <- read.csv( paste0("Grants data/", i) )
  grants <- rbind(temp_df, grants)
}

#####
vec <- c(1, 2, 3, 4, 5, -5, NA)
sum(vec[vec>0 & !is.na(vec)])
sum(vec[vec>0], na.rm = T)
###
library(ggplot2)
df  <- mtcars

cor.test(x = df$mpg, y = df$hp)
fit  <- cor.test(x = df$mpg, y = df$hp)

cor.test(~ mpg + hp, df)

str(fit)

fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 2.5)

#####
#1
corr.calc <- function(x){
  return(c(cor.test(x[[1]], x[[2]])$estimate, cor.test(x[[1]], x[[2]])$p.value))
}

banana <- corr.calc( mtcars[, c(1,5)] )
banana$p.value
banana$estimate
banana

#2
install.packages("gdata")
library(gdata)
step6 <-  read.table("step6.csv",  header=TRUE, sep=',' )
step6
str(step6)

clear_data <- step6[,sapply(step6, is.numeric)]
clear_data
iris


corrs <- vector()
ncol(clear_data)

for (i in 1:(ncol(clear_data)-1)){
  for (j in (i+1):ncol(clear_data)){
#    p <- c(i, j)
#    print(p)
    corrs <- abs(c(corrs, cor.test(clear_data[, i], clear_data[, j])$estimate))
    return(corrs)
  }
}

corrs
max(corrs)

clear_data <- step6[,sapply(step6, is.numeric)]

filtered.cor <- function(x){
  x <- x[,sapply(x, is.numeric)]
  corrs <- vector()
  abscorrs <- vector()
  for (i in 1:(ncol(x)-1)){
    for (j in (i+1):ncol(x)){
      corrs <- c(corrs, cor.test(x[, i], x[, j])$estimate)
      abscorrs <- c(abscorrs, abs(cor.test(x[, i], x[, j])$estimate))
    }
  }
  return(corrs[which.max(abscorrs)])
}


test_data <- as.data.frame(list(V4 = c(0.4, 0, -1.5, -1.8, -2.1, -1.7, -2.2, -1.6), V3 = c(1.4, -0.4, 1, -1.1, 0.2, 1.6, 0, 0.6), V6 = c("m", "m", "m", "m", "m", "m", "m", "m"), V1 = c(-0.4, 0.5, -1, 0.5, 0.4, -1.1, 1.8, -1), V8 = c("g", "g", "g", "g", "g", "g", "g", "g"), V5 = c(-1.7, 0, 0.2, -1.9, -0.1, 1.3, 2.2, -0.4), V7 = c("m", "m", "m", "m", "m", "m", "m", "m"), V2 = c(0.4, -0.3, -0.5, -0.2, -0.1, -1.1, 0.2, -1.1)))

ans <- filtered.cor(clear_data)
ans <- filtered.cor(iris)
ans <- filtered.cor(test_data)
ans

library(psych)
filtered.cor <- function(x){
  y <- cor(x[sapply(x, is.numeric)])
  diag(y) <- 0
  return(y[which.max(abs(y))]) 
}



test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
test_data <- as.data.frame(list(col1 = c(1.6, 1.69, -0.53, 1.41, 1.39, -1.51, 0.48, -1.34, -0.98, -1.88, -1.67, -1.35, -1.44, 0.88, -1.33, 1.78, -1.51, 0.59, 1.48, -0.94, 1.5, -0.92, -0.21, -0.78, -1.24, 1.58, 1.19, -1.6, -1.57, -1.48), col2 = c(-1.67, -1.18, 0.23, -0.75, -0.69, -0.14, -1.21, 0.24, -0.37, -0.56, -1.91, -1.49, 1.28, -0.84, -0.47, 1.81, 0.75, -0.09, 0, -0.74, 0.61, 0.82, -1.32, -0.04, -2.44, -1.39, 1.06, -0.42, -0.94, -0.64)))

test_data

shapiro.test(test_data[,1])$p.value
shapiro.test(test_data[,2])$p.value

smart_cor <- function(x){
  if (shapiro.test(x[,1])$p.value < 0.05 | shapiro.test(x[,2])$p.value < 0.05){
    return(cor.test(x[,1], x[,2], method = 'spearman')$estimate)
  }
  else {
    return(cor.test(x[,1], x[,2])$estimate)
  }
}

smart_cor(test_data)

smart_cor <- function(x){
  ifelse(shapiro.test(x[[1]])$p.value<0.05|shapiro.test(x[[2]])$p.value<0.05, 
         cor(x,method = "spearman")[1,2], cor(x, method = "pearson")[1,2])
}

smart_cor <- function(x){
  cor(x[[1]], x[[2]], method = if (shapiro.test(x[[1]])$p < 0.05 | shapiro.test(x[[2]])$p < 0.05) "spearman" else "pearson")
}

###
library(ggplot2)

df  <- mtcars
df_numeric  <- df[,c(1,3:7)]

fit  <- lm(mpg ~ hp, df)
summary(fit)

ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values )

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg  <- predict(fit, new_hp)

predict(fit, new_hp)


##################################


library(psych)

my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ hp, my_df)

summary(fit)

ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values )

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp
new_hp$mpg  <- predict(fit, new_hp)
new_hp
predict(fit, new_hp)

my_df <- mtcars
fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

ggplot(my_df, aes(cyl, mpg))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=15, face='bold'))

my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

aggregate(mpg ~ cyl, my_df, mean)

#####
ttt <- read.table("dataset_11508_12.txt", sep = " ")
fit <- lm(V1 ~ V2, ttt)
summary(fit)
fit$coefficients


fit <- lm(mpg ~ disp + wt, mtcars)
fit$coefficients # коэффициенты модели

str(diamonds)


fit_coef <- lm(price ~ depth, diamonds, subset = (carat==0.46 & cut=="Ideal"))$coefficients
fit_coef

df_y <- iris[,c(1,4)] 
df_n <- iris[,1:2]

ifelse(diamonds$price >= mean(diamonds$price), 1, 0)


regr.calc <- function(df){
  if (summary(lm(df[,1] ~ df[,2], df))$coefficients[2,4] < 0.05){
    df$fit <- lm(df[,1] ~ df[,2], df)$fitted.values
    return(df)
  }
  else{
    print("There is no sense in prediction")
  }
}  

df_yy <- df_y
df_yy$fit <- 1
df_yy

regr.calc(df_n)

ttt <- lm(df_y[,1] ~ df_y[,2], df_y)
ttt$fitted.values

vvv <- regr.calc(df_y)
vvv


df <- mtcars
my_df <- mtcars
fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

ggplot(my_df, aes(cyl, mpg))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=15, face='bold'))

ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)



ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species))+
  geom_point() + 
  geom_smooth(method = "lm")

###

?swiss
str(swiss)

hist(swiss$Fertility, col = 'red')

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)


fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)


confint(fit2)

###
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
test_data <- as.data.frame(list(x_1 = c(10, 8, 11, 8, 10, 9, 9, 11, 11, 7),
                                x_2 = c(33, 43, 40, 27, 30, 31, 26, 37, 39, 26), y = c(13, 12, 17, 18, 10, NA, NA, 5, 10, 14)))
test_data
str(test_data)

fit_test <- lm(y ~ x_1*x_2, test_data)
summary(fit_test)
confint(fit_test)

ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)

fill_na <- function(x){
  fit_test <- lm(y ~ x_1 + x_2, x)
  y_full <- predict(fit_test, x)
  x$y_full <- x$y
  x$y_full[is.na(x$y)] <- y_full[is.na(x$y)]
  return(x)
}

fill_na <- function(x)
  data.frame(x, y_full = ifelse(is.na(x$y), predict(lm(y ~ x_1 + x_2, x), x), x$y))

fill_na(test_data)


my_df = iris[,c(1,4)]
df

df <- mtcars[,c(1, 3, 4, 5, 6)]
str(df)
fit_test <- lm(wt ~ mpg + disp + hp, df)
summary(fit_test)

?attitude
str(attitude)
summary(lm(rating ~ complaints * critical, attitude))
confint(lm(rating ~ complaints * critical, attitude))

###
# categorical predictors
str(swiss)
swiss
hist(swiss$Catholic, col = 'red')

swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ Examination*religious, data = swiss)
fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)

# plots

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')

#####
dfmtc <- mtcars
dfmtc$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
dfmtc$wt_centered <- mtcars$wt - mean(mtcars$wt)

fit <- lm(mpg ~ wt_centered*am, dfmtc)
summary(fit)

ggplot(dfmtc, aes(x = wt, y = mpg, col = am)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')

ggplot(dfmtc, aes(x = wt, y = mpg)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')

###

rm(swiss)
str(swiss)

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)


# model comparison

rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

fit_reduced3 <-lm(Fertility ~ . -Agriculture -Education, swiss)

# model selection

optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)

#####
model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
scope = list(lower = model_null, upper = model_full)

ideal_model <- step(model_full, direction = 'backward', scope = scope)
summary(ideal_model)

anova(model_full, ideal_model)

###

pairs(swiss)
ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point()


# Outliers

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

#####

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, + 0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
shapiro.test(my_vector)
shapiro.test(1/my_vector)
shapiro.test(sqrt(my_vector))
shapiro.test(log(my_vector))
































