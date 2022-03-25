df <- iris

str(iris)
?iris
summary(iris)
#par(mfrow=c(1.1))
plot(iris$Sepal.Length)
plot(iris$Sepal.Width)
plot(iris$Petal.Length)
plot(iris$Petal.Width)

library(dplyr)
library(ggplot2)
iris %>% 
  select(Species, Petal.Length, Sepal.Length) %>% 
  ggplot(aes(Petal.Length, Sepal.Length, colour = Species, shape = Species)) + 
  geom_point()
iris %>% 
  select(Species, Petal.Width, Sepal.Width) %>% 
  ggplot(aes(Petal.Width, Sepal.Width, colour = Species, shape = Species)) + 
  geom_point()


### Plots
pairs(iris)

par(
  mfrow=c(2,2),
  mar=c(4,4,1,0)
)
hist(df$Petal.Length[df$Species == 'virginica'], breaks = 10, xlab = "PL", main ="Histogram of Iris", 
     col = "white", cex.lab = 1.2, cex.axis = 1.2)
hist(df$Petal.Width[df$Species == 'virginica'], breaks = 10, xlab = "PL", main ="Histogram of Iris", 
     col = "white", cex.lab = 1.2, cex.axis = 1.2)
hist(df$Sepal.Length[df$Species == 'virginica'], breaks = 10, xlab = "PL", main ="Histogram of Iris", 
     col = "white", cex.lab = 1.2, cex.axis = 1.2)
hist(df$Sepal.Width[df$Species == 'virginica'], breaks = 10, xlab = "PL", main ="Histogram of Iris", 
     col = "white", cex.lab = 1.2, cex.axis = 1.2)
par(mfrow=c(1,1))

plot(density(df$Petal.Length[df$Species == 'virginica']), xlab = "PL", main ="Density of Iris", 
     col = "white", cex.lab = 1.3, cex.axis = 1.3)
plot(density(df$Petal.Width[df$Species == 'virginica']), xlab = "PL", main ="Density of Iris", 
     col = "white", cex.lab = 1.3, cex.axis = 1.3)
plot(density(df$Sepal.Length[df$Species == 'virginica']), xlab = "PL", main ="Density of Iris", 
     col = "white", cex.lab = 1.3, cex.axis = 1.3)
plot(density(df$Sepal.Width[df$Species == 'virginica']), xlab = "PL", main ="Density of Iris", 
     col = "white", cex.lab = 1.3, cex.axis = 1.3)


boxplot(Petal.Length ~ Species, iris, ylab = "MPG", main ="MPG and AM", 
        col = "white", cex.lab = 1.3, cex.axis = 1.3)
plot(~Petal.Length + Species, iris) 
plot(~Species + Sepal.Length, iris) 
plot(~Species + Sepal.Width, iris) 
plot(~Species + Petal.Length, iris) 
plot(~Species + Petal.Width, iris) 

### Normality check
shapiro.test(iris$Sepal.Length)
shapiro.test(iris$Sepal.Width[iris$Species == 'setosa'])
shapiro.test(iris$Petal.Length)
shapiro.test(iris$Petal.Width)

# Histograms for all W/L ans species all

iris_hist_all <- function() {
  par(
    mfrow=c(4,3),
    mar=c(4,4,1,0)
  )
  hist(df$Sepal.Length[df$Species == 'setosa'], breaks = 10, xlab = "PL", main ="Sepal.Length setosa", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Sepal.Length[df$Species == 'versicolor'], breaks = 10, xlab = "PL", main ="Sepal.Length versicolor", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Sepal.Length[df$Species == 'virginica'], breaks = 10, xlab = "PL", main ="Sepal.Length virginica", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Petal.Length[df$Species == 'setosa'], breaks = 10, xlab = "PL", main ="Petal.Length setosa", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Petal.Length[df$Species == 'versicolor'], breaks = 10, xlab = "PL", main ="Petal.Length versicolor", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Petal.Length[df$Species == 'virginica'], breaks = 10, xlab = "PL", main ="Petal.Length virginica", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Sepal.Width[df$Species == 'setosa'], breaks = 10, xlab = "PL", main ="Sepal.Width setosa", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Sepal.Width[df$Species == 'versicolor'], breaks = 10, xlab = "PL", main ="Sepal.Width versicolor", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Sepal.Width[df$Species == 'virginica'], breaks = 10, xlab = "PL", main ="Sepal.Width virginica", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Petal.Width[df$Species == 'setosa'], breaks = 10, xlab = "PL", main ="Petal.Width setosa", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Petal.Width[df$Species == 'versicolor'], breaks = 10, xlab = "PL", main ="Petal.Width versicolor", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  hist(df$Petal.Width[df$Species == 'virginica'], breaks = 10, xlab = "PL", main ="Petal.Width virginica", 
       col = "white", cex.lab = 1.2, cex.axis = 1.2)
  par(mfrow=c(1,1))
}

iris_hist_all()

# Normality check all

iris_norma_check_all <- function() {
  rbind(
    c("SL, setosa", shapiro.test(iris$Sepal.Length[iris$Species == 'setosa'])$p.value),
    c("SL, versicolor", shapiro.test(iris$Sepal.Length[iris$Species == 'versicolor'])$p.value),
    c("SL, virginica", shapiro.test(iris$Sepal.Length[iris$Species == 'virginica'])$p.value),
    
    c("PL, setosa", shapiro.test(iris$Petal.Length[iris$Species == 'setosa'])$p.value),
    c("PL, versicolor", shapiro.test(iris$Petal.Length[iris$Species == 'versicolor'])$p.value),
    c("PL, virginica", shapiro.test(iris$Petal.Length[iris$Species == 'virginica'])$p.value),
    
    c("SW, setosa", shapiro.test(iris$Sepal.Width[iris$Species == 'setosa'])$p.value),
    c("SW, versicolor", shapiro.test(iris$Sepal.Width[iris$Species == 'versicolor'])$p.value),
    c("SW, virginica", shapiro.test(iris$Sepal.Width[iris$Species == 'virginica'])$p.value),
    
    c("PW, setosa", shapiro.test(iris$Petal.Width[iris$Species == 'setosa'])$p.value),
    c("PW, versicolor", shapiro.test(iris$Petal.Width[iris$Species == 'versicolor'])$p.value),
    c("PW, virginica", shapiro.test(iris$Petal.Width[iris$Species == 'virginica'])$p.value) )
}

iris_norma_check_all()

# t-test, but in this case it isn't useful due to three levels in Species 
df1 <- subset(df, Species != "setosa")
t.test(Sepal.Length ~ Species, df1)
















