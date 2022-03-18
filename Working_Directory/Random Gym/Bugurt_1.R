# Here we go again! 
print("Here we go again!")

##### cheatsheet 
### common dataframes
st() summary() head() tail()
### descriptive statistics
median() mean() sd() range() describe(from psych)
### plots
plot() hist() boxplot() ggplot(from ggplot2)
#####

str(iris)
summary(iris)
head(iris)
summary(iris[iris$Species == 'setosa',])
summary(iris[iris$Species == 'versicolor',])
summary(iris[iris$Species == 'virginica',])

mean(iris$Sepal.Length[iris$Species == 'versicolor'])
sd(iris$Sepal.Length[iris$Species == 'versicolor'])

library(psych)
?describe
describe(iris)

hist(iris$Sepal.Length[iris$Species == 'versicolor'], breaks  = 4)
plot(iris$Sepal.Length[iris$Species == 'versicolor'])

library(ggplot2)
ggplot(data = iris, aes(x = iris$Petal.Length, y = iris$Species)) + 
  geom_boxplot(aes(fill = iris$Species), width = 0.8) + theme_bw()




