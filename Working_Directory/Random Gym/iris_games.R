df <- iris

str(iris)
?iris
summary(iris)
par(mfrow=c(1.1))
plot(iris$Sepal.Length)
plot(iris$Sepal.Width)
plot(iris$Petal.Length)
plot(iris$Petal.Width)
par()

library(dplyr)
library(ggplot2)
iris %>% 
  select(Species, Petal.Length, Sepal.Length) %>% 
  ggplot(aes(Petal.Length, Sepal.Length, colour = Species, shape = Species)) + 
  geom_point()














