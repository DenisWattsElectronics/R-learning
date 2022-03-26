# Here we go again! 
print("Here we go again!")

##### cheatsheet 
### common dataframes
st() summary() head() tail()
### descriptive statistics
median() mean() sd() range() describe(from psych)
### plots
plot() hist() boxplot() ggplot(from ggplot2)
### statistic analysis 
cor.test() - correlation between two or more numeric vectors
lm() - linear corr, use summary(lm)
chisq.test() - both variables are nominative/categorial
fisher.test() - the same as chisq but for less num of experiments
glm() - logistic regression
t.test() - logistic, but no predit
aov() - dispersion analysis, dependent var is categorial
anova() di, second step
### diagnostics
pairs()
shapiro.test(lm1$residuals) - test of normality
homoscedacity and other must to be remembered!
bartlett.test() - check dispersions equality 
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

###---
df  <- mtcars

cor.test(x = df$mpg, y = df$hp)
fit  <- cor.test(x = df$mpg, y = df$hp)

cor.test(~ mpg + hp, df)

str(fit)

fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)+
  facet_grid(. ~ am)


df  <- mtcars
df_numeric  <- df[, c(1,3:7)]

pairs(df_numeric)

cor(df_numeric)

fit  <- corr.test(df_numeric)
fit$r
fit$p
fit$adjust

###---
df  <- mtcars
str(df)
df_numeric  <- df[,c(1,3:7)]
str(df_numeric)
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

#---
my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)
summary(fit)

###---
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

###---
require(graphics)

t.test(1:10, y = c(7:20))      # P = .00001855
t.test(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore

## Classical example: Student's sleep data
plot(extra ~ group, data = sleep)
## Traditional interface
with(sleep, t.test(extra[group == 1], extra[group == 2]))
## Formula interface
t.test(extra ~ group, data = sleep)

###---
## From Venables and Ripley (2002) p.165.
## Set orthogonal contrasts.
op <- options(contrasts = c("contr.helmert", "contr.poly"))
( npk.aov <- aov(yield ~ block + N*P*K, npk) )
summary(npk.aov)
coefficients(npk.aov)

## to show the effects of re-ordering terms contrast the two fits
aov(yield ~ block + N * P + K, npk)
aov(terms(yield ~ block + N * P + K, keep.order = TRUE), npk)


## as a test, not particularly sensible statistically
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
npk.aovE
## IGNORE_RDIFF_BEGIN
summary(npk.aovE)
## IGNORE_RDIFF_END
options(op)  # reset to previous

#-
fit <- aov(sr ~ ., data = LifeCycleSavings)  ## can also use `lm`
fit
z <- anova(fit)
z
