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






