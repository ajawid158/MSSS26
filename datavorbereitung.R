###Writing a function in R
x=c(2,3,4,5,10,1)
mean(x)

#write a function that finds the sum and product of two numbers, x, y

f1=function(x,y){
  s=x+y
  p=x*y
  
  return(c(s,p))
}
f1(2,10)

#Write a function that checks whether a quadratic equation has 
# real solution(s). ax^2 + bx + c=0
#   delta= b^2 – 4ac
#  if delta >= 0 then real solutions 
#  (delta<0) no real solutions
#Data preprocessing / Datenvorbereitung
setwd("C:/Users/ajawi/OneDrive/Desktop/MLSS26")

install.packages("tidyverse")
#install.packages("mice")
library("tidyverse")
library("mice")

#Create a sample dataset

df1=data.frame(
  Gender=c("Female", "f","Male","Child","m", "Female", "Female"), 
  Age=c(21, NA, 60, 22, -30, 60, 60),
  Height =c(190, 185, "6ft", 191, 189, "6ft", "6ft"),
  Mart.S=c(NA, NA, NA, "Single", NA, NA, NA)
)

view(df1)
names(df1)
df1

#Gender
table(df1$Gender)

#Wrong value 
df1$Gender=gsub("Child", NA, df1$Gender)

table(df1$Gender)
df1

#Inconsistencies
df1$Gender=gsub("Female", "f", df1$Gender)
df1$Gender=gsub("Male", "m", df1$Gender)
table(df1$Gender)

df1

##Invalid values
class(df1$Height)
table(df1$Height)

df1$Height=gsub("6ft", "183", df1$Height)
class(df1$Height)
summary(df1$Height)

df1$Height=as.numeric(df1$Height)
class(df1$Height)
summary(df1$Height)

df1
#Duplikat
df1.dup=duplicated(df1)
df1.dup

#Remove the duplicated row(s)
df1.1=unique(df1)
df1.1


##Missing Values
x=read.csv("datana.csv")
View(x)
names(x)
head(x)
str(x)

names(x)
sum(is.na(x$age))
sum(is.na(x$gender))

colSums(is.na(x))
md.pattern(x)

##Calculation under NA value
mean(x$age, na.rm=T)
sd(x$income, na.rm=T)

#Test if Na are ocurring Randomly
dm.na=is.na(x$age)
table(dm.na)
View(dm.na)

#TEst if the Way data is missing on Age is related to Gender 

chisq.test(x$gender, dm.na)
#p-value = 0.7162 we Do Not Reject The H0
#H0:Gender and dm.na are indpendent
#data is missing randomly

#Impute it 
x.fill=mice(x)
y=complete(x.fill)

colSums(is.na(y))

##Outliers
names(y)

#do we have outliers
boxplot(y$income, horizontal = T)
boxplot.stats(y$income)

#Remove the outliers 
y.1=y[y$income<1000000, ]
boxplot(y.1$income, horizontal = T)

dim(y)
dim(y.1)


