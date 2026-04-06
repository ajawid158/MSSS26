##SS26 Machine Learning R Prgrogramming Intro

#1. Variables> data storage in R 

x=c(0, 1, -4,3.5, 10)

length(x)

#2. Data Types> numeric, character, logic
class(x)

g=c("A", "B", "C", "A")
class(g)

L=c(T, F, T, F, T,F)
class(L)

#3. Data Structures in R: Vector, Matrix, dataframe
#Consider ALL 5 employees of a Bäckerei. And collect data on their
#Gender, Salary(M), and Working Hrs

#Vector
Gender=c("f", "m", "f", "f", "f")
Salary= c(2,2.5, 3, 2.8, 1.8)
Working.Hrs = c(7, 8, 9, 7, 5)

#Matrix 
m1=matrix(Salary, nrow = 5)
m1
m2=matrix(Gender, nrow = 5)
m2
dim(m1)
m3=cbind(m2,m1)
m3
str(m3)

#Dataframe
dt1=data.frame(GND=Gender,
               SLR=Salary, 
               WRK=Working.Hrs)
dt1

head(dt1, 3)
tail(dt1, 2)
View(dt1)
colnames(dt1)=c("Gender", "Salary", "WRK")
head(dt1)
dim(dt1)

#(), [], {}
dt1[1,3]
dt1[5,]
dt1[,2]

#3 Basic principles in R Programming
#1. Sqeunncing  the codes are executed line by line Top - > Bottom
#2. Selection, if else

t=-2

if (t>0){
  print("t is positive")
} else {
  print("t is non positive")
}

#3. Loop for 

for (i in 1:8){
  print(i*i)
}

#take the Gender and transform to a 0/1 variable s.t female:=0, and male:=1



Gender
new.Gender=c()

for (i in 1:5) {
  if (Gender[i]=="f"){
    new.Gender[i]=0
  } else {
    new.Gender[i]=1
  }
}
new.Gender
Gender

##Consider Salary and Gender, add a bonus of 100/1000=0.1 Euros to the monthly salary
#if the Gender is Female.
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

f2=function(a,b,c){
  delta=b^2 - 4*a*c
  if (delta>=0){
    return("Real solution(s)")
  } else {
    return("No real Solution")
  }
}

#2x^2 -10x +100=0
f2(2, -10, 100)

