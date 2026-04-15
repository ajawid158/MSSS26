##Supervised Methods Classification
#1. Logistic Regression

gdt=read.csv("stgrades.csv")
head(gdt)

#X=c(Gender, StuyHrs, Major) y=Grade

#data prep
table(gdt$Grade)

gdt$Grade=ifelse(gdt$Grade=="A",2,
                 ifelse(gdt$Grade=="B", 1, 0))
table(gdt$Grade)
gdt$Grade=factor(gdt$Grade)


gdt$Gender=ifelse(gdt$Gender=="Male",1,0)
head(gdt)

gdt$Major=ifelse(gdt$Major=="IT",2,
                 ifelse(gdt$Major=="Business",1,0))
head(gdt)
View(gdt)

#Split the dataset into test and train
s=sample(nrow(gdt), 0.7*nrow(gdt))

gdt.training=gdt[s, ]
gdt.test=gdt[-s, ]

#Models Construction
library(nnet)
#X=c(Gender, StuyHrs, Major)
names(gdt)
models=list(
  m1=multinom(Grade~StudyHrs, data = gdt.training, trace=F),
  m2=multinom(Grade~Gender, data = gdt.training, trace=F),
  m3=multinom(Grade~Major, data = gdt.training, trace=F),
  m4=multinom(Grade~StudyHrs + Gender, data = gdt.training, trace=F),
  m5=multinom(Grade~StudyHrs + Major, data = gdt.training, trace=F),
  m6=multinom(Grade~ Gender + Major, data = gdt.training, trace=F),
  m7=multinom(Grade~ StudyHrs + Gender + Major, data = gdt.training, trace=F)
)

#Test the performance

test_error = sapply(models, function(model){
  pred=predict(model, newdata=gdt.test)
  mean(pred != gdt.test$Grade)
})

test_error
barplot(test_error, 
        horiz = T)

best_model_name= names(which.min(test_error))
best_model_name


##2. KNN
View(gdt)
head(gdt)

#Scale Study Hrs
sc.stHr=(gdt$StudyHrs-mean(gdt$StudyHrs))/(sd(gdt$StudyHrs))
plot(density(sc.stHr))
plot(density(gdt$StudyHrs))
##
library(class)

#Data
head(gdt.training)
head(gdt.test)
gdt.training$Grade=factor(gdt.training$Grade)
gdt.test$Grade=factor(gdt.test$Grade)

gdt$Major=factor(gdt$Major)

#Model Construction
names(gdt)

predictor_sets= list(
  c("StudyHrs"), 
  c("Gender"), 
  c("Major"), 
  c("StudyHrs", "Gender"), 
  c("StudyHrs","Major"),
  c("Gender","Major"), 
  c("StudyHrs", "Gender", "Major")
)

model_names=c("m1","m2","m3","m4","m5","m6","m7" )

knn_test=sapply(1:7, function(i){
  vars=predictor_sets[[i]]
  train_x=scale(gdt.training[,vars, drop=F])
  test_x= scale(gdt.test[, vars, drop=F], 
                center=attr(train_x, "scaled:center"),
                scale=attr(train_x, "scaled:scale"))
  
  pred=knn(train = train_x, test = test_x, cl=gdt.training$Grade, k=9)
  mean(pred!=gdt.test$Grade)
})

names(knn_test)=model_names
knn_test

barplot(knn_test, 
        horiz = T)
best_model_name= names(which.min(knn_test))
best_model_name

#3. Decision Tree 
library(rpart)
library(rpart.plot)

#data
head(gdt.training)
head(gdt.test)


#Models Construction
models=list(
  m1=rpart(Grade~StudyHrs, data = gdt.training,method = "class"),
  m2=rpart(Grade~Gender, data = gdt.training,method = "class" ),
  m3=rpart(Grade~Major, data = gdt.training, method = "class"),
  m4=rpart(Grade~StudyHrs + Gender, data = gdt.training, method = "class"),
  m5=rpart(Grade~StudyHrs + Major, data = gdt.training, method = "class"),
  m6=rpart(Grade~ Gender + Major, data = gdt.training, method = "class"),
  m7=rpart(Grade~ StudyHrs + Gender + Major, data = gdt.training, method = "class")
)

#test the performance
test_error = sapply(models, function(model){
  pred=predict(model, newdata=gdt.test)
  mean(pred != gdt.test$Grade)
})

test_error

barplot(test_error, horiz = T)
best_model_name=names(which.min(test_error))
best_model_name
rpart.plot(models[["m1"]])
#4. Random Forest
library(randomForest)

#data
head(gdt.training)
head(gdt.test)

#Model Construction
models=list(
  m1=randomForest(Grade~StudyHrs, data = gdt.training),
  m2=randomForest(Grade~Gender, data = gdt.training),
  m3=randomForest(Grade~Major, data = gdt.training),
  m4=randomForest(Grade~StudyHrs + Gender, data = gdt.training),
  m5=randomForest(Grade~StudyHrs + Major, data = gdt.training),
  m6=randomForest(Grade~ Gender + Major, data = gdt.training),
  m7=randomForest(Grade~ StudyHrs + Gender + Major, data = gdt.training)
)

#Test the performance
test_error = sapply(models, function(model){
  pred=predict(model, newdata=gdt.test)
  mean(pred != gdt.test$Grade)
})

test_error

barplot(test_error, horiz = T)
best_model_name=names(which.min(test_error))
best_model_name



