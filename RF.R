# Using Random Forest
library(randomForest)
data(iris)
View(iris)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) # 100% accuracy 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(iris_test$Species, pred_test,
                       prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                       dnn = c('actual default', 'predicted default'))
