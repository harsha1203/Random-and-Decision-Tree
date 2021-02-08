#i guess party package is used only for numerical independent variables
fraud_check <- read.csv(file.choose())
View(fraud_check)
attach(fraud_check)
head(fraud_check)
library(caret)
library(C50)

summary(fraud_check)
category<-ifelse(fraud_check$Taxable.Income<=30000, "Risky","Good")

category
customer<-data.frame(fraud_check,category)
View(customer)
table(customer$category)
customer<-customer[,c(-3)]
names(customer)



#partioning data
set.seed(1234)
pd<-sample(2,nrow(customer),replace=TRUE,prob = c(0.8,0.2))
train<-customer[pd==1,]
test<-customer[pd==2,]
#Decision tree with party
install.packages("party")
library(party)
tree <- ctree(category~Undergrad+Marital.Status+City.Population+Work.Experience+Urban, 
              data=train)
tree
names(customer_train)
print(tree)
plot(tree,type="simple")

#Misclassification error for train data
train_predict<-predict(tree,data=train,type="response")
train_predict
a<-table(train_predict,train$category)
a
1-sum(diag(a))/sum(a)
mean(table(train_predict != train$category))
table(customer$category)


#Misclassification error for test data
test_predict<-predict(tree,newdata=test)
test_predict
b<-table(test_predict,test$category)
print(tab)
1-sum(diag(b))/sum(b)


#decission tree with rpart  
library(rpart)
tree1<-rpart(category~Undergrad+Marital.Status+City.Population+Work.Experience+Urban, 
              data=train)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree1,extra=1)
rpart.plot(tree1,extra=2)
rpart.plot(tree1,extra=4)
#prediction
predict(tree1,test)



