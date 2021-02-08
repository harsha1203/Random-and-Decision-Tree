CompanyData <- read.csv(file.choose())
View(CompanyData)
str(CompanyData)
install.packages("randomForest")
install.packages("Mass")
# install.packages("caret")
library(MASS)
library(caret)
library(randomForest)
#data partioning
set.seed(123)

hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
highsales = ifelse(CompanyData$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
CD = data.frame(CompanyData[2:11], highsales)
str(CD)
table(CD$highsales)
#data partioning
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)
rf <- randomForest(highsales~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting
attributes(rf)
pred1 <- predict(rf, train)
head(pred1)
confusionMatrix(pred1, train$highsales)# 100 % accuracy on training data 
# looks like the first six predicted value and original value matches.
# more than 95% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales) # 82.61 % accuracy on test data 
# Error Rate in Random Forest Model :
plot(rf)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1
pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales)  # 100 % accuracy on training data 
# Around 98% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales) # 84.35 % accuracy on test data 
# Confidence Interval is around 90 % 


# no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
# Majority of the trees has an average number of 45 to 50 nodes. 

# Variable Importance :

varImpPlot(rf1) #this graph tests how worse the model performs without each variable
# Mean Decrease Accuracy graph shows that how worst the model performs without each variable.
# say ShelveLoc is the most important variable for prediction.on looking at population,it has no value.
# gini graph- this graph measures how pure the nodes are at the end of the tree without each variable.
# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Price is very important and Urban is not that important.

varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
# Quantitative values 
importance(rf1)
varUsed(rf)   # which predictor variables are actually used in the random forest./how ofen they have
# appeared or occured in the entire random forest

# Partial Dependence Plot - here doing for price
partialPlot(rf1, train, Price, "Yes")
# On that graph, i see that if the price is 100 or greater, than they are not buying those computers.

# Extract single tree from the forest :

getTree(rf, 1, labelVar = TRUE)
# if the status is negative that means it is terminal node
#for terminal nodes we do not have values for left and write daughter

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, CD$highsales)
