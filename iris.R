#####DECISION TREES/RANDOM FOREST#####

# Build a decision tree for the 'iris' data with function 'ctree()' in 
#package "party".

#loading iris dataset
library(party)
library(caret)
library(gmodels)
data("iris")
str(iris)
head(iris)

# Splitting data into training and testing
set.seed(1234) #To get reproducible result
ind <- sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
iris_train <- iris[ind==1,]
iris_test <- iris[ind==2,]

#Building model formula 
irisFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(irisFormula, data=iris_train)

#Checking model predictions on training data
train_predict <- predict(iris_ctree,iris_train,type="response")
table(train_predict,iris_train$Species)
mean(train_predict == iris_train$Species) * 100
# Model accuracy is 96.43%; Misclassification error is 3.57%
CrossTable(iris_train$Species,train_predict)
confusionMatrix(iris_train$Species,train_predict)

#Checking model predictions on testing data
test_predict <- predict(iris_ctree,iris_test,type="response")
table(test_predict,iris_test$Species)
mean(test_predict == iris_test$Species) * 100
# Model accuracy is 94.74%; Misclassification error is 5.26%
CrossTable(iris_test$Species,test_predict)
confusionMatrix(iris_test$Species,test_predict)

#plotting the decision tree
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")

