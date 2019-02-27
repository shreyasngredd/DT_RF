#####DECISION TREES/RANDOM FOREST#####

# A cloth manufacturing company is interested to know about the segment or 
# attributes causes high sale.

#loading dataset
cloth<-read.csv(file.choose())
View(cloth)
attach(cloth)
str(cloth)
head(cloth)

library(DataExplorer)
plot_density(cloth$Sales)

library(party)

High_low = ifelse(cloth$Sales<10, "No", "Yes")
FC = data.frame(cloth,High_low)

#Splitting data into training and testing
set.seed(1234) #To get reproducible result
ind<- sample(2,nrow(FC), replace=TRUE, prob=c(0.7,0.3))
FC_train <- FC[ind==1,]
FC_test <- FC[ind==2,]

#Building model formula using training data
HL_formula<- High_low ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc+
  Age + Education + Urban + US
HL_ctree <- ctree(HL_formula, data=FC_train)
summary(HL_ctree)
plot(HL_ctree)
plot(HL_ctree,type="simple")

###Checking model predictions using test data
pred_tree1 <- as.data.frame(predict(HL_ctree,newdata=FC_test))
pred_tree1["final"] <- NULL
pred_test_df1 <- predict(HL_ctree,newdata=FC_test)
mean(pred_test_df1 == FC$High_low)*100
#Model accuracy is 66.25%; Misclassification error is 33.75%

library(gmodels)
CrossTable(FC_test$High_low,pred_test_df1)
library(caret)
confusionMatrix(FC_test$High_low,pred_test_df1)

###Checking model predictions using training data
pred_tree2 <- as.data.frame(predict(HL_ctree,newdata=FC_train))
pred_tree2["final"] <- NULL
pred_test_df2 <- predict(HL_ctree,newdata=FC_train)
mean(pred_test_df2 == FC$High_low)*100
#Model accuracy is 69.5%; Misclassification error is 30.5%

CrossTable(FC_train$High_low,pred_test_df2)
confusionMatrix(FC_train$High_low,pred_test_df2)

###using tree fuction
library(tree)
cd_tree_org <- tree(High_low~.-Sales,data=FC)
summary(cd_tree_org)

plot(cd_tree_org)
text(cd_tree_org,pretty = 0)

###using tree function for training data
cd_tree_org2 <- tree(High_low~.-Sales,data=FC_train)
summary(cd_tree_org2)

plot(cd_tree_org2)
text(cd_tree_org2,pretty = 0)

###Evaluating the model
pred_tree3 <- as.data.frame(predict(cd_tree_org,newdata=FC_test))
pred_tree3["final"] <- NULL

pred_test_df <- predict(cd_tree_org,newdata=FC_test)
pred_tree3$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
pred_tree3$final <- as.factor(pred_tree3$final)

summary(pred_tree3$final)

summary(FC_test$High_low)

mean(pred_tree3$final==FC$High_low)*100
# Model accuracy is 68.5%; Misclassification error is 31.5%

CrossTable(FC_test$High_low,pred_tree3$final)
confusionMatrix(FC_test$High_low,pred_tree3$final)
