#####DECISION TREES/RANDOM FOREST#####

# Use decision trees to prepare a model on fraud data 
# treating those who have taxable_income <= 30000 as "Risky" and others 
# are "Good"

#loading dataset
fraud<-read.csv(file.choose())
View(fraud)
attach(fraud)
str(fraud)
head(fraud)

library(DataExplorer)
plot_density(fraud$Taxable.Income)

library(party)

Risky_Good = ifelse(fraud$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(fraud,Risky_Good)

#Splitting data into training and testing
set.seed(1234) #To get reproducible result
ind<- sample(2,nrow(FC), replace=TRUE, prob=c(0.7,0.3))
FC_train <- FC[ind==1,]
FC_test <- FC[ind==2,]

#Building model formula using data
RG_formula<- Risky_Good ~ Undergrad + Marital.Status + City.Population + 
  Work.Experience + Urban
RG_ctree <- ctree(RG_formula, data=FC)
print(RG_ctree)
summary(RG_ctree)
plot(RG_ctree)


#Building model formula using training data
RG_formula_tr<- Risky_Good ~ Undergrad + Marital.Status + City.Population + 
  Work.Experience + Urban
RG_ctree_tr <- ctree(RG_formula_tr, data=FC_train)
print(RG_ctree_tr)
summary(RG_ctree_tr)
plot(RG_ctree_tr)

#####
pred_tree <- as.data.frame(predict(RG_ctree_tr,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(RG_ctree_tr,newdata=FC_test)
mean(pred_test_df==FC_test$Risky_Good)
# Model accuracy is 79.34%; Misclassification error is 20.7%

library(gmodels)
library(caret)
CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)
