require(tidyverse)
require(plotly)
require(GGally)
require(ggplot2)
require(mice)
require(reshape)
require(MASS)
require(ggpubr)
require(car)
require(factoextra)
require(class)
require(pROC)
require(ggnewscale)
require(data.table)
require(dplyr)
require(stringr)
require(DT)
require(tidyr)
require(corrplot)

require(dplyr)
if (!require("hms")) install.packages("hms") # fors non serve
if (!require("kableExtra")) install.packages("kableExtra") # forse non serve



########################################################
## Data Cleaning, Exploratory Data Analysis
########################################################

#load csv file and create dataframe
BankData <- read.csv('bank_accounts_train.csv', stringsAsFactors = T)

BankData =  BankData %>%
  filter(!duplicated(.)) %>%
  dplyr::select(-1) %>%
  mutate(Closed_Account = as_factor(.$Closed_Account)) %>%
  mutate(Education_Level = na_if(Education_Level, 'Unknown')) %>%
  mutate(Marital_Status = na_if(Marital_Status, 'Unknown'))


########################################################
## Describe the data, measure and visualize the most relevant relations
########################################################

str(BankData)
head(BankData)
summary(BankData)


#take out variables in order to better plot them
Customer_Age = BankData$Customer_Age
Dependent_count = BankData$Dependent_count
Months_on_book = BankData$Months_on_book
Total_Relationship_Count = BankData$Total_Relationship_Count
Months_Inactive_12_mon = BankData$Months_Inactive_12_mon
Contacts_Count_12_mon = BankData$Contacts_Count_12_mon
Credit_Limit = BankData$Credit_Limit
Total_Revolving_Bal = BankData$Total_Revolving_Bal
Avg_Open_To_Buy = BankData$Avg_Open_To_Buy
Total_Amt_Chng_Q4_Q1 = BankData$Total_Amt_Chng_Q4_Q1
Total_Trans_Ct = BankData$Total_Trans_Ct
Total_Trans_Amt = BankData$Total_Trans_Amt
Total_Ct_Chng_Q4_Q1 = BankData$Total_Ct_Chng_Q4_Q1
Avg_Utilization_Ratio = BankData$Avg_Utilization_Ratio
Income = BankData$Income
Closed_Account = BankData$Closed_Account



#Correlation Matrix
BankData %>% 
  dplyr::select(Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Amt_Chng_Q4_Q1, Total_Trans_Ct, Total_Trans_Amt, Total_Ct_Chng_Q4_Q1, Avg_Utilization_Ratio, Income) %>%
  ggcorr(nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")

# Visualize data
qplot(Months_on_book, data = BankData, geom = "histogram",fill=Education_Level , bins = 30)
qplot(Credit_Limit, data = BankData, geom = "histogram",fill=Marital_Status)
qplot(Months_on_book, data = BankData, geom = "density",col=Education_Level)
qplot(Credit_Limit, data = BankData, geom = "density",col=Card_Category)

#Gender
BankData %>%
  ggplot(aes(x = Gender, col = 'red')) +
  labs(y = 'Clients') +
  geom_bar()

#Box Plots
BankData_Num = data.table(
  BankData %>%
    dplyr::select(where(is.numeric))
)
BankData_Num$Income = as.integer(BankData_Num$Income)

MultiPlot <-  ggplot( melt(BankData_Num,id.vars=integer()), aes(factor(variable), value, color="Red")) + 
  geom_boxplot() + 
  theme(legend.position="none") +
  rremove('x.text') +  
  rremove('xylab') + 
  facet_wrap(~variable, scale="free")

#Show Plot
MultiPlot

########################################################
## Fit a Logistic Regression Model to estimate the effect of Income and Gender on the probability of closing acc.
########################################################

TrainSet = BankData[1:4301,]
TestSet = BankData[4301:5301,]

Model = glm( 
  formula = Closed_Account ~ Income + Gender, 
  family = binomial(link="logit"),
  data = TrainSet
)

summary(Model)

#Predictions
TestSet$prediction = predict(Model, TestSet, type = "response")

# plot results
TestSet %>%
  ggplot(aes(x=Income, y=Closed_Account, group=Gender)) + 
  geom_point(aes(col=Gender), alpha=0.6) + 
  geom_line(aes(y=prediction, col=Gender), data=TestSet)

#such logistic lines only confirm what we expected: income does not consistently affect
#the probability of closing an account

test_mod_pred = rep(0, nrow(TestSet))
test_mod_pred[TestSet$prediction>0.15]=1

#Test error rate
ErrorRate = 1 - mean(test_mod_pred==TestSet$Closed_Account)
ErrorRate

#Confusion matrix
table(test_mod_pred,TestSet$Closed_Account)

tab_rates_test = prop.table(table(test_mod_pred,TestSet$Closed_Account),margin = 2)  #test

ROC=roc(TestSet$Closed_Account,test_mod_pred,plot = TRUE,
        legacy.axes=TRUE,col="red",lwd=3,
        auc.polygon=T,auc.polygon.col="green",print.auc=T)


########################################################
## POINT 4
########################################################
##Consider only the continuous predictors Total Trans Amt and Total Trans Ct.

require(patchwork)
require(manipulate)
require(hexbin)
require(ggridges)
require(systemfonts)
require(scico) 
require(ggtext)
require(ggforce)    
require(ggdist)     
require(gclus)

TotalPred = data.frame(Total_Trans_Ct, Total_Trans_Amt)

TotalPred = prcomp(TotalPred, scale=TRUE)
fviz_eig(TotalPred)
TotalPred = data.frame(TotalPred[["x"]])

data = data.frame(TotalPred$PC1, TotalPred$PC2, BankData$Closed_Account)

#visualize data points
PP1 = data %>%
  ggplot(aes(x=TotalPred.PC1, y=BankData.Closed_Account)) + 
  geom_point()

PP2 = data %>%
  ggplot(aes(x=TotalPred.PC2, y=BankData.Closed_Account)) + 
  geom_point()

PP1 / PP2

#split into training and test set
df_train = data[1:4300,]
df_test = data[4301:5301,]

########################################################
## Model 1: k-NN
########################################################

#visualize all the data
data %>%
  ggplot(aes(x=TotalPred$PC1, y=TotalPred$PC2)) + 
  geom_point(aes(col=as.factor(BankData$Closed_Account))) + 
  scale_color_manual(values = c("#d31919", "#83d319"))+ 
  coord_fixed() +
  labs(x="PC1",
       y="PC2")

#Compute and visualize the optimal number of neighbors
scores = list()
ks = seq(1, 30)

for (i in ks){
  df_pred = copy(df_test)
  df_pred$class_ki = knn(train=df_train[,1:2], test = df_test[,1:2], k=i, cl=df_train$BankData.Closed_Account)
  
  df_pred %>%
    summarise(
      err = mean(BankData.Closed_Account != class_ki)
    )
  
  table(df_pred$BankData.Closed_Account, df_pred$class_ki)
  
  ROC=roc(df_test$BankData.Closed_Account,as.numeric(df_pred$class_ki),plot = FALSE,
          legacy.axes=TRUE,col="midnightblue",lwd=3,
          auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)
  scores[i] = ROC$auc
}


scores = matrix(scores)
df_scores = data.frame(ks, scores) 

df_scores %>%
  ggplot(aes(x=ks, y=as.numeric(scores)))+
  geom_line()+
  labs(x="K",
       y="AUC Score")

df_scores = as.data.frame(lapply(df_scores, unlist))
optimal_k = df_scores[order(df_scores$scores, decreasing=TRUE), ]

#list of values of K, in descending order of associated AUC score
optimal_k[[1]] 

#optimal number of neighbors
k = optimal_k[[1]][1]

#apply k-NN with the optimal number of neighbors
#k=17
df_pred = copy(df_test)
df_pred$class_k17 = knn(train=df_train[,1:2], test = df_test[,1:2], k=17, cl=df_train$BankData.Closed_Account)


# plot predicted regions
p_k17 = 
  df_pred %>%
  ggplot(aes(x=TotalPred$PC1[0:nrow(df_pred)], y=TotalPred$PC2[0:nrow(df_pred)]))+
  geom_point(aes(col=class_k17), alpha=0.9, pch = 16, size=1.5) +
  scale_color_manual("K=1", values = c("#d31919", "#83d319"))+
  coord_fixed() +
  labs(x=quote(X[1]),
       y=quote(X[2]))
p_k17

#compare predictions (on test set) with test set real values
p_k17 + 
  new_scale_color() + 
  geom_point(data=df_test, mapping=aes(x=TotalPred$PC1[0:nrow(df_test)], y=TotalPred$PC2[0:nrow(df_test)], col=as.factor(BankData$Closed_Account[0:nrow(df_test)]))) + 
  scale_color_manual("Test", values = c("#d31919", "#83d319"))



df_pred %>%
  summarise(
    err = mean(BankData$Closed_Account != class_k17)
  )

table(df_pred$BankData.Closed_Account, df_pred$class_k17)


ROC=roc(df_test$BankData.Closed_Account,as.numeric(df_pred$class_k17),plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)

########################################################
## Model 2: Discriminant Analysis (Discriminant Analysis)
########################################################

#fit the model with the training set
lda_model = lda(BankData.Closed_Account ~ TotalPred.PC1 + TotalPred.PC2, data=df_train)
lda_model

# predict test
test_pred = predict(lda_model, df_test[, 1:2])$class
#df_test$class_lda = test_pred

# plot predicted regions
p_lda = 
  df_test %>%
  ggplot(aes(x=TotalPred.PC1, y=TotalPred.PC2)) + 
  geom_point(aes(col=test_pred), alpha=1, pch = 16, size=2) + 
  scale_color_manual("Test fitted values", values = c("#d31919", "#83d319"))+
  coord_fixed() +
  labs(x=quote(X[1]),
       y=quote(X[2]))

p_lda

# visualize test data and predictions
p_lda + 
  new_scale_color() + 
  geom_point(data=df_test, mapping=aes(x=TotalPred.PC1, y=TotalPred.PC2, col=as.factor(BankData.Closed_Account))) + 
  scale_color_manual("True", values = c("#d31919", "#83d319"))

# classification error for lda
df_test %>%
  summarise(
    err = mean(BankData.Closed_Account != test_pred)
  )

#confusion matrix
table(df_test$BankData.Closed_Account, test_pred)

ROC=roc(df_test$BankData.Closed_Account,as.numeric(test_pred),plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)


########################################################
## Quadratic Discriminant Analysis (QDA)
########################################################

#Try with Quadratic Discriminant Analysis (QDA)
#fit the model with the training set
qda_model = qda(BankData.Closed_Account ~ TotalPred.PC1 + TotalPred.PC2, data=df_train)
qda_model

# predict test
test_pred = predict(qda_model, df_test[, 1:2])$class
#df_test$class_lda = test_pred

# plot predicted regions
p_qda = 
  df_test %>%
  ggplot(aes(x=TotalPred.PC1, y=TotalPred.PC2)) + 
  geom_point(aes(col=test_pred), alpha=1, pch = 16, size=2) + 
  scale_color_manual("Test fitted values", values = c("#d31919", "#83d319"))+
  coord_fixed() +
  labs(x=quote(X[1]),
       y=quote(X[2]))

p_qda

# visualize test data and predictions
p_qda + 
  new_scale_color() + 
  geom_point(data=df_test, mapping=aes(x=TotalPred.PC1, y=TotalPred.PC2, col=as.factor(BankData.Closed_Account))) + 
  scale_color_manual("True", values = c("#d31919", "#83d319"))

# classification error for qda
df_test %>%
  summarise(
    err = mean(BankData.Closed_Account != test_pred)
  )

#confusion matrix
table(df_test$BankData.Closed_Account, test_pred)

ROC=roc(df_test$BankData.Closed_Account,as.numeric(test_pred),plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)


########################################################
## POINT 5
########################################################

predictors = data.frame(Avg_Utilization_Ratio, Total_Ct_Chng_Q4_Q1, Total_Amt_Chng_Q4_Q1, Total_Revolving_Bal, Contacts_Count_12_mon, Months_Inactive_12_mon, Total_Relationship_Count, Total_Trans_Ct, Total_Trans_Amt)

#extract Gender from original dataframe
copy = copy(BankData["Gender"])

#encode Gender into Male, a binary variable (1 if male, 0 if female)
copy = 
  copy %>% 
  mutate(Male = ifelse(Gender == "M",1,0))

#add encoded Gender (Male) and response to our dataframe
predictors["Male"] = copy$Male
predictors["Closed_Account"] = BankData$Closed_Account

training_set = predictors[1:4300,]
test_set = predictors[4301:5301,]

#Compute and visualize the optimal number of neighbors
scores = list()
ks = seq(1, 30)

for (i in ks){
  df_pred = copy(test_set)
  df_pred$class_ki = knn(train=training_set[,1:10], test = test_set[,1:10], k=i, cl=training_set$Closed_Account)
  
  df_pred %>%
    summarise(
      err = mean(Closed_Account != class_ki)
    )
  
  table(df_pred$Closed_Account, df_pred$class_ki)
  
  ROC=roc(test_set$Closed_Account,as.numeric(df_pred$class_ki),plot = FALSE,
          legacy.axes=TRUE,col="midnightblue",lwd=3,
          auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)
  scores[i] = ROC$auc
}

scores = matrix(scores)
df_scores = data.frame(ks, scores) 

df_scores %>%
  ggplot(aes(x=ks, y=as.numeric(scores)))+
  geom_line()+
  labs(x="K",
       y="AUC Score")

df_scores = as.data.frame(lapply(df_scores, unlist))
optimal_k = df_scores[order(df_scores$scores, decreasing=TRUE), ]

optimal_k[[1]] #list of values of K, in descending order of associated AUC score
k = optimal_k[[1]][1] #optimal number of neighbors

#apply k-NN with the optimal number of neighbors
#k=5
df_pred = copy(test_set)
df_pred$class_k5 = knn(train=training_set[,1:10], test = test_set[,1:10], k=5, cl=training_set$Closed_Account)

err = df_pred %>%
  summarise(
    err = mean(Closed_Account != class_k5)
  )

# Accuracy New Model
Accuracy = 1-err

table(df_pred$Closed_Account, df_pred$class_k5)

ROC=roc(test_set$Closed_Account,as.numeric(df_pred$class_k5),plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)


########################################################
## LOGISTIC REGRESSION
########################################################

##prepare true test set
TestSet <- read.csv('bank_accounts_test.csv', stringsAsFactors = T)

TestSet = TestSet %>%
  filter(!duplicated(.)) %>%
  dplyr::select(-1) %>%
  mutate(Education_Level = na_if(Education_Level, 'Unknown')) %>%
  mutate(Marital_Status = na_if(Marital_Status, 'Unknown'))

      
BankDat = copy(BankData)
BankDat = BankDat[,1:19]
union = union(x=BankDat, y=TestSet)


Customer_Age              = union$Customer_Age
Dependent_count           = union$Dependent_count
Months_on_book            = union$Months_on_book
Total_Relationship_Count  = union$Total_Relationship_Count
Months_Inactive_12_mon    = union$Months_Inactive_12_mon
Contacts_Count_12_mon     = union$Contacts_Count_12_mon
Credit_Limit              = union$Credit_Limit
Total_Revolving_Bal       = union$Total_Revolving_Bal
Avg_Open_To_Buy           = union$Avg_Open_To_Buy
Total_Amt_Chng_Q4_Q1      = union$Total_Amt_Chng_Q4_Q1
Total_Trans_Ct            = union$Total_Trans_Ct
Total_Trans_Amt           = union$Total_Trans_Amt
Total_Ct_Chng_Q4_Q1       = union$Total_Ct_Chng_Q4_Q1
Avg_Utilization_Ratio     = union$Avg_Utilization_Ratio
Income                    = union$Income
Closed_Account            = union$Closed_Account

str(Closed_Account)

predictors = data.frame(Avg_Utilization_Ratio, Total_Ct_Chng_Q4_Q1, Total_Amt_Chng_Q4_Q1, Total_Revolving_Bal, Contacts_Count_12_mon, Months_Inactive_12_mon, Total_Relationship_Count, Total_Trans_Ct, Total_Trans_Amt)

PCA = prcomp(predictors, scale=TRUE)
fviz_eig(PCA) 
PCA = data.frame(PCA[["x"]])

PCA["AmtChTr"] = predictors$Total_Amt_Chng_Q4_Q1/predictors$Total_Trans_Amt
PCA["CtChTr"] = predictors$Total_Ct_Chng_Q4_Q1/predictors$Total_Trans_Ct
PCA["RevolvingOverRelationship"] = predictors$Total_Revolving_Bal/predictors$Total_Relationship_Count
PCA["ChangesRatio"] = predictors$Total_Amt_Chng_Q4_Q1/predictors$Total_Ct_Chng_Q4_Q1
PCA["Gender"] = union$Gender
PCA["Closed_Account"] = union$Closed_Account

initial_split = PCA[1:5301,]
initial_split["Closed_Account"] = BankData$Closed_Account
training_set = initial_split[1:4300,]
test_set = initial_split[4301:5301,]
true_test_set = PCA[5302:10127,]

poly(PCA$PC1,9)

training_set$Closed_Account

#model with PCA
mod = glm(formula= BankData$Closed_Account ~ poly(PCA$PC1,9)+
                                  poly(PCA$PC2,9)+
                                  poly(PCA$PC3,2)+
                                  poly(PCA$PC4,7)+
                                  poly(PCA$PC5,5)+
                                  poly(PCA$PC6,2)+
                                  poly(PCA$PC7,2)+
                                  poly(PCA$PC8,5)+
                                  poly(PCA$PC9,5)+
                                  PCA$Gender+
                                  PCA$ChangesRatio+
                                  poly(PCA$AmtChTr,5)+
                                  poly(PCA$CtChTr,5)+
                                  poly(PCA$RevolvingOverRelationship,4), 
                family = binomial(link="logit"))

summary(mod)

#make predictions
p_pred = predict(mod, test_set, type = "response")
test_set$p_pred = p_pred


#Compute and visualize the optimal threshold
scores = list()
ts = seq(0.05, 0.60, by=0.01)
index = 0

for (t in ts){
  test_mod_pred = rep(0, nrow(test_set))
  test_mod_pred[p_pred>t]=1
  
  ROC=roc(test_set$Closed_Account, test_mod_pred, plot = FALSE,
          legacy.axes=TRUE, col="midnightblue", lwd=3,
          auc.polygon=T, auc.polygon.col="lightblue", print.auc=T)
  scores[index] = ROC$auc
  index = index + 1
}

scores = matrix(scores)
df_scores = data.frame(ts[2:56], scores) 

df_scores = as.data.frame(lapply(df_scores, unlist))
optimal_t = df_scores[order(df_scores$scores, decreasing=TRUE), ]

optimal_t[[1]] #list of values of t, in descending order of associated AUC score
t = optimal_t[[1]][1] #optimal threshold

test_mod_pred = rep(0, nrow(test_set))
test_mod_pred[p_pred>t]=1

1-mean(test_mod_pred==test_set$Closed_Account) #overall test error rate

table(test_mod_pred,test_set$Closed_Account)#confusion matrix

tab_rates_test = prop.table(table(test_mod_pred,test_set$Closed_Account),margin = 2)  #test

ROC=roc(test_set$Closed_Account,test_mod_pred,plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)
##

##without PCA with final submission (true test set predictions)
predictors["AmtChTr"] = predictors$Total_Amt_Chng_Q4_Q1/predictors$Total_Trans_Amt
predictors["CtChTr"] = predictors$Total_Ct_Chng_Q4_Q1/predictors$Total_Trans_Ct
predictors["RevolvingOverRelationship"] = predictors$Total_Revolving_Bal/predictors$Total_Relationship_Count
predictors["ChangesRatio"] = predictors$Total_Amt_Chng_Q4_Q1/predictors$Total_Ct_Chng_Q4_Q1
predictors["Gender"] = union$Gender

training_set = predictors[1:4300,]
test_set = predictors[4301:5301,]
true_test_set = predictors[5302:10127,]

initial_split = predictors[1:5301,]
initial_split["Closed_Account"] = BankData$Closed_Account
training_set = initial_split[1:4300,]
test_set = initial_split[4301:5301,]
true_test_set = predictors[5302:10127,]
training_set$Closed_Account = BankData$Closed_Account[0:4300]
#model without PCA
mod = glm(formula = Closed_Account ~ poly(Avg_Utilization_Ratio,6)+poly(Total_Ct_Chng_Q4_Q1,3)+
            poly(Total_Amt_Chng_Q4_Q1,5)+poly(Total_Revolving_Bal,3)+
            poly(Contacts_Count_12_mon,5)+poly(Months_Inactive_12_mon,2)+
            poly(Total_Relationship_Count,2)+poly(Total_Trans_Ct,5)+
            poly(Total_Trans_Amt,5)+Gender+ChangesRatio+poly(AmtChTr,5)+
            poly(CtChTr,5)+poly(RevolvingOverRelationship,4), 
          family = binomial(link="logit"),
          data = training_set)
##

summary(mod)

#make predictions
p_pred = predict(mod, test_set, type = "response")
test_set$p_pred = p_pred


#Compute and visualize the optimal threshold
scores = list()
ts = seq(0.05, 0.60, by=0.01)
index = 0

for (t in ts){
  test_mod_pred = rep(0, nrow(test_set))
  test_mod_pred[p_pred>t]=1
  
  ROC=roc(test_set$Closed_Account,test_mod_pred,plot = FALSE,
          legacy.axes=TRUE,col="midnightblue",lwd=3,
          auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)
  scores[index] = ROC$auc
  index = index + 1
}

scores = matrix(scores)
df_scores = data.frame(ts[2:56], scores) 

df_scores = as.data.frame(lapply(df_scores, unlist))
optimal_t = df_scores[order(df_scores$scores, decreasing=TRUE), ]

optimal_t[[1]] #list of values of t, in descending order of associated AUC score
t = optimal_t[[1]][1] #optimal threshold

test_mod_pred = rep(0, nrow(test_set))
test_mod_pred[p_pred>t]=1

1-mean(test_mod_pred==test_set$Closed_Account) #overall test error rate

table(test_mod_pred,test_set$Closed_Account)#confusion matrix

tab_rates_test = prop.table(table(test_mod_pred,test_set$Closed_Account),margin = 2)  #test

ROC=roc(test_set$Closed_Account,test_mod_pred,plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)

#AUC = 0.935 on the "fake" test set; we expect such AUC to be a bit lower on the true test set
#due to the much larger number of rows; it will probably fall between 0.920 and 0.930

#TRUE TEST SET - predictions (Predictions on the REAL test set using the threshold defined before)
p_pred = predict(mod, true_test_set, type = "response")
true_test_set$p_pred = p_pred

test_mod_pred = rep(0, nrow(true_test_set))
test_mod_pred[p_pred>t]=1
true_test_set$pred = test_mod_pred


table(test_mod_pred)#confusion matrix

#where "test_mod_pred" are the predictions for each row of the true test set

########################################################
## POINT 6
########################################################

#We have to provide the correct threshold value in order to provide a relevant metrics in the case of the cost (gain) matrix
# Let's suppose that
# For every customer that doesn't close the contract = -50 cost (corresponds to a 50 gain)
# For every customer that closes the contract = +50 cost (corresponds to a -50 gain)
# If we are able to predict that an user is about to close the account we can propose a more convenient offer to mantain that client this mill mean = -20 cost ( corresponds to a +20 gain)
(costMat <- matrix(c(50,0,-50,--20), ncol=2, 
                   dimnames = list(c("No", "Yes"), c("No", "Yes"))))
# If we propose the offer we are sure that they accept so we gain 50 and pay 30, so we get 20
# If we do not propose the offer rand they don't close the contract we get 50 
# If we do not propose the offer and they close the account we lose 50

# To understand the overall performances in terms of the cost matrix, we must pick a threshold, compute the confusion matrix, and match it with cost matrix
# Keeping the same threshold of before (che threshold abbiamo deciso prima?), what is the final cost?
# On the train
confMatAic <- table(stepAicPreds, d_train$y)
(totcostAic <- sum(costMat*confMatAic))
confMatBic <- table(stepBicPreds, d_train$y)
(totcostBic <- sum(costMat*confMatBic))
# AIC has lower cost = larger gain

# Sul test
confMatAicOut <- table(stepAicPredsOut, d_val$y)
(totcostAicOut <- sum(costMat*confMatAicOut))
confMatBicOut <- table(stepBicPredsOut, d_val$y)
(totcostBicOut <- sum(costMat*confMatBicOut))
# AIC has lower cost = larger gain

#in sostanza dobbiamo fare vari test manualmente, bisogna provare diversi treshold values
#il treshold deve rappresentare fino a quale livello all'interno della logistic curve noi pensiamo che un cliente stia per lasciare la banca
#allora da quel valore(porzione di customer base) la banca offrir?? un servizio pi?? conveniente che convincer?? il cliente a rimanere e a far comunque guadagnare un +20 alla banca
#DOMANDA: dove posso andare a cambiare il treshold value per decidere a quale livello della curva i customers stanno per lascviare la banca?


########################################################
## 2) CLUSTERING
########################################################

###############################
### POINT 7
###############################

purchases <- read.csv('purchases.csv', stringsAsFactors = T)

str(purchases)
head(purchases)
summary(purchases)

Fresh = purchases$Fresh
Milk = purchases$Milk
Grocery = purchases$Grocery
Frozen = purchases$Frozen
Detergents_Paper = purchases$Detergents_Paper
Delicatessen = purchases$Delicatessen



#PCA & PCA VISUALIZATION
purchases_num = purchases[3:8]
pr_comp = prcomp(purchases_num, scale=TRUE)


fviz_eig(pr_comp)

fviz_pca_ind(pr_comp,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pr_comp,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_biplot(pr_comp, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


reduced_purchases = data.frame(pr_comp[["x"]])
reduced_purchases = reduced_purchases %>%
  filter(reduced_purchases$PC1>-2.5 & reduced_purchases$PC2>-2.5)

x <- reduced_purchases$PC1
y <- reduced_purchases$PC2
z <- reduced_purchases$PC3

reduced_purchases%>%
  ggplot(aes(x=x, y=y)) +
  geom_point(alpha=0.5, size=3, color="blue")  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


fig1 = 
  plot_ly(x=x, y=y, z=z,
          marker=list(size=3, color="blue", 
                      colorscale="rgb(0,0,0)", 
                      line=list(color="rgb(0,0,0)", width=1),
                      showscale=F,
                      opacity=0.3)) %>%
  add_markers() %>%
  layout(scene = list(
    aspectratio =  list(x=1, y=1, z=1)
  )) 
fig1

#Clustering

clustering_initialize = function(dataframe, nclusters, nrows){ 
  # randomly assign points
  dataframe$lab = as.factor(sample(1:nclusters, nrows, replace = T))
  # (random) centroids, 2 principal components
  centr = dataframe %>%
    group_by(lab) %>%
    summarise(
      centr_1 = mean(PC1),
      centr_2 = mean(PC2)
    )
  
  #represent randomly-assigned points and randomly-assigned centroids
  p0 = ggplot(dataframe, aes(x=PC1, y=PC2)) +
    geom_point(aes(col=lab), alpha=0.5, size=3)  +
    geom_point(aes(x=centr_1, y=centr_2, col=lab), pch=15, alpha=0.7, size=4, data=centr) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  pp0 = ggarrange(p0, labels=c("Initialization"))
  
  toreturn = list(dataframe, nclusters, centr, pp0)
  return(toreturn)
}



clustering_iterate = function(steps, nclusters, dataframe, nrows, centr){
  #iteration
  for(i in 1:steps){
    dists = numeric(nclusters)
    for(j in 1:nclusters)
      dists[j] = sum((dataframe[i,1:2] - centr[j,2:3])^2)
    dataframe$lab[i] = centr$lab[which.min(dists)]
  }
  
  
  p1 = ggplot(dataframe, aes(x=PC1, y=PC2)) +
    geom_point(aes(col=lab), alpha=0.5, size=3)  +
    geom_point(aes(x=centr_1, y=centr_2, col=lab), pch=15, alpha=0.7, size=4, data=centr) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  # now recompute centroids
  centr = dataframe %>%
    group_by(lab) %>%
    summarise(
      centr_1 = mean(PC1),
      centr_2 = mean(PC2)
    )
  
  
  p2 = ggplot(dataframe, aes(x=PC1, y=PC2)) +
    geom_point(aes(col=lab), alpha=0.5, size=3)  +
    geom_point(aes(x=centr_1, y=centr_2, col=lab), pch=15, alpha=0.7, size=4, data=centr) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  pp2 = ggarrange(p1, p2, labels = c("New partition, old centroids", "New partition, New centroids"), ncol=2)
  #Reiterate until convergence (i.e., until centroids recomputation does not yield changes)
  toreturn = list(dataframe, nclusters, centr, pp2)
  return(toreturn)
}

initialized = clustering_initialize(reduced_purchases, 2, 404)
initialized[[4]]

iteration1 = clustering_iterate(400, initialized[[2]], initialized[[1]], 404, initialized[[3]])
iteration1[[4]]

iteration2 = clustering_iterate(400, iteration1[[2]], iteration1[[1]], 404, iteration1[[3]])
iteration2[[4]]

iteration3 = clustering_iterate(400, iteration2[[2]], iteration2[[1]], 404, iteration2[[3]])
iteration3[[4]]

iteration4 = clustering_iterate(400, iteration3[[2]], iteration3[[1]], 404, iteration3[[3]])
iteration4[[4]]
#convergence reached

#Elbow Method for finding the optimal number of clusters
set.seed(123)
#Visualize WSS for k = 1 to k = 15.
kmax <- 15
WSS <- sapply(1:kmax, function(k){kmeans(reduced_purchases, k, nstart=50,iter.max = 15 )$tot.withinss})
#where WSS is the within-cluster sum of squares, i.e. intra-class variance
plot(1:kmax, WSS,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters (K)",
     ylab="Within-cluster Sum of Squares (WSS)")

#R k-means function
kmm = kmeans(reduced_purchases[1:2],6,nstart = 5,iter.max = 15) 

centr = data.frame(kmm[["centers"]])
centr["cluster"] = c(1, 2, 3, 4, 5, 6)


ggplot(reduced_purchases, aes(x=PC1, y=PC2)) +
  geom_point(aes(col=as.factor(kmm$cluster)), alpha=1, size=3) + #represent data points
  geom_point(aes(x=PC1, y=PC2, col = as.factor(cluster)), pch=15, alpha=1, size=5, data=centr) + #represent centroids
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#R hierarchical clustering function

set.seed(1)


h_1 = hclust(dist(reduced_purchases), method = "complete")
h_2 = hclust(dist(reduced_purchases), method = "single")
h_3 = hclust(dist(reduced_purchases), method = "average")

plot(h_1, hang=0.05)
plot(h_2, hang=0.05)
plot(h_3, hang=0.05)


#-Distance-based method
h1 = hclust(dist(reduced_purchases), method="average")
plot(h1)
abline(h=3.5)
cutree(h1, h=3.5)

#-Correlation-based method
d2 = as.dist(1 - cor(t(reduced_purchases)))
h2 = hclust(d2, method="average")
plot(h2)
abline(h=1, col=2)
cutree(h2, h=1)


########################################################
##POINT 8
########################################################
#For this point we will take in consideration labels obtained from the K-means clustering algorithm


#recover Channel from purchases dataframe and match it together with pr_comp[["x"]] since it is
#the only way of retrieving the principal components with the original index, before 
#outliers were ruled out

df = data.frame(pr_comp[["x"]])
df["Channel"] = purchases$Channel
df = df %>%
  filter(df$PC1>-2.5 & df$PC2>-2.5)


ChannelsLab = data.frame(kmm$cluster)
colnames(ChannelsLab)[1] = "Cluster"
ChannelsLab["Channel"] = df$Channel

#confusion matrix
table(ChannelsLab$Cluster, ChannelsLab$Channel)
#As expected, the great majority of Retail units stand in a specific cluster, while 
#the great majority of Food Service units stand in another distinct one

#same as before, recover Region
df2 = data.frame(pr_comp[["x"]])
df2["Region"] = purchases$Region
df2 = df2 %>%
  filter(df2$PC1>-2.5 & df2$PC2>-2.5)

RegionsLab = data.frame(kmm$cluster)
colnames(RegionsLab)[1] = "Cluster"
RegionsLab["Region"] = df2$Region

#confusion matrix
table(RegionsLab$Cluster, RegionsLab$Region)
#We notice that, this time, clusters are not properly paired with regions

