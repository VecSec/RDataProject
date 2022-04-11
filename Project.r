# Package with install errors

#require(patchwork) #to plot several charts jointly
#require(manipulate)
#require(hexbin) #for heatmaps
#require(ggridges) #for ridgeplots
#require(systemfonts) # use custom fonts (need to be installed on your OS)  
#require(scico)       # scico color palettes(http://www.fabiocrameri.ch/colourmaps.php) in R 
#require(ggtext)      # add improved text rendering to ggplot2
#require(ggforce)     # add missing functionality to ggplot2
#require(ggdist)      # add uncertainty visualizations to ggplot2
#require(gclus)
#_______________

require(tidyverse)
require(plotly)
require(GGally)
require(ggplot2)
require(mice)
require(reshape)
require(MASS)
require(ggpubr)
require(car) #for variance inflation factor etc.
require(factoextra) #to graphically represent PCA results
require(class)
require(pROC)
require(ggnewscale)
require(data.table)
require(dplyr)
require(stringr)
require(DT)
require(tidyr)
require(corrplot)

if (!require("hms")) install.packages("hms")
if (!require("kableExtra")) install.packages("kableExtra")

######Data Cleaning, Exploratory Data Analysis


#load csv file and create dataframe
BankData <- read.csv('bank_accounts_train.csv', stringsAsFactors = T)

BankData = BankData %>%
  filter(!duplicated(.)) %>%
  dplyr::select(-1) %>%
  mutate(Closed_Account = as_factor(.$Closed_Account)) %>%
  mutate(Education_Level = na_if(Education_Level, 'Unknown')) %>%
  mutate(Marital_Status = na_if(Marital_Status, 'Unknown'))



# How to deal with unknown values?
# Since there are unknown values only for the categorical variables, they will not harm
# predictive power of any model for the simple reason that Unknown by itself may be considered as
# a category. We thus have two options:
# Option 1: we consider Unknown as a category by itself, thus without dealing with it
# Option 2: we replace all Unknowns with NA and ultimately discard them
# As we will see soon, the only categorical variable affecting Closed_Account is Gender,
# while Unknown classes exist for Education Level and Marital Status; discarding Unknown rows
# would be a huge mistake, since it would cause a useless reduction of rows that may reduce
# our final model's accuracy.



###POINT 2
#Describe the data with some simple built-in functions
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

#ERROR Lungo da calcolare e inutile
#PLOT DISPERSION MATRIX
#dfdm = data.frame(Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Amt_Chng_Q4_Q1, Total_Trans_Ct, Total_Trans_Amt, Total_Ct_Chng_Q4_Q1, Avg_Utilization_Ratio, Income)
#ggpairs(dfdm, aes(alpha=0.4))

#PLOT CORRELATION MATRIX
BankData %>% 
  dplyr::select(Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Amt_Chng_Q4_Q1, Total_Trans_Ct, Total_Trans_Amt, Total_Ct_Chng_Q4_Q1, Avg_Utilization_Ratio, Income) %>%
  ggcorr(label=TRUE)

# Code for table formatting (markdown output)
BankData %>% 
  kableExtra::kbl() %>%
  kableExtra::kable_paper(full_width = TRUE) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "condensed", "responsive")) %>%
  kableExtra::scroll_box(width = "700px", height = "500px")

#PLOT BOXPLOTS
# In this case we isolate the numerical variables (it does not make sense to 
# plot boxplots of categorical).
BankData_Numeric = BankData %>%
  dplyr::select(where(is.numeric))

BankData_Numeric = setDT(BankData_Numeric)
melt_numeric = melt(BankData_Numeric)
p = ggplot(melt_numeric, aes(factor(variable), value, color="Red"))
k = p + geom_boxplot() + theme(legend.position="none") + rremove('x.text') +  
  rremove('xylab') + 
  facet_wrap(~variable, scale="free")
k

########################################################
##POINT 3
########################################################

# We fit the logistic regression model. 
log_mod_gender = glm (Closed_Account ~ Gender, family='binomial', data = BankData)
summary(log_mod_gender)
# Let's see the posterior probabilities from the training set.
head(log_mod_gender$fitted.values)

BankData %>%
  ggplot(aes(x = Gender, col = 'red')) +
  labs(y = 'Clients') +
  geom_bar()


BankData %>%
  ggplot(aes(x = Gender, y = Closed_Account, col = 'red')) +
  labs(y = 'Closed Account') +
  geom_bar(stat = 'identity')


# We computed two plots, the first one to compare the proportion of males to females
# in the training set, the second one to check if there was a insightful difference in the number of closed accounts
# between the two classes. We can see from the plots that the MLE computation should work fine here,
# since the closed accounts cases are not strictly isolated in one of the two gender classes. 


# We use simple conditional subsetting in order to avoid columns we don't need here

logit.fit = glm(Closed_Account~., family = 'binomial', data = BankData[,-c(5,6)])
summary(logit.fit)

logit.fit.F = step(glm(Closed_Account~1,family = "binomial",data = BankData[,-c(5,6)]),
                   scope = formula(logit.fit), direction = "forward")

logit.fit.B = step(glm(Closed_Account~.,family = "binomial",data = BankData[,-c(5,6)]),
                   direction = "backward")

logit.fit.BB = step(glm(Closed_Account~1,family = "binomial",data = BankData[,-c(5,6)]),
                    scope = formula(logit.fit), direction = "both")

# Gender is kept by the three methods of progression. Let's try witch BIC

logit.fit.F.BIC = step(glm(Closed_Account~1,family = "binomial",data = BankData[,-c(5,6)]),
                       scope = formula(logit.fit), k = log(nrow(BankData)), direction = "forward")
logit.fit.B.BIC = step(glm(Closed_Account~.,family = "binomial",data = BankData[,-c(5,6)]),
                       k =log(nrow(BankData)), direction = "backward")
logit.fit.BB.BIC = step(glm(Closed_Account~1,family = "binomial",data = BankData[,-c(5,6)]),
                        k =log(nrow(BankData)), scope = formula(logit.fit),direction = "both")

# It is kept even by BIC, hence it seems significant for the overall prediction.
# Even after having seen all of this, we can't say - looking at the posterior probabilities
# that come from the logistic regression. It is useless, if we computed a roc we would get
# an AUC very similar to random choice. 


# Now we compute confidence intervals for the coefficients estimated in the logistic regression
# on the training data 

confint(log_mod_gender) #95% is the default value for the function

#From the correlation matrix we can well suppose that Income has no effect at all on the
#probability of closing an account

train = BankData[1:4300,]
test = BankData[4301:5301,]


mod = glm(formula = Closed_Account ~ Income + Gender, 
          family = binomial(link="logit"),
          data = train)

summary(mod)
#it seems what we supposed about Income is true; conversely, the same may not be 
#true for Gender

#make predictions
p_pred = predict(mod, test, type = "response")
test$p_pred = p_pred


# plot results
test %>%
  ggplot(aes(x=Income, y=as.numeric(Closed_Account), group=Gender)) + 
  geom_point(aes(col=Gender), alpha=0.5) + 
  geom_line(aes(y=p_pred, col=Gender), data=test)
#such logistic lines only confirm what we expected: income does not consistently affect
#the probability of closing an account

test_mod_pred = rep(0, nrow(test))
test_mod_pred[p_pred>0.15]=1

1-mean(test_mod_pred==test$Closed_Account) #overall test error rate

table(test_mod_pred,test$Closed_Account)#confusion matrix

tab_rates_test = prop.table(table(test_mod_pred,test$Closed_Account),margin = 2)  #test

ROC=roc(test$Closed_Account,test_mod_pred,plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)

#AUC=0.541 for threshold=0.15; the two predictors come with low accuracy and an overall
#error rate close to 0.5
#This might be considered as a baseline for future improvements

########################################################
## POINT 4
########################################################

#Classification models using only two predictors: Total_Trans_Ct and Total_Trans_Amt
#Having two distinct continuous predictors may be problematic for result visualization, hence
#we reduce data dimension by applying PCA in order to have a single-dimension predictor,
#for all the three models

reduced = data.frame(Total_Trans_Ct, Total_Trans_Amt)

reduced = prcomp(reduced, scale=TRUE) #scale=TRUE, since the dataset comes with different scales and has not been scaled so far
fviz_eig(reduced) #almost 100% of variability caught by the first principal component
reduced = data.frame(reduced[["x"]])

data = data.frame(reduced$PC1, reduced$PC2, BankData$Closed_Account)

#ERROR correlation matrix
data %>% 
  dplyr::select(BankData.Closed_Account, reduced.PC1, reduced.PC2) %>%
  ggcorr(label=TRUE)

#visualize data points
pl1 = data %>%
  ggplot(aes(x=reduced.PC1, y=BankData.Closed_Account)) + 
  geom_point()

pl2 = data %>%
  ggplot(aes(x=reduced.PC2, y=BankData.Closed_Account)) + 
  geom_point()

pl1 / pl2

#split into training and test set
df_train = data[1:4300,]
df_test = data[4301:5301,]


########################################################
## Model 1: k-NN
########################################################

#visualize all the data
data %>%
  ggplot(aes(x=reduced.PC1, y=reduced.PC2)) + 
  geom_point(aes(col=as.factor(BankData.Closed_Account))) + 
  scale_color_manual(values = c("#481568FF", "#FDE725FF"))+ 
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

optimal_k[[1]] #list of values of K, in descending order of associated AUC score
k = optimal_k[[1]][1] #optimal number of neighbors
#NOTE: the optimal number of neighbors may vary; anyway, even with variations it is
#always a very good option (usually, among the first 3/4 best Ks)
#by running the previous lines of code we can fine-tune the parameter; k=17 is 
#always in the first positions, yielding highest overall AUC score compared to other options


#apply k-NN with the optimal number of neighbors
#k=17
df_pred = copy(df_test)
df_pred$class_k17 = knn(train=df_train[,1:2], test = df_test[,1:2], k=17, cl=df_train$BankData.Closed_Account)


# plot predicted regions
p_k17 = 
  df_pred %>%
  ggplot(aes(x=reduced.PC1, y=reduced.PC2))+
  geom_point(aes(col=class_k17), alpha=0.9, pch = 4, size=0.8) + 
  #geom_path(data=df_bay, mapping=aes(x=x, y=y)) + 
  scale_color_manual("K=1", values = c("blue", "red"))+
  coord_fixed() +
  labs(x=quote(X[1]),
       y=quote(X[2]))
p_k17

#compare predictions (on test set) with test set real values
p_k17 + 
  new_scale_color() + 
  geom_point(data=df_test, mapping=aes(x=reduced.PC1, y=reduced.PC2, col=as.factor(BankData.Closed_Account))) + 
  scale_color_manual("Test", values = c("#481568FF", "#FDE725FF"))



df_pred %>%
  summarise(
    err = mean(BankData.Closed_Account != class_k17)
  )

table(df_pred$BankData.Closed_Account, df_pred$class_k17)


ROC=roc(df_test$BankData.Closed_Account,as.numeric(df_pred$class_k17),plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)
#AUC fluctuates around 0.79 with k=17, which seems to be the best choice for this parameter
#even if there are some fluctuations



########################################################
## Model 2: Discriminant Analysis (Discriminant Analysis)
########################################################

#fit the model with the training set
lda_model = lda(BankData.Closed_Account ~ reduced.PC1 + reduced.PC2, data=df_train)
lda_model

# predict test
test_pred = predict(lda_model, df_test[, 1:2])$class
#df_test$class_lda = test_pred

# plot predicted regions
p_lda = 
  df_test %>%
  ggplot(aes(x=reduced.PC1, y=reduced.PC2)) + 
  geom_point(aes(col=test_pred), alpha=1, pch = 4, size=6) + 
  scale_color_manual("Test fitted values", values = c("blue", "red"))+
  coord_fixed() +
  labs(x=quote(X[1]),
       y=quote(X[2]))

p_lda

# visualize test data and predictions
p_lda + 
  new_scale_color() + 
  geom_point(data=df_test, mapping=aes(x=reduced.PC1, y=reduced.PC2, col=as.factor(BankData.Closed_Account))) + 
  scale_color_manual("True", values = c("blue", "red"))

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
qda_model = qda(BankData.Closed_Account ~ reduced.PC1 + reduced.PC2, data=df_train)
qda_model

# predict test
test_pred = predict(qda_model, df_test[, 1:2])$class
#df_test$class_lda = test_pred

# plot predicted regions
p_qda = 
  df_test %>%
  ggplot(aes(x=reduced.PC1, y=reduced.PC2)) + 
  geom_point(aes(col=test_pred), alpha=1, pch = 4, size=6) + 
  scale_color_manual("Test fitted values", values = c("blue", "red"))+
  coord_fixed() +
  labs(x=quote(X[1]),
       y=quote(X[2]))

p_qda

# visualize test data and predictions
p_qda + 
  new_scale_color() + 
  geom_point(data=df_test, mapping=aes(x=reduced.PC1, y=reduced.PC2, col=as.factor(BankData.Closed_Account))) + 
  scale_color_manual("True", values = c("blue", "red"))

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

df_pred %>%
  summarise(
    err = mean(Closed_Account != class_k5)
  )

table(df_pred$Closed_Account, df_pred$class_k5)

ROC=roc(test_set$Closed_Account,as.numeric(df_pred$class_k5),plot = TRUE,
        legacy.axes=TRUE,col="midnightblue",lwd=3,
        auc.polygon=T,auc.polygon.col="lightblue",print.auc=T)
#AUC: 0.837


########################################################
## LOGISTIC REGRESSION
########################################################

##prepare true test set
test2 <- read.csv('bank_accounts_test.csv', stringsAsFactors = T)

test2 = test2 %>%
  filter(!duplicated(.)) %>%
  dplyr::select(-1) %>%
  mutate(Education_Level = na_if(Education_Level, 'Unknown')) %>%
  mutate(Marital_Status = na_if(Marital_Status, 'Unknown'))


##incorporate true test set into main dataframe
BankDatac = copy(BankData)
BankDatac = BankDatac[,1:19]
union = union(x=BankDatac, y=test2)

Customer_Age = union$Customer_Age
Dependent_count = union$Dependent_count
Months_on_book = union$Months_on_book
Total_Relationship_Count = union$Total_Relationship_Count
Months_Inactive_12_mon = union$Months_Inactive_12_mon
Contacts_Count_12_mon = union$Contacts_Count_12_mon
Credit_Limit = union$Credit_Limit
Total_Revolving_Bal = union$Total_Revolving_Bal
Avg_Open_To_Buy = union$Avg_Open_To_Buy
Total_Amt_Chng_Q4_Q1 = union$Total_Amt_Chng_Q4_Q1
Total_Trans_Ct = union$Total_Trans_Ct
Total_Trans_Amt = union$Total_Trans_Amt
Total_Ct_Chng_Q4_Q1 = union$Total_Ct_Chng_Q4_Q1
Avg_Utilization_Ratio = union$Avg_Utilization_Ratio
Income = union$Income
Closed_Account = union$Closed_Account

predictors = data.frame(Avg_Utilization_Ratio, Total_Ct_Chng_Q4_Q1, Total_Amt_Chng_Q4_Q1, Total_Revolving_Bal, Contacts_Count_12_mon, Months_Inactive_12_mon, Total_Relationship_Count, Total_Trans_Ct, Total_Trans_Amt)
##

#Since we are dealing with a response which is not balanced (the proportion of class0 and class1 is 5:1),
#we could use a weighted logistic regression to deal with this problem (rather than optimizing
#the threshold)
#outcome = training_set$Closed_Account
#weighted <- as.data.frame(cbind(outcome))
#weighted = weighted %>%
#mutate(weights = ifelse(outcome==1,5,1))

##with PCA
PCA = prcomp(predictors, scale=TRUE) #scale=TRUE, since the dataset comes with different scales and has not been scaled so far
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

#model with PCA
mod = glm(formula = Closed_Account ~ poly(PC1,9)+poly(PC2,9)+poly(PC3,2)+poly(PC4,7)+poly(PC5,5)+poly(PC6,2)+poly(PC7,2)+poly(PC8,5)+poly(PC9,5)+Gender+ChangesRatio+poly(AmtChTr,5)+poly(CtChTr,5)+poly(RevolvingOverRelationship,4), 
          family = binomial(link="logit"),
          data = training_set)

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

###POINT 10
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

