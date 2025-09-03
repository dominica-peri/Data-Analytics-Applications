# Load libraries 
library(MASS)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(car)
library(GGally)
library(ROCR)

# load the wine dataset from the rattle package
df = read.csv('/Users/dap45/OneDrive/Desktop/Data Analytics/DA 6813 - Analytics Applications/Assignments/Case Study 1/securebank_marketing_dataset.csv')
df = dplyr::select(df, -Customer_ID) # remove the customer number

# Look at the data
names(df)
str(df)

# Data Preprocessing

df <- na.omit(df)
df <- df |>
  mutate(across(where(is.character), as.factor))
num_vars <- 
  
  # Outlier detection (boxplots)
  
  "Age"

"Gender" 

"Marital_Status"

"Income"

"Account_Balance"

"Credit_Score"

"Loan_Repayment_Status"

"Transactions_Per_Month"

"Digital_Banking_Usage"

"Campaign_Response"   


# Class imbalance
# 75% 0's
# Predicts mostly 1's



# ____________________________________________________________________________________________________


# Check if it is a classification problem
levels(df$Attrition_Flag)

# You can take a look at the relationships between the variables. 
df_num <- dplyr::select_if(df, is.numeric)
M = cor(df_num)
corrplot(M, method = c("number")) # try 'color', 'square', 'ellipse', 'shade', 'color', 'pie'.

df = dplyr::select(df, -Total_Trans_Ct) # highly correlated with Total trans amount
## note the select function from dplyr and MASS conflict. 

pairs(df_num) # quick look at data distributions
ggplot(data = df) +
  geom_bar(mapping = aes(x = Attrition_Flag, fill = Education_Level), position = "dodge")  ## imbalance in data

ggplot(data = df, mapping = aes(x = Attrition_Flag, y = Credit_Limit)) + geom_boxplot() ## Visualizations

# Split data into training and testing samples
# Setting seed locks the random number generator. 
set.seed(1)
tr_ind <- sample(nrow(df),0.8*nrow(df),replace = F) # Setting training sample to be 80% of the data
dftrain <- df[tr_ind,]
dftest <- df[-tr_ind,]

# With logistic regression
m1.log = glm(Attrition_Flag ~ ., data = dftrain, family = binomial)
summary(m1.log) ## look at results
vif(m1.log) # double check multicollinearity

# Predict the responses on the testing data. 
predprob_log <- predict.glm(m1.log, dftest, type = "response")  ## for logit
predclass_log = ifelse(predprob_log >= 0.5, "Existing Customer", "Attrited Customer")



## Resample with more balanced data 
df_ext_cust = df %>% filter(Attrition_Flag == "Existing Customer")
df_att_cust = df %>% filter(Attrition_Flag == "Attrited Customer")
sample_ext_cust = sample_n(df_ext_cust, nrow(df_att_cust))
df_bal = rbind(df_att_cust,sample_ext_cust)

# Split data into training and testing balanced samples
set.seed(1)
tr_ind_bal <- sample(nrow(df_bal),0.8*nrow(df_bal),replace = F) # Setting training sample to be 80% of the data
dftrain_bal <- df_bal[tr_ind_bal,]
dftest_bal <- df_bal[-tr_ind_bal,]

### Build model with balanced data
m1.log_bal = glm(Attrition_Flag ~ ., data = dftrain_bal, family = binomial)
summary(m1.log_bal) ## look at results
vif(m1.log_bal) # double check multicollinearity

# Predict the responses on the balanced testing data. 
predprob_log_bal <- predict.glm(m1.log_bal, dftest_bal, type = "response")  ## for logit
predclass_log_bal = ifelse(predprob_log_bal >= 0.5, "Existing Customer", "Attrited Customer")



# Compare to actual results using the confusion matrix. 
caret::confusionMatrix(as.factor(predclass_log), dftest$Attrition_Flag, positive = "Existing Customer")
caret::confusionMatrix(as.factor(predclass_log_bal), dftest_bal$Attrition_Flag, positive = "Existing Customer")



## variable selection - reduce complexity if the model
m2.log_bal = step(glm(Attrition_Flag ~ ., data = dftrain_bal, family = binomial), direction = "backward") # stepwise backward elim.
summary(m2.log_bal) ## look at results
vif(m2.log_bal) # double check multicollinearity
# Predict the responses on the testing data. 
predprob2_log_bal <- predict.glm(m2.log_bal, dftest_bal, type = "response")  ## for logit
predclass2_log_bal = ifelse(predprob2_log_bal >= 0.5, "Existing Customer", "Attrited Customer")
caret::confusionMatrix(as.factor(predclass2_log_bal), dftest_bal$Attrition_Flag, positive = "Existing Customer")
