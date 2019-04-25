# Clear the environment 
rm(list=ls())

library(DescTools)
library(dplyr)
library(ggplot2)
library(aod)
library(ROCR)
library(pROC)
library(Deducer)
library(dplyr)
library(readxl)
library(caret)
library(MASS)

loan <- read.csv(file="//wfs1/users$/junxiong/Desktop/Lending Club/loan.csv", stringsAsFactors = FALSE)
loan_data <- loan
loan_data <- loan_data[,c("grade","sub_grade","term","loan_amnt","issue_d","loan_status","emp_length",
                          "home_ownership", "annual_inc","verification_status","purpose","dti",
                          "delinq_2yrs","addr_state","int_rate", "inq_last_6mths","mths_since_last_delinq",
                          "mths_since_last_record","open_acc","pub_rec","revol_bal","revol_util","total_acc")]

# For the variables mths_since_last_delinq & mths_since_last_record many values are NA so I set them to zero.
loan_data$mths_since_last_delinq[is.na(loan_data$mths_since_last_delinq)] <- 0
loan_data$mths_since_last_record[is.na(loan_data$mths_since_last_record)] <- 0

# Not only those variables show incomplete cases, so I check how many NAs are there and make a complete dataset.
var.has.na <- lapply(loan_data, function(x){any(is.na(x))})
num_na <- which( var.has.na == TRUE )	
per_na <- num_na/dim(loan_data)[1] 
per_na


# Since the amount of NA is low, I eliminate incomplete cases and make a complete dataset
loan_data <- loan_data[complete.cases(loan_data),]


# Descriptive Analysis
# Plot Loan Amounts
amountplot <- ggplot(data=loan_data, aes(loan_data$loan_amnt)) + 
  geom_histogram(breaks=seq(0, 35000, by=1000), 
                 col="black", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="aliceblue", high="royalblue3")+
  labs(title="Loan Amount", x="Amount", y="Number of Loans")
amountplot
# The majority of loans is lower than 20000, the highest frequency is around the 10000 mark.


# Boxplot of loan amount by status
box_status <- ggplot(loan_data, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(title = "Loan amount by status", x = "Loan Status", y = "Amount"))
box_status


# Plot loan status, purpose and grades
Desc(loan_data$loan_status, main = "Loan Status", plotit = TRUE)
Desc(loan_data$purpose, main = "Loan purposes", plotit = TRUE)
Desc(loan_data$grade, main = "Loan grades", plotit = TRUE)

# Comments
# 67.8% of loans are current, 23.4% were fully paid. This leaves 8.8% of other kind of status
# 59.1% of loans were requested for debt consolidation, a form of refinancing, and 23.2% for credit card payments. In total, 82.3% of loans are used to pay other debts
# The majority of loans were graded B or C (28.7% and 27)

# interest rate by grade
# As expected, worse rated loans require higher interest rates
Desc(loan_data$int_rate ~ loan_data$grade,  main = "Interest rate by grade", plotit = TRUE)

# Is a higher annual income associated to better grades?
Desc(loan_data$annual_inc ~ loan_data$grade,  main = "Annual income by grade", plotit = TRUE)
# As income increases, the grade gets better too


# Pre-Regression
#Create a copy of the dataset before starting, as the dataset will be modified
old_loan_data <- loan_data

# Exclude current loans from the analysis
loan_data=as.data.frame(loan_data[loan_data$loan_status!="Current", ])

# In order to prepare the dataset for the GLM, I dichotomize variables. I start with grouping annual income.
limits_inc = quantile(loan_data$annual_inc, seq(0,1,0.1))
labels <- c(0, limits_inc[2:10], "+inf")
labels <- prettyNum(labels, big.mark = ",")
labels <- paste(labels[1:10], labels[2:11], sep = "-")
loan_data$annual_inc <- cut(loan_data$annual_inc, limits_inc, labels = labels, include.lowest = T)
loan_data[,"annual_inc"] <- as.character(loan_data[,"annual_inc"])

# Dichotomize annual_inc: if annual income is higher than 70000 = 1; if lower, = 0. Set Annual_inc to numeric.
loan_data$annual_inc[loan_data$annual_inc == "70,000- 80,000"| loan_data$annual_inc == "80,000- 94,000" | loan_data$annual_inc == "94,000-120,000" | loan_data$annual_inc == "120,000-   +inf" ] <- 1
loan_data$annual_inc[loan_data$annual_inc != 1] <- 0
loan_data$annual_inc <- as.numeric(loan_data$annual_inc)

# Dichotomize home_ownership: if own or have a mortage = 1; otherwise = 0.
loan_data$home_ownership <- as.character(loan_data$home_ownership)
loan_data$home_ownership[loan_data$home_ownership=="OWN" | loan_data$home_ownership=="MORTGAGE"  ] <- 1       
loan_data$home_ownership[loan_data$home_ownership!=1] <- 0

# Dichotomize delinq_2yrs
loan_data$delinq_2yrs <- as.character(loan_data$delinq_2yrs)
loan_data$delinq_2yrs[loan_data$delinq_2yrs=="0"] <- 0
loan_data$delinq_2yrs[loan_data$delinq_2yrs!= 0] <- 1

# Change variable revol_util from % to general numbers.
loan_data[,"revol_util"] <- as.numeric(sub("%", "",loan_data$"revol_util", fixed =TRUE))/100

# Group purpose variable
loan_data$purpose <- as.character(loan_data$purpose)
loan_data$purpose[loan_data$purpose == "car" | loan_data$purpose == "major_purchase" | 
                    loan_data$purpose == "home_improvement"| loan_data$purpose == "credit_card" ] <- 2
loan_data$purpose[loan_data$purpose == "moving" | loan_data$purpose == "small_business" | 
                    loan_data$purpose == "renewable_energy" ] <- 0
loan_data$purpose[loan_data$purpose!= 0 & loan_data$purpose!= 2 ] <- 1
loan_data$purpose <- as.factor(loan_data$purpose)

# Dichotomize the verification status: if Verified = 1 ; otherwise = 0.
loan_data$verification_status = as.character(loan_data$verification_status)
loan_data$verification_status[loan_data$verification_status == "Verified" | loan_data$verification_status == "Source Verified"] = 1
loan_data$verification_status[loan_data$verification_status != 1] = 0
loan_data$verification_status=as.numeric(loan_data$verification_status)

# Change int_rate in general numbers
loan_data[,"int_rate"] <- as.numeric(sub("%", "",loan_data$"int_rate", fixed =TRUE))/100

# Dichotomize Dti
dti_quant <- quantile(loan_data$dti, seq(0, 1, 0.1))
labels = c(0,prettyNum(dti_quant[2:10], big.mark = ","), "+Inf")
labels = paste(labels[1:10],labels[2:11], sep = "-")
loan_data <- mutate(loan_data, dti= cut(loan_data$dti, breaks = dti_quant, labels = factor(labels), include.lowest = T))
loan_data$dti <- as.character(loan_data$dti)
loan_data$dti[loan_data$dti == "0-6.57" | loan_data$dti == "12.13-14.32" | loan_data$dti == "14.32-16.49" ] <- 1
loan_data$dti[loan_data$dti!=1] <- 0

# Dichotomize status: if charged off or default = 1 ; otherwise = 0.
loan_data$loan_status <- as.character(loan_data$loan_status)
loan_data$loan_status[loan_data$loan_status == "Charged Off" | loan_data$loan_status == "Default" ] <- 1
loan_data$loan_status[loan_data$loan_status != 1] <- 0
table(loan_data$loan_status)
PercTable(loan_data$loan_status)

# Specify status as numeric
loan_data$loan_status <- as.numeric(loan_data$loan_status)

# Regression Models Stepwise Regression
log1 <- glm(loan_status ~ loan_amnt + home_ownership + annual_inc
            + verification_status + purpose + dti + delinq_2yrs 
            + int_rate + inq_last_6mths + mths_since_last_delinq 
            + revol_bal + revol_util + total_acc,
            data = loan_data, family = binomial(link= "logit"))
step <- stepAIC(log1, direction="both")
step$anova


# Divide the dataset on training and testing data


perc <- floor((nrow(loan_data)/4)*3)       
loan_data <- loan_data[sample(nrow(loan_data)), ]          
loan_data.train <- loan_data[1:perc, ]              
loan_data.test <- loan_data[(perc+1):nrow(loan_data), ]


# Train the model
fit.log <- glm(loan_status ~ loan_amnt + home_ownership + verification_status + 
                 purpose + dti + delinq_2yrs + int_rate + inq_last_6mths + 
                 mths_since_last_delinq + revol_bal + revol_util + total_acc,
               data=loan_data.train,family = binomial(link= "logit"))
summary(fit.log)

# AUC and ROC curve
fitted.results <- predict(fit.log, newdata = loan_data.test, type = "response")
loan_data.test$prob <- fitted.results
pred <- prediction(loan_data.test$prob,loan_data.test$loan_status)
auc1 <- performance(pred, measure = "auc")
auc1@y.values
# Plot the graph
plot(roc1, colorize = T)
abline(0, 1, col= "black")

# confusion matrix
confusionMatrix(round(loan_data.test$prob), loan_data.test$loan_status)

