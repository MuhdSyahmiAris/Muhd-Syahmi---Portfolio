install.packages("tidyverse")
install.packages("lubridate")
install.packages("caret")
install.packages("randomForest")
install.packages("pROC")
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(pROC)

df <- read.csv("Loan.csv")
glimpse(df)

df <- df %>% 
  mutate(ApplicationDate = ymd(ApplicationDate))

categorical_cols <- c("EmploymentStatus", "EducationLevel", "MaritalStatus","HomeOwnershipStatus", "LoanPurpose")
df[categorical_cols] <- lapply(df[categorical_cols], as.factor)

df$LoanApproved <- as.factor(df$LoanApproved)
summary(select(df, Age, AnnualIncome, CreditScore, LoanAmount))

ggplot(df, aes(CreditScore)) +geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of Credit Scores")

ggplot(df, aes(LoanApproved)) + geom_bar(fill = c("#ef4444", "#22c55e")) +
  labs(title = "Loan Approval Distribution")

set.seed(123)
index <- createDataPartition(df$LoanApproved, p = 0.8, list = FALSE)
train <- df[index, ]
test  <- df[-index, ]

# Random forest model 
set.seed(123)
rf_model <- randomForest(
  LoanApproved ~ .,
  data = train,
  importance = TRUE,
  ntree = 300
)
rf_model
# model prediction
rf_pred <- predict(rf_model, test)
confusionMatrix(rf_pred, test$LoanApproved)

# model evaluation 
cm <- confusionMatrix(rf_pred, test$LoanApproved)
cm$overall["Accuracy"]
cm$byClass[c("Precision", "Recall", "F1")]

