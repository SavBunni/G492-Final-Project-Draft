# OPEN AND USE IN RSTUDIO

apple_data <- read.csv("bottom_line_Apple_R.csv", header = TRUE, as.is = TRUE)
options(scipen = 100, digits = 4) 

#Data Slicing
set.seed(1)
train_size <- 0.6 * nrow(apple_data)
train_index <- sample(x = 1:nrow(apple_data), size = train_size)
train_set <- apple_data[train_index, ]
valid_set <- apple_data[-train_index, ]

summary(apple_data)

# Histogram

numerical_vars <- c("Record", "HomeOwners", "Female", "LogProfit", "Household", "NumEmployees", "Income", "Commute", "EmployeeWage", "Rainfall", "LogPrice", "White", "Month", "Education", "Age", "County", "NumBestBuy", "NumKrogers", "Unemployment", "Democrats","Year", "Population")
for (var in names(numerical_vars)) { hist(numerical_vars[[var]], main = paste("Distribution of", var), xlab = var)}

# linear regression

lin_reg_all <- lm(formula = LogProfit ~ ., data = train_set)
summary(lin_reg_all)

lin_reg_model <- lm(formula = LogProfit ~ LogPrice + Income + Commute + Education + Population, data = train_set)
summary(lin_reg_model)

# check overfitting/underfitting

# Using validation data
pred_lin_valid <- predict(object = lin_reg_model, newdata = valid_set) 
errors_lin_valid <- valid_set$LogProfit - pred_lin_valid 
ME_lin_valid <- mean(errors_lin_valid) 
RMSE_lin_valid <- sqrt(mean((errors_lin_valid)^2)) 
c(ME_lin_valid, RMSE_lin_valid)

# Using training data
pred_lin_train <- predict(object = lin_reg_model, newdata = train_set) 
errors_lin_train <- train_set$LogProfit - pred_lin_train 
ME_lin_train <- mean(errors_lin_train) 
RMSE_lin_train <- sqrt(mean((errors_lin_train)^2)) 
c(ME_lin_train, RMSE_lin_train)

# TWFE
install.packages("fixest")
library(fixest)

twfe_model <- feols(LogProfit ~ LogPrice + Income + Commute + Education + Population | Month, data = train_set)
summary(twfe_model)


# check overfitting/underfitting

# Using validation data
pred_twfe_valid <- predict(object = twfe_model, newdata = valid_set) 
errors_twfe_valid <- valid_set$LogProfit - pred_twfe_valid 
ME_twfe_valid <- mean(errors_twfe_valid) 
RMSE_twfe_valid <- sqrt(mean((errors_twfe_valid)^2)) 
c(ME_twfe_valid, RMSE_twfe_valid)

# Using training data
pred_twfe_train <- predict(object = twfe_model, newdata = train_set) 
errors_twfe_train <- train_set$LogProfit - pred_twfe_train 
ME_twfe_train <- mean(errors_twfe_train) 
RMSE_twfe_train <- sqrt(mean((errors_twfe_train)^2)) 
c(ME_twfe_train, RMSE_twfe_train)

# Overfitting %
c(RMSE_twfe_valid, RMSE_twfe_train)

percent_difference = (RMSE_twfe_valid - RMSE_twfe_train) / RMSE_twfe_valid * 100
percent_difference


