params <-
list(handout = TRUE)

library(tidyverse)
library(broom)
library(rsample) # data splitting
library(glmnet) # LASSO estimation
library(mlbench) # BostonHousing dataset
library(pROC) # ROC curves

# Load data 
data(BostonHousing)
BostonHousing %>% select(medv, 1:11) %>% head

# Split Sample
set.seed(1)
split <- initial_split(BostonHousing, prop = 0.7)
boston_train <- training(split)
boston_test <- testing(split)

# Define model formula
medv_formula <- medv ~ crim + zn + indus + chas + nox + 
  rm + age + dis + rad + tax + ptratio + b + lstat

# OLS -- Regress median housing value on all other variables
fit_ols <- lm(medv_formula, boston_train)

# Extract independent variables in training set + remove column 1
X_train <- model.matrix(medv_formula, boston_train) 
X_train <- X_train[,-1]

# While we're here... extract independent variables for test set
X_test <- model.matrix(medv_formula, boston_test) 
X_test <- X_test[,-1]

# Estimate LASSO (choosing lambda by cross-validation)
fit_lasso <- cv.glmnet(X_train, boston_train$medv)

# Save LASSO penalties and performance
cv_lasso_values <- fit_lasso %>% 
  tidy() %>% 
  select(lambda, cv_mse = estimate)

head(cv_lasso_values)

plot(fit_lasso)

tibble(term = names(fit_ols$coefficients),
       ols_coef = coef(fit_ols),
       lasso_coef = coef(fit_lasso, s = "lambda.min")[, 1])

# Get predictions ------------- #
# Training set
boston_train <- mutate(boston_train,
                       pred_ols = predict(fit_ols, boston_train),
                       pred_lasso = predict(fit_lasso, X_train,
                                            s = "lambda.min"))

# Test set
boston_test <- mutate(boston_test,
                      pred_ols = predict(fit_ols, boston_test),
                      pred_lasso = predict(fit_lasso, X_test, 
                                           s = "lambda.min"))

# Training Set
boston_train %>% 
  summarize(mse_ols = mean((medv - pred_ols)^2),
            mse_lasso = mean((medv - pred_lasso)^2))

# Test Set
boston_test %>% 
  summarize(mse_ols = mean((medv - pred_ols)^2),
            mse_lasso = mean((medv - pred_lasso)^2))


# Add binary indicator for exposure to new tax
boston_train <- mutate(boston_train, 
                       newtax = ifelse(medv >= 35, 1, 0))
boston_test <- mutate(boston_test, 
                      newtax = ifelse(medv >= 35, 1, 0))

# Define formula
newtax_formula <- newtax ~ crim + zn + indus + chas + nox + 
  rm + age + dis + rad + tax + ptratio + b + lstat

# Logit
fit_logit <- glm(newtax_formula, boston_train, 
                 family = "binomial")

# LASSO-Logit
fit_lassologit <- cv.glmnet(X_train, boston_train$newtax, 
                            family = "binomial")

# Calculate predicted probabilities ------------ #
# Training set
boston_train <- mutate(boston_train,
                       prob_logit = predict(fit_logit, boston_train, 
                                            type = "response"),
                       prob_lassologit = predict(fit_lassologit, X_train, 
                                                 type = "response", 
                                                 s = "lambda.min")[, 1])
boston_test <- mutate(boston_test,
                      prob_logit = predict(fit_logit, boston_test, 
                                           type = "response"),
                      prob_lassologit = predict(fit_lassologit, X_test, 
                                                type = "response", 
                                                s = "lambda.min")[, 1])

# Set cutoffs  ------------ #
boston_train <- boston_train %>% 
  mutate(pred_logit = ifelse(prob_logit > quantile(prob_logit, 0.75),
                             1, 0),
         pred_lassologit = ifelse(prob_lassologit > quantile(prob_lassologit, 0.75),
                                  1, 0))

boston_test <- boston_test %>% 
  mutate(pred_logit = ifelse(prob_logit > quantile(prob_logit, 0.75), 
                             1, 0),
         pred_lassologit = ifelse(prob_lassologit > quantile(prob_lassologit, 0.75), 
                                  1, 0))

# Training set: precision
summarize(boston_train,
          precision_logit = sum(newtax * pred_logit) / sum(pred_logit),
          precision_lassologit = sum(newtax * pred_lassologit) / sum(pred_lassologit))

# Training set: recall
summarize(boston_train,
          recall_logit = sum(pred_logit * newtax) / sum(newtax),
          recall_lassologit = sum(pred_lassologit * newtax) / sum(newtax))

# Test set: precision
summarize(boston_test,
          precision_logit = sum(newtax * pred_logit) / sum(pred_logit),
          precision_lassologit = sum(newtax * pred_lassologit) / sum(pred_lassologit))

# Test set: recall
summarize(boston_test,
          recall_logit = sum(pred_logit * newtax) / sum(newtax),
          recall_lassologit = sum(pred_lassologit * newtax) / sum(newtax))


roc(newtax ~ prob_lassologit, boston_test, plot = TRUE) 
