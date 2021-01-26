#install.packages('MASS')
library(MASS)
# Read prostate cancer dataset
Data <- read.delim("prostate.data.txt")
training_data_prostrate <- Data[Data$train==T, ]
testing_data_prostrate <- Data[Data$train==F, ] 
train_Data <- training_data_prostrate[,-c(1,11)]
test_Data <- testing_data_prostrate[,-c(1,11)] 

#Q4 a)
#Linear Regression model
Linear_reg <- lm(lpsa ~., data=train_Data)
print(summary(Linear_reg))

# training error
lin_pred_train <- predict(Linear_reg, train_Data)
n_train <- 67
train_error <- sum(((train_Data[,9]) - lin_pred_train)^2) / n_train
print(train_error)

# testing error
lin_pred_test <- predict(Linear_reg, testing_data_prostrate[,-c(1,11)])
n_test <- 30
test_error <- sum(((test_Data[,9]) - lin_pred_test)^2) / n_test
print(test_error)


# Q4 b) BIC Applying forward selection for selecting variables
library(leaps)
forward_sel = regsubsets(train_Data[,-9], train_Data[,9], method="forward")
fwd_summary <- summary(forward_sel)
print(fwd_summary)

coefficient <- coef(forward_sel, id=1:8) # this is regresssion coefficent
print('Regression Coefficeint: ')
print(coefficient)
cat('\n')

# Checking model structure
rss_out <- str(forward_sel)

#training error
train_error_reg <- (forward_sel$rss[-1]) / 67
print('Training Error for the Variable Selection: ')
print(train_error_reg)

# BIC using the formula
bic <- rep(0,8)
for (i in 1:8){
  bic[i] = 67*log(train_error_reg[i]) + log(67)*(1+i)
}
print(bic)

# find the optimal model
optimal_bic <- which.min(bic)
print(bic[optimal_bic])
cat("\n")

# report the set of important variables selected by BIC
imp_var_bic <- coefficient[which.min(bic)]
print('Important Variable selected by BIC')
print(imp_var_bic)
cat("\n")

# use the selected variables to refit the OLS and report TestErr
refit_OLS_bic <- lm(lpsa ~ lcavol+lweight, data=train_Data)

# compute the testing error
lin_pred_test_refit <- predict(refit_OLS_bic, test_Data)
n_test <- 30
test_error_refit_bic <- sum(((test_Data[,9]) - lin_pred_test_refit)^2) / n_test
print('Testing Error for the Refitted OLS BIC: ')
print(test_error_refit_bic)
cat("\n")

# Q4 c) AIC Applying forward selection for selecting variables
# AIC using the formula
aic <- rep(0,8)
for (i in 1:8){
  aic[i] = 67*log(train_error_reg[i]) + 2*(i+1)
}
print("AIC from 8 different models: ")
print(aic)
cat("\n")

# find the optimal model
optimal_aic <- which.min(aic)
print("Optimal AIC: ")
print(optimal_aic)
cat("\n")

# report the set of important variables selected by AIC
imp_var_aic <- coefficient[which.min(aic)]
print('Important Variable selected by AIC')
print(imp_var_aic)
cat("\n")

# use the selected variables to refit the OLS and report TestErr
refit_OLS_aic <- lm(lpsa ~ lcavol+lweight+age+lbph+svi+lcp+pgg45, data=train_Data)

# compute the testing error
lin_pred_test_refit <- predict(refit_OLS_aic, test_Data)
n_test <- 30
test_error_refit_aic <- sum(((test_Data[,9]) - lin_pred_test_refit)^2) / n_test
print('Testing Error for the Refitted OLS AIC: ')
print(test_error_refit_aic)


# Q5 
#install.packages('lars')
library(lars)

data_prostate <- read.delim("prostate.data.txt")
training_data_prostrate <- data_prostate[data_prostate$train==T, ]# Creating training datasets (train=True)
testing_data_prostrate <- data_prostate[data_prostate$train==F, ] # Creating testing datasets (train=False)
train_data_prostate <- training_data_prostrate[,-c(1,11)] # Removing 1st and 11th columns from traing datasets
test_data_prostate <- testing_data_prostrate[,-c(1,11)] # Removing 1st and 11th columns from testing datasets

# a)
lasso_step <- seq(0,1, length=100)
lasso_cv <- cv.lars(as.matrix(train_data_prostate[,-9]), train_data_prostate[,9], K=5, index=lasso_step, mode="fraction")
# plot(lasso.cv)
# summary(lasso.cv)

lasso_min_cv <- which.min(lasso_cv$cv)

## minimum CV rule
bests1 <- lasso_step[lasso_min_cv]
print("Best Lambda: ")
print(bests1)

lasso_model <- lars(as.matrix(train_data_prostate[,-9]), train_data_prostate[,9])
lasso_coef1 <- predict.lars(lasso_model, s=bests1, type="coef", mode="fraction")
print("Regression Coefficeint: ")
print(lasso_coef1)

predict_test_error <- predict.lars(lasso_model, newx=as.matrix(test_data_prostate[,-9]), s=bests1, type="fit", mode="fraction")
test_error_minCV<- mean((predict_test_error$fit - test_data_prostate[,9])^2)
print("Test Error using Minimum CV: ")
print(test_error_minCV)

cat("\n")

#Q-5 b) 
bound <- lasso_cv$cv[lasso_min_cv] + lasso_cv$cv.error[lasso_min_cv]


bests2 <- lasso_step[min(which(lasso_cv$cv<bound))]
print("Best Lambda: ")
print(bests2)

lasso_coef2 <- predict.lars(lasso_model, s=bests2, type="coef", mode="fraction")
print("Regression Coefficient:")
print(lasso_coef2)

predict_test_error_std <- predict.lars(lasso_model, newx=as.matrix(test_data_prostate[,-9]), s=bests2, type="fit", mode="fraction")
test_error_stdCV<- mean((predict_test_error_std$fit - test_data_prostate[,9])^2)
print("Test Error using one-standard deviation CV: ")
print(test_error_stdCV)