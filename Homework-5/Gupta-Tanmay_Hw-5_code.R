#install.packages('e1071')
#install.packages('tree)
library(e1071)
library(tree)
# Hw-5 Q1 

#fuction for normalizing values
normal <- function(inputs) {
  normalized<- ((inputs- min(inputs))/ (max(inputs)-min(inputs))) 
  return(normalized)
}

# Getting the training data
train<- read.table('zip.train.gz')
train_2_3 <- train[train[,1] %in% c(2,3), ]
X_train_1 <- sapply(train_2_3[,-1], normal)
train_data_1 <- cbind(train_2_3[,1], X_train_1)
train_data_1 <- as.data.frame(train_data_1)
train_data_1$V1 <- as.factor(train_data_1$V1)

#Getting the test data
test <-  read.table('zip.test.gz')
test_2_3 <- test[test[,1] %in% c(2,3), ]
x_test_1 <- sapply(test_2_3[,-1], normal)
test_data_1 <- cbind(test_2_3[,1], x_test_1)
test_data_1 <- as.data.frame(test_data_1)
test_data_1$V1 <- as.factor(test_data_1$V1)

# Tuning the parameters
set.seed(123)
linearSVM_tune <- tune(svm, V1~., data=train_data_1, kernel="linear",
                       ranges =list(cost=2^(2:7)))
info <- summary(linearSVM_tune)
#print(info)
best_linear_model <- linearSVM_tune$best.model

#Training the model
linearSVM <- svm(V1~., data=train_data_1, kernel="linear",
                 gamma = best_linear_model$gamma, 
                 cost = best_linear_model$cost)
info_linearSVM <- summary(linearSVM)
print(info_linearSVM)

#predicting and getting training error
linearSVM_pred <- predict(linearSVM, newdata=test_data_1[,-1])
linearSVM_error <- mean(linearSVM_pred != test_data_1[,1])
print('Test error for linear SVM:')
print(linearSVM_error)

#Tuning parameters for Gaussian kernel SVM
set.seed(123)
gaussSVM_tune <- tune(svm, V1~., data=train_data_1, kernel="radial",
                      ranges =list(cost=2^(2:7)))
info_gaussSVM_tune <- summary(gaussSVM_tune)
best_gauss_model <- gaussSVM_tune$best.model

# Training the model
gaussSVM <- svm(V1~., data=train_data_1, kernel="radial",
                gamma = best_gauss_model$gamma, 
                cost = best_gauss_model$cost)
info_gaussSVM <- summary(gaussSVM)
print(info_gaussSVM)

#Predicting and getting test error
gaussSVM_pred <- predict(gaussSVM, newdata=test_data_1[,-1])
gaussSVM_error <- mean(gaussSVM_pred != test_data_1[,1])
print('Test Error for gaussian Kernel SVM: ')
print(gaussSVM_error)


# Tuning parameters for Polynomial kernel SVM
set.seed(123)
polySVM_tune <- tune(svm, V1~., data=train_data_1, kernel="polynomial",
                     ranges = list(degree = c(1,2,3,4)))
info_polySVM_tune <- summary(polySVM_tune)
# print(info_polySVM_tune)
best_poly_model <- polySVM_tune$best.model
# print(best_poly_model)

## Training the model
polySVM <- svm(V1~., data=train_data_1, kernel="poly",
               gamma = best_poly_model$gamma, 
               cost = best_poly_model$cost,
               degree = best_poly_model$degree )
info_polySVM <- summary(polySVM)
print(info_polySVM)

#Predicting and getting the testing error
polySVM_pred <- predict(polySVM, newdata=test_data_1[,-1])
polySVM_error <- mean(polySVM_pred != test_data_1[,1])
print('Test Error for polynomial kernel SVM: ')
print(polySVM_error)

#Q2 
#training data
train_1_2_3 <- train[train[,1] %in% c(1,2,3), ]
X_train <- sapply(train_1_2_3[,-1], normal) # Normalizing training data
train_data <- cbind(train_1_2_3[,1], X_train)
train_data <- as.data.frame(train_data)
train_data$V1 <- as.factor(train_data$V1)

test_data_1_2_3 <- test[test[,1] %in% c(1,2,3), ]
X_test <- sapply(test_data_1_2_3[,-1], normal) # Normalizing testing data
test_data <- cbind(test_data_1_2_3[,1], X_test)
test_data <- as.data.frame(test_data)
test_data$V1 <- as.factor(test_data$V1)


## Classification Tree ##

## fit the tree with split="deviance
set.seed(1)
tree1 <- tree(V1~. , data=train_data, split='deviance')
info1 <- summary(tree1)

# compute the training error
train_pred1 <- predict(tree1, train_data[-1], type="class")
train_err <- mean(train_pred1 != train_data[,1])
print('Training Error for Classification Tree:')
print(train_err)


# compute the testing error
test_pred1 <- predict(tree1, test_data[,-1], type="class")
test_err <- mean(test_pred1 != test_data[,1])
print('Testing Error for CLassification Tree:')
print(test_err)