#install.packages('MASS')
#install.packages('class')

library(MASS)
library(class)

#Q3 (exercise 2.8 from textbook)
#getting data 
training_data <- read.table("zip.train.gz")
training_data <- as.data.frame(training_data) 
training_2_3 <- training_data[training_data[,1] %in% c(2,3), ]
testing_data <-  read.table('zip.test.gz')
testing_data <- as.data.frame(testing_data) 
testing_2_3 <- testing_data[testing_data[,1] %in% c(2,3), ]


# for linear regression
linear_Reg <- lm(V1 ~ ., data = training_2_3)

#function for value between 2 and 3
func <- function(x){
  ifelse((x>=2.5), '3','2')
}

# Training Error
linear_reg_train <- func(predict(linear_Reg, training_2_3)) 
linear_reg_train_error <- mean(linear_reg_train != training_2_3[,1])
print(linear_reg_train_error*100)

## Testing Error
lin_reg_test <-  func(predict(linear_Reg, testing_2_3))
linear_reg_test_error <- mean(lin_reg_test != testing_2_3[,1])
print(linear_reg_test_error*100)


#for KNN
#Normalize function
data_norm <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

# Normailizing train data and test data 
norm_train_data_2_3 <- sapply(training_2_3[,-1], data_norm)
norm_test_data_2_3 <- sapply(testing_2_3[,-1], data_norm)

## Function for knn training error
knn_func_training <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_data_2_3_only[,1], k=k, prob=F)
  knn_error_train <- mean(knn_fit != train_data_2_3_only[,1])
}
## Function for knn testing error
knn_func_testing <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=training_2_3[,1], k=k, prob=F)
  knn_error_test <- mean(knn_fit != testing_2_3[,1])
}


# KNN Algorithm for K = 1, 3, 5, 7, 15
for (i in c(1, 3, 5, 7, 15)) {
  knn_train_error <- knn_func_training(norm_train_data_2_3[,-1], norm_train_data_2_3[,-1], i)
  knn_test_error <- knn_func_testing(norm_train_data_2_3[,-1], norm_test_data_2_3[,-1], i)
  train_test_error <- sprintf(paste('For k=%2d: ', 'train error is %.3f &', 'test error is %.3f'),
                              i, knn_train_error*100, knn_test_error*100)
  
  print(train_test_error)
  
}

#Q4 a)
#Getting data from Hw-1 Q-4 to be used in Hw-3 Q4 

#pre defining numbers and parameters
mean_1<- c(2,1)
mean_2<- c(1,2)
sigma<- matrix(c(1,0,0,1),nrow = 2)
#Q-4 part i (Train set)
no_train_set<- 100
set.seed(2020)
train_model<-rbind(mvrnorm(no_train_set,mean_1,sigma),
                   mvrnorm(no_train_set,mean_2,sigma))
train_y<-c(rep(1,no_train_set),rep(0,no_train_set))
# Q4 part iii (test set)
no_test_set<-500
set.seed(2019)
test_model<- rbind(mvrnorm(no_test_set,mean_1,sigma),
                   mvrnorm(no_test_set,mean_2,sigma))
test_y<- c(rep(1,no_test_set),rep(0,no_test_set))

#Arranging datasets for training and testing
train_set_scen1 <- cbind(train_model, train_y)

test_set_scen1 <- cbind(test_model, test_y)

#KNN function for training error
knn_func_training <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_set_scen1[,3], k=k, prob=F)
  knn_error <- mean(knn_fit != train_set_scen1[,3])
}
# KNN function for testing error 
knn_func_testing <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_set_scen1[,3], k=k, prob=F)
  knn_error <- mean(knn_fit != test_set_scen1[,3])
}

# Analyzing KNN Algorithm with K = 1, 4, 7, 10, 13, 16, 30, 45, 60, 80, 100, 150, 200 
error_vector <- matrix(NA,ncol=3, nrow=13) 
j=0
for (i in c(1, 4, 7, 10, 13, 16, 30, 45, 60, 80, 100, 150, 200)) {
  knn_train_error <- knn_func_training(train_set_scen1[,-3], train_set_scen1[,-3], i)
  knn_test_error <- knn_func_testing(train_set_scen1[,-3], test_set_scen1[,-3], i)
  train_test_error <- sprintf(paste('For k=%2d: ', 'train error percent is %.1f & ', 'test error percent is %.1f'),
                              i, knn_train_error*100, knn_test_error*100)
  print(train_test_error)
  j=j+1;
  error_vector[j, 1] = i
  error_vector[j, 2] = knn_train_error
  error_vector[j, 3] = knn_test_error
}


# Plot for Scenario 1: with n
plot(error_vector[, 1], error_vector[, 2], type="b", lwd=2, col="red", xlim=c(0,200), ylim=c(0,0.6), xlab = "k-Number of Nearest Neighbours", ylab = "Errors", main="Scenario 1")
lines(error_vector[,1], error_vector[,3], type="b", lwd=2, col="blue")
legend(x=150,y=0.07,legend=c("Train Error","Test Error"),cex=1,col=c("red","blue"),pch=c(1,1), lwd=2)

# Plot for Scenario 1: with n/k
plot(200/error_vector[, 1], error_vector[, 2], type="b", lwd=2, col="red", xlim=c(0,200), ylim=c(0,0.6), xlab = "n/k-Degrees of Freedom", ylab = "Error", main="Plot for n/k DF")
lines(200/error_vector[,1], error_vector[,3], type="b", lwd=2, col="blue")
legend(x=150,y=0.55,legend=c("Train Error","Test Error"),cex=1,col=c("red","blue"),pch=c(1,1), lwd=2)


# Q4 b) 
#taking data from Q4 of Hw-2

#generate ten centers, which are treated
Sig <- matrix(c(1,0,0,1),nrow=2)
seed_center <- 16
set.seed(seed_center)
center_green <- mvrnorm(n=10,c(1,0),Sig)
center_red <- mvrnorm(n=10,c(0,1),Sig)
##define a function "gendata2" first
gendata2 <-function(n,mu1,mu2,Sig1,Sig2,myseed)
{
  set.seed(myseed)
  mean1 <- mu1[sample(1:10,n,replace=T),]
  mean2 <- mu2[sample(1:10,n,replace=T),]
  green <- matrix(0,ncol=2,nrow=n)
  red <- matrix(0,ncol=2,nrow=n)
  for(i in 1:n){
    green[i,] <- mvrnorm(1,mean1[i,],Sig1)
    red[i,] <- mvrnorm(1,mean2[i,],Sig2)
  }
  x <- rbind(green,red)
  return(x)
}

#generate the training set
seed_train <- 2000
ntrain <- 100
train2 <- gendata2(ntrain,center_green,center_red,
                   Sig/5, Sig/5,seed_train)
ytrain <- c(rep(1,ntrain),rep(0,ntrain))

training_set_2<- cbind(train2,ytrain)

#Generating testing set for Q4 c)
seed_testing <- 2014
ntesting <- 500
test_2 <- gendata2(ntesting,center_green,center_red,
                   Sig/5, Sig/5,seed_testing)
test_y_2 <- c(rep(1,ntesting),rep(0,ntesting))

test_set_2<- cbind(test_2,test_y_2)

# KNN function for training error 
knn_func_scen2_training <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=training_set_2[,3], k=k, prob=F)
  knn_error <- mean(knn_fit != training_set_2[,3])
}
# KNN function for testing error ##
knn_func_scen2_testing <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=training_set_2[,3], k=k, prob=F)
  knn_error <- mean(knn_fit != test_set_2[,3])
}

#Analyzing KNN Algorithm with K = 1, 4, 7, 10, 13, 16, 30, 45, 60, 80, 100, 150, 200
error_vector_scen2 <- matrix(NA, ncol=3, nrow=13) # creating empty matrix
k=0
for (i in c(1, 4, 7, 10, 13, 16, 30, 45, 60, 80, 100, 150, 200)) {
  knn_train_error_scen2 <- knn_func_scen2_training(training_set_2[,-3], training_set_2[,-3], i)
  knn_test_error_scen2 <- knn_func_scen2_testing(training_set_2[,-3], test_set_2[,-3], i)
  train_test_error_scen2 <- sprintf(paste('For k=%2d: ', 'train error percent: %.1f  & ', 'test error percent: %.1f'),
                                    i, knn_train_error_scen2*100, knn_test_error_scen2*100)
  
  print(train_test_error_scen2)
  k=k+1;
  error_vector_scen2[k, 1] = i
  error_vector_scen2[k, 2] = knn_train_error_scen2
  error_vector_scen2[k, 3] = knn_test_error_scen2
}


# Plot for Scenario 2: with n
plot(error_vector_scen2[, 1], error_vector_scen2[, 2], type="b", col="red", lwd=2, xlim=c(0,200), ylim=c(0,0.55), xlab = "k-Number of Nearest Neighbours", ylab = "Errors", main="Scenario 2")
points(error_vector_scen2[,1], error_vector_scen2[,3], type="b", col="blue", lwd=2)
legend(x=150,y=0.07,legend=c("Train Error","Test Error"),cex=1,col=c("red","blue"),pch=c(1,1), lwd=2)

# Plot for Scenario 2: with n/k
plot(200/error_vector_scen2[, 1], error_vector_scen2[, 2], type="b", col="red", lwd=2, xlim=c(0,200), ylim=c(0,0.55), xlab = "n/k-Degrees of Freedom", ylab = "Errors", main="Scenario 2")
points(200/error_vector_scen2[,1], error_vector_scen2[,3], type="b", col="blue", lwd=2)
legend(x=150,y=0.55,legend=c("Train Error","Test Error"),cex=1,col=c("red","blue"),pch=c(1,1), lwd=2)

# Q5 a)

train_zipcode_data = read.table("zip.train.gz")
train_zipcode_data = as.data.frame(train_zipcode_data) # We do not have cbind and rbind
train_data_1_2_3_only = train_zipcode_data[train_zipcode_data[,1] %in% c(1,2,3), ]
test_zipcode_data =  read.table('zip.test.gz')
test_zipcode_data = as.data.frame(test_zipcode_data) # We do not have cbind and rbind
test_data_1_2_3_only = test_zipcode_data[test_zipcode_data[,1] %in% c(1,2,3), ]

data_norm <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

# Normailizing train data and test data ##
norm_train_data_1_2_3 <- sapply(train_data_1_2_3_only[,-1], data_norm)
norm_test_data_1_2_3 <- sapply(test_data_1_2_3_only[,-1], data_norm)

# KNN function for trainng error
knn_func_training <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_data_1_2_3_only[,1], k=k, prob=F)
  knn_error_train <- mean(knn_fit != train_data_1_2_3_only[,1])
}
# KNN function for testing error ##
knn_func_testing <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_data_1_2_3_only[,1], k=k, prob=F)
  knn_error_test <- mean(knn_fit != test_data_1_2_3_only[,1])
}

# Analyzing KNN Algorithm with K = 1, 3, 5, 7, 15 ##
for (i in c(1, 3, 5, 7, 15)) {
  knn_train_error <- knn_func_training(norm_train_data_1_2_3[,-1], norm_train_data_1_2_3[,-1], i)
  knn_test_error <- knn_func_testing(norm_train_data_1_2_3[,-1], norm_test_data_1_2_3[,-1], i)
  train_test_error <- sprintf(paste('For k=%2d: ', 'train error percent is %.2f &', 'test error percent is %.2f'),
                              i, knn_train_error*100, knn_test_error*100)
  print(train_test_error)
}

# b) Implementing LDA

#Removing first 16 columns from training and testing data
train_out_data_1_2_3_only = train_data_1_2_3_only[, 18:257]
test_out_data_1_2_3_only= test_data_1_2_3_only[, 18:257]

# Training LDA model with training data
lda_model <- lda(train_out_data_1_2_3_only, grouping=train_data_1_2_3_only[,1])
#lda_model <- lda(V1~., data=train_out_data_1_2_3_only)
summary(lda_model)


# Training Error
y_pred_lda_train <- predict(lda_model, train_out_data_1_2_3_only)$class
#table(y_pred_lda_train, train_out16_data_2_3_only[,1])
lda_training_erros <- mean(y_pred_lda_train != train_data_1_2_3_only[,1])
lda_train_error <- sprintf(paste('LDA train error percent is %.2f'),lda_training_erros*100)
print(lda_train_error)


# Testing Errors
y_pred_lda_test <- predict(lda_model, test_out_data_1_2_3_only)$class
#table(y_pred_lda_test, test_out16_data_2_3_only[,1])
lda_testing_erros <- mean(y_pred_lda_test != test_data_1_2_3_only[,1])
lda_test_error <- sprintf(paste('LDA test error percent is %.2f'),lda_testing_erros*100)
print(lda_test_error)
