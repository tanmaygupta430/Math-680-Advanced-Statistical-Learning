library(dplyr)
library(MASS)
#Getting data from Hw-1 Q-4 to be used in Hw-2 Q3 --

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

## Linear Regression model on training data for HW-2 Q3 a) 
linear_reg<- lm(train_y~train_model)
summary(linear_reg)
intercept<- linear_reg$coefficients[1]
slope<- linear_reg$coefficients[-1]
#training error-
linear_reg_training =  as.vector(1*((intercept + train_model%*%slope)>0.5))
linear_reg_training_error = mean(linear_reg_training != train_y)
print(linear_reg_training_error)

#testing error
linear_reg_testing =  as.vector(1*((intercept + test_model%*%slope)>0.5))
linear_pred_test_error = mean(linear_reg_testing != test_y)
print(linear_pred_test_error)

# LDA model on training data for hw-2 Q3 b)
lda_train_data<- cbind(train_model,train_y)
lda_train<- as.data.frame(lda_train_data)
lda_model <- lda(train_y ~ V1+V2, data=lda_train)
summary(lda_model)

# Training Error
table(lda_train[,3])

lda_training <- predict(lda_model, lda_train)$class
table(lda_training, lda_train[,3])
lda_training_error <- mean(lda_training != lda_train[,3])
print(lda_training_error)

# Testing Error
lda_test_data<- cbind(test_model,test_y)
lda_test<- as.data.frame(lda_test_data)

#table(lda_test[,3])
lda_testing <- predict(lda_model, lda_test)$class
table(lda_testing, lda_test[,3])
lda_testing_error = mean(lda_testing != lda_test[,3])
print(lda_testing_error)

# Logitic Regression model on training data for hw-2 Q3 c)
log_training = cbind(train_model, train_y)
log_training_data = as.data.frame(log_training)
table(log_training_data[,3])

#Logistic Regression Model
log_model = glm(train_y ~ V1+V2, data=log_training_data, family='binomial')
summary(log_model)

# Training Error
log_train = predict(log_model, log_training_data)
log_v = ifelse(log_train > 0, '1','0')
y_new = cbind(log_training_data, log_v)
log_training_error = mean(y_new[,4] != log_training_data[,3])
print(log_training_error)

# Testing Error
log_testing = cbind(test_model, test_y)
log_testing_data = as.data.frame(log_testing)
table(log_testing_data[,3])

log_test = predict(log_model, log_testing_data)
log_testing_data = log_testing_data %>% mutate(log_v = ifelse(log_test > 0, '1','0'))
log_testing_error = nrow(subset(log_testing_data,log_v!=log_testing_data[,3]))/nrow(log_testing_data)
print(log_testing_error)

#Q4 a) Generating a training set with given code

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

training_set<- cbind(train2,ytrain)
save(training_set,file= "training_set_2.RData")

#Drawing a scatterplot of the training set for Q4 b)
class_1<- "green"
class_2<- "red"

plot_1<- plot(train2[1:100, ], col="green", xlim=c(-2,4), ylim=c(-2,4), xlab = "Col_1", ylab = "Col_2", main=" Training Data")+
points(train2[101:200, ], col="red") 

#Generating testing set for Q4 c)
seed_testing <- 2014
ntesting <- 500
test_2 <- gendata2(ntesting,center_green,center_red,
                   Sig/5, Sig/5,seed_testing)
test_y_2 <- c(rep(1,ntesting),rep(0,ntesting))

test_set<- cbind(test_2,test_y_2)
testing_set<- as.data.frame(test_set)
#Saving test set for fututre use
save(testing_set,file= "test_set_2.RData")

# Q-5 a) fitting a linear regression model on the data generated in Q-4
linear_reg_model<- lm(ytrain~train2)
summary(linear_reg_model)
intercept_linear_model<- linear_reg_model$coefficients[1]
slope_linear_model<- linear_reg_model$coefficients[-1]

# Q-5 b) Adding the linear decision boundary to the plot 
plot_2<- plot(train2[1:100, ], col="green", xlim=c(-2,4), ylim=c(-2,4), xlab = "Col_1", ylab = "Col_2", main=" Training Data")+
  points(train2[101:200, ], col="red") 
abline(coef = c((intercept_linear_model-0.5)/(-slope_linear_model[2]),slope_linear_model[1]/(-slope_linear_model[2])), col=1,lwd=1)

# Q-5 c) Computing the training and test error
#training error 
lin_reg_train =  ifelse(as.vector(1*((intercept_linear_model + train2 %*% slope_linear_model)>0.5)),'1','0')
lin_reg_training_error = mean(lin_reg_train != ytrain)
print(lin_reg_training_error)

# Testinng Error
lin_reg_test =  ifelse(as.vector(1*((intercept_linear_model + test_2 %*% slope_linear_model)>0.5)),'1','0')
lin_reg_test_error = mean(lin_reg_test != test_y_2)
print(lin_reg_test_error)
