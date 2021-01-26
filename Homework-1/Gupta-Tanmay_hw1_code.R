# Install the package if not installed in R
#install.packages("MASS")
library(MASS)
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
# Q4 part ii (scatterplot)
class_1<- "green"
class_2<- "red"
plot_1<- plot(train_model,main = "Training Data",col=c(class_1,class_2),xlab = "Col_1",ylab = "Col_2")
plot_1
# Q4 part iii (test set)
no_test_set<-500
set.seed(2019)
test_model<- rbind(mvrnorm(no_test_set,mean_1,sigma),
               mvrnorm(no_test_set,mean_2,sigma))
test_y<- c(rep(1,no_test_set),rep(0,no_test_set))
#Q5 part ii (adding bayes decision boundary)
plot_2<-plot(train_model,main = "Training Data",col=c(class_1,class_2),xlab = "Col_1",ylab = "Col_2") +abline(train_model[,2]>train_model[,1],train_model[,2]<train_model[,1])
plot_2
#Q5 part iii 
#Computing train and test error for bayes classifier
training <- 1*(train_model[,1]>train_model[,2])
bayes_train_error <- mean(training!=train_y)
testing <- 1*(test_model[,1]>test_model[,2])
bayes_test_error <- mean(testing!=test_y)
bayes_train_error
bayes_test_error