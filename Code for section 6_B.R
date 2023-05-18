library(glmnet)
library(tidyverse)
library(caret)
#setting seed:
set.seed(08052023)
test_index <- 1:dim(X)[1]
#dividing the test_index into 10 subsets:
folds_index<-createFolds(test_index, k = 10, list = TRUE, returnTrain = FALSE)
#creating the for loop to make the different partitions:
residualMatrix <- matrix(c(0,0), nrow = 1, byrow = TRUE)
colnames(residualMatrix)<- c('lambda','residual')
for (lambda in 1:5000){
  #going in steps of 0.1:
  lambda<- lambda/10
  for (j in 1:10){
    #defining the sub-test set:
    #the 5 subsets formed in line 17 are named Fold1, Fold2,.., Fold5
    #choosing the j-th partition:
    #for partition of the associated X data set:
    subtest_set_x <- (X[test_index[folds_index[j][[1]]], ])
    #the index for the train set, is formed by the removing the elements from the indices of the subtest set out of the index set:
    subtest_index <- (setdiff(x = test_index, y = test_index[folds_index[j][[1]]]))
    subtrain_set_x <- X[subtest_index,]
    #partitioning the associated Y-data set:
    subtest_set_y <- (Y[test_index[folds_index[j][[1]]]])
    subtrain_set_y <- matrix(Y[subtest_index])
    #calculating the beta parameters using the train set:
    model <- glmnet(subtrain_set_x, subtrain_set_y, alpha = 0,lambda = lambda)
    #calculating the residual using the predict function on the test set:
    y_hat_j <- predict(model, s = lambda, newx = subtest_set_x)
    #calculating the residual:
    residual_j <- (sum((subtest_set_y -  y_hat_j)^2))/length(y_hat_j)
  }
  row <- cbind(lambda, residual_j)
  residualMatrix<- rbind(residualMatrix, row)
}
#removing the default row:
residualMatrix <- residualMatrix[- 1,]
#Defining LAMDA(lambda) as a data frame:
LAMBDA <- data.frame(residualMatrix)
plot(LAMBDA$residual~LAMBDA$lambda, log = 'x', xlab = 'log(LAMBDA)', ylab = 'lambda')
lambda_optimal <- LAMBDA$lambda[which.min(LAMBDA$residual)]
#Showing the minimum:
abline(v = LAMBDA$lambda[which.min(LAMBDA$residual)],col = 2)
