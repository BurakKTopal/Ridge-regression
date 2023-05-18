#taking data from section 1:
example<- data.frame(length = c(160, 170, 180, 165, 177, 172), weight = c(66, 70, 76, 70, 75, 72))
#plotting, making the train set of one point in red:
plot(weight~length, data = example, xlim = c(150, 200), ylim = c(60, 80), col = ifelse(length == 170, 2, 1), xlab = 'length(cm)', ylab = 'weight(kg)')
#defining the necessary matrices to calculate beta:
Y<- matrix(nrow = 1, ncol = 1)
Y[1,1] <- 70
X<-matrix(nrow = 1, ncol = 1)
X[1,1] <- 170
A<- matrix(nrow = 1, ncol = 1)
A[,1] <- 1
X <- cbind(A,X)
#setting Ridge parameter to one:
lambda <- 1
#identity matrix:
I <- matrix(0, 2, 2)
diag(I) <- 1
#calculating beta:
beta <- inv(t(X)%*%X + lambda*I)%*%t(X)%*%Y
abline(c(beta[1], beta[2]), col = 2)
