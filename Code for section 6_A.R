library(glmnet)
#To fit the model
model <- glmnet(X, Y, alpha = 0,lambda = 0.5)
#plotting the coefficients
plot(coef(model), xlab = 'gene number', ylab = 'Weight of the corresponding gene number')
y_predicted <- predict(model, s = 1278, newx = X)
#calculating the SSR:
ssr <- sum((y_predicted - mean(Y))^2)
#calculating the SST:
sst <- sum((Y - mean(Y))^2)
#calculating the SSE:
sse <- sum((y_predicted - Y)^2)
rse<-1-sse/sst
rse
