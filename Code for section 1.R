#creating data:
example <- data.frame(length = c(160, 170, 180, 165,177, 172), weight = c(66, 70, 76, 70, 75,72))
#plotting:
plot(weight~length, data= example,xlim = c(150,200), ylim = c(60,80), xlab = 'length(cm)', ylab = 'weight(kg)')
#training model on train set
train <- data.frame(length_train = c(170,172), weight_train = c(70,72))
model<-lm(weight_train ~length_train, data = train)
abline(model, col = 2)
