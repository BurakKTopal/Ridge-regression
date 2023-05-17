library(matlib)
#to create empty plot:
plot(c(0,0), xlim = c(0,10), ylim = c(0,20), col = 0, ylab = 'y')
#allpoints is to save all the different coordinates per cycle
allpoints<-data.frame()
#"row" will be used to bind the different cycles to each other
row <- c()

for (k in 0:5){
  #to generate the two points with coordinates (1+2*k/5, 2+2*k*2/5) and (8-2*k/5, 7-2*k*2/5)
  points <- data.frame(x = c(1+2*k/5,8-2*k/5), y = c(2+2*k*2/5,7-k*2/5))
  #this is the first point's coordinate
  first_segment<- data.frame(x1 =1+2*k/5, y1 = 2+2*k*2/5)
  #the second point's coordinate
  second_segment <- data.frame(x2 = 8-2*k/5, y2 =7-k*2/5)
  #the two segments will be combined to form one row
  cycle <- cbind(first_segment, second_segment)
  #to generate the linear model
  model<-lm(y ~x, data= points)
  #drawing the lines given by the coefficients of the model in black
  abline(model,col = 1)
  #saving the cycle in the 'allpoints' dataframe
  allpoints <- rbind(allpoints, cycle = cycle)
}

for (k in 0:5){
  #defining the design matrix:
  X <- matrix(nrow = 2, ncol = 2)
  X[,1]<-1
  #adding the two points to make the design matrix:
  X[, 2]<- c(1+2*k/5,8-2*k/5)
  #defining the vector with the response variables:
  Y <- matrix(nrow = 2, ncol = 1)
  Y[,1] =  c(2+2*k*2/5,7-k*2/5)
  #equaling the ridge parameter to 1:
  lambda = 1
  #computing the beta vector:
  beta<- inv(t(X)%*%X + lambda*I)%*%t(X)%*%Y
  #drawing the lines computed by the beta vector in red:
  abline(c(beta[1],beta[2]),col = 2)
}


