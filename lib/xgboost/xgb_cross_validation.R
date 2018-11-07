########################
### Cross Validation ###
########################

### Author:zx2229
### Project 3

########## xgboost



cv.function <- function(X.train, y.train, par=NULL, K){
  
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  PSNR = rep(NA,K)
  
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]
    
    #par <- list(depth=d
      fit <- train.xgb(train.data,train.label,par = NULL)
      pred <- test.xgb(fit, test.data)  
     
    	
      
  cv.error[i] <- mean((pred - as.numeric(test.label))^2)
  
  
  
  print(i)
  
  }
  return(c(Mean.cv=mean(cv.error),SD.cv = sd(cv.error)))
}
# xgb.cv.begin = Sys.time()
# 
# cv.xgboost = cv.function(X.train,y.train,K=5)
# 
# xgb.cv.end = Sys.time()
# 
# xgb.cv.time = xgb.cv.end - xgb.cv.begin
# 
# xgb.cv.time
