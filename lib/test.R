######################################################
### Fit the regression model with testing data ###
######################################################

### Author: Chengliang Tang
### Project 3

test <- function(modelList, dat_test, test.gbm=F, test.rf=F, test.nnet=F,test.xgboost=F){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model list using training data
  ###  - processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  library("gbm")
  library("xgboost")
  
  predArr <- array(NA, c(dim(dat_test)[1], 4, 3))
  
  for (i in 1:12){
    fit_train <- modelList[[i]]
    ### calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_test[, , c2]
    ### make predictions
    if(test.gbm){
      predArr[, c1, c2] <- predict(fit_train$fit, newdata=featMat, 
                      n.trees=fit_train$iter, type="response")
    }
    if(test.rf){
      predArr[, c1, c2]<- predict(fit_train$fit, newdata=featMat, type="response")
    }
    if(test.nnet){
      featMat_dataframe<- data.frame(matrix(featMat,ncol = 8))
      colnames(featMat_dataframe)<- paste0("feature",1:8)
      predArr[,c1,c2]<- predict(modelList[[i]]$fit,featMat_dataframe,type="raw")
    }
    if(test.xgboost){
      predArr[, c1, c2] <- predict(fit_train, newdata=featMat)
    }
  }
  return(as.numeric(predArr))
}

