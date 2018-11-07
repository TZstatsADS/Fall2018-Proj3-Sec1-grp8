nnet_test <- function(modelList, dat_test){
  
  predArr <- array(NA, c(dim(dat_test)[1], 4, 3))
  
  for (i in 1:12){
    #fit_train <- modelList[[i]]
    ### calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_test[, , c2]
    featMat_dataframe <- data.frame(matrix(featMat,ncol=8))
    colnames(featMat_dataframe) <- paste0("feature", 1:8) 
    ### make predictions
    predArr[, c1, c2] <- predict(modelList[[i]],featMat_dataframe,type = "raw")$fit
  }
  return(as.numeric(predArr))
}
