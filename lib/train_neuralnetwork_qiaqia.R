#install.packages("neuralnet")
run.nnt=T

####  function:
train_nn = function(feat_train, label_train){
  
  library("neuralnet")
  
  start <- Sys.time()
  modelList <- list()
  
  for (i in 1:12){
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- feat_train[, , c2]
    labMat <- label_train[, c1, c2]
    df1 <- data.frame(matrix(featMat,ncol=8))
    df2 <- data.frame(matrix(labMat,ncol=1))
    colnames(df1) <- paste0("feature", 1:8) 
    colnames(df2) <- paste0("label")
    dataset <- cbind(df1, df2)
    model <- neuralnet(label~feature1+feature2 +feature3+feature4+feature5+feature6+feature7+feature8,data=dataset,hidden=1,linear.output = T)
    modelList[[i]] <- list(fit=model)
  }
  end <- Sys.time()
  time <- end -start
  return(list(modelList,time=time))
}

##### data & run function:
feat_train <- dat_train$feature[1:100000,,]
label_train <- dat_train$label[1:100000,,]

if(run.nnt){
  nnt_train <- train_nn(feat_train, label_train)
  cat("Time for training neural network model=", nnt_train$time, "s \n")
}