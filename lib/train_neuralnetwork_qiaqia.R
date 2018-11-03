require(nnet)

run.nnt=T

####  function:
train_nnet = function(feat_train, label_train){
  
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
    model <- nnet(label~feature1+feature2 +feature3+feature4+feature5+feature6+feature7+feature8,data=dataset,size=2,linout = TRUE)
    modelList[[i]] <- list(fit=model)
  }
  return(modelList)
}

##### data & run function:
#feat_train <- dat_train$feature[1:10000,,]
#label_train <- dat_train$label[1:10000,,]

#if(run.nnt){
# nnet_train <- train_nnet(feat_train, label_train)
#}
#save(nnet_train, file="../output/nnt_train.RData")