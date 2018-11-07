train_nnet = function(feat_train, label_train, par=NULL){
  
  require(nnet)
  modelList <- list()
  
  if(is.null(par)){
    size <- 2
    decay <- 0
  } else {
    size <- par$size
    decay <- par$decay
  }
  
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
    model <- nnet(label~feature1+feature2 +feature3+feature4+feature5+feature6+feature7+feature8,data=dataset,size=size,linout = TRUE,trace=F,decay=decay)
    modelList[[i]] <- list(fit=model)
  }
  return(modelList)
}
