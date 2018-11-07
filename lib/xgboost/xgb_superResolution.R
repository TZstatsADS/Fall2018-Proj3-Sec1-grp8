########################
### Super-resolution ###
########################

### Author: group 3

### Project 3

########### This part is only for testing ############
# test_dir <- "../data/test_set/"
# test_LR_dir <- paste(test_dir, "LR/", sep="")
# test_HR_dir <- paste(test_dir, "HR/", sep="")
# LR_dir<- test_LR_dir
# HR_dir<- test_HR_dir
#load(file="../output/fit_train.RData")
#load(file="../output/nnt_train.RData")
#load(file="../output/xgb_train.RData")
#imgLR<- readImage(paste0(LR_dir,"img","_",sprintf("%04d",0101),".jpg"))
#pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", 0101), ".jpg")
#imgHR<- readImage(pathHR)
#featMat<- array(NA,c(dim(imgLR)[1]*dim(imgLR)[2],8,3))  #328*171=56088
#rows=dim(imgLR)[1]
#cols=dim(imgLR)[2]

#for (d in 1:3) {
#  padded<- matrix(0,nrow = rows+2,ncol = cols+2)
#  padded[2:(rows+1),2:(cols+1)]<- imgLR@.Data[,,d]
#  count<- 0
#  for (i in 2:(rows+1)) {
#    for (j in 2:(cols+1)) {
      #neighbor8<- c(padded[i-1,j-1],padded[i,j-1],padded[i+1,j-1],padded[i-1,j],padded[i+1,j],padded[i-1,j+1],padded[i,j+1],padded[i+1,j+1])-padded[i,j]
#      neighbor8<- c(padded[i-1,j-1],padded[i,j-1],padded[i+1,j-1],padded[i-1,j],padded[i+1,j],padded[i-1,j+1],padded[i,j+1],padded[i+1,j+1])
#                  - (c(padded[(i-1):(i+1),(j-1):(j+1)])[-5] !=0) * padded[i,j]
#      count<- count+1
#      featMat[count,,d]<- neighbor8
#    }
#  }
#}
#predMAT<- test(fit_train,featMat,test.gbm =T)
#predMAT<- test(nnet_train,featMat,test.nnet = T)
#predArray<- array(predMAT,c(rows*2,cols*2,3))
#testt<- Image(predArray,colormode = Color)
#plot(testt)
#i<- 289
#photo_name<- paste0("img","_",sprintf("%04d",i),".jpg")
#writeImage(testt,photo_name)
#########################################################

superResolution <- function(LR_dir, HR_predict_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  library(magrittr)
  library(grid)
  library(raster)
  
  # profvis({
  n_files <- length(list.files(LR_dir))
  #n_files <- 1
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR@.Data)
    #pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    rows=dim(imgLR)[1]
    cols=dim(imgLR)[2]
          
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    padded <- array(0, c(rows+2, cols+2, 3))
    padded[2:(rows+1),2:(cols+1),] <- imgLR
    count<- 0
    # 
    # b <- c(padded)
    # b <- array(b, c(length(b)/3, 3))
    # for(i in (cols+2):(length(b)/3-cols-2)){
    #   neighbours <- b[c((i-cols-1):(i-cols+1), (i-1):(i+1), (i+cols-1):(i+cols+1)),]
    #   neighbours <- neighbours - neighbours[c(rep(5, 9),
    #                                           rep(14, 9),
    #                                           rep(23, 9))] # problem
    #   neighbours <- neighbours[c(-5, -14, -23)] # no central pixel
    #   count <- count+1
    #   featMat[count,,] <- neighbours
    #   print(i)
    # }
    
    for (x in 2:(rows+1)) {
      for (y in 2:(cols+1)) {
    # define square (don't allow for out of bound subscripts)
        cube <- padded[(x - 1):(x + 1), (y - 1):(y + 1), ]
        vectorized <- c(cube)
        vectorized <- vectorized - vectorized[c(rep(5, 9),
                                    rep(14, 9),
                                    rep(23, 9))] # problem
        vectorized <- vectorized[c(-5, -14, -23)] # no central pixel
        count <- count+1
        featMat[count,,] <- vectorized
        }
      }
      
    #   for (d in 1:3) {
    #   padded <- matrix(0, nrow = rows+2, ncol = cols+2)
    #   padded[2:(rows+1),2:(cols+1)] <- imgLR[,,d]
    #   v <- c(padded)
    #   count<- 0
    #   for (k in 2:(rows+1)) {
    #     for (j in 2:(cols+1)) {
    #       neighbor8 <- c(padded[(k-1):(k+1),(j-1):(j+1)])
    #       neighbor8 <- neighbor8[-5] - (neighbor8[-5] != 0) * padded[k,j]
    #       count <- count+1
    # 
    #       featMat[count,,d]<- neighbor8
    #     }
    #   }
    # }

    ### step 2. apply the modelList over featMat
    #predMAT <- test(modelList, featMat,test.gbm = T) # for baseline
    # predMAT<- test(modelList,featMat,test.nnet =T)  # for neural network
     predMAT<- test.xgb(modelList,featMat)  # for xgboost

    

    ### step 3. recover high-resolution from predMat and save in HR_dir
    predArray<- array(predMAT,c(rows*2,cols*2,3))
    
    a <- imgLR[rep(1:nrow(imgLR), times = rep(2, nrow(imgLR))), rep(1:ncol(imgLR), times = rep(2, ncol(imgLR))),]
    predArray <- predArray + a
    

    predicted_image<- Image(predArray, colormode = Color)
    photo_name<- paste0(HR_predict_dir,"img","_",sprintf("%04d",i),".jpg")
    print(i)
    writeImage(predicted_image,photo_name)
    
    
    
    
  }
  # })
}
