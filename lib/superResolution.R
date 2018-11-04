########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

########### This part is only for testing ############
test_dir <- "../data/test_set/" 
test_LR_dir <- paste(test_dir, "LR/", sep="")
test_HR_dir <- paste(test_dir, "HR/", sep="")
LR_dir<- test_LR_dir
HR_dir<- test_HR_dir
load(file="../output/fit_train.RData")
load(file="../output/nnt_train.RData")
imgLR<- readImage(paste0(LR_dir,"img","_",sprintf("%04d",0101),".jpg"))
pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", 0101), ".jpg")
imgHR<- readImage(pathHR)
featMat<- array(NA,c(dim(imgLR)[1]*dim(imgLR)[2],8,3))  #328*171=56088
rows=dim(imgLR)[1]
cols=dim(imgLR)[2]

for (d in 1:3) {
  padded<- matrix(0,nrow = rows+2,ncol = cols+2)
  padded[2:(rows+1),2:(cols+1)]<- imgLR@.Data[,,d]
  count<- 0
  for (i in 2:(rows+1)) {
    for (j in 2:(cols+1)) {
      neighbor8<- c(padded[i-1,j-1],padded[i,j-1],padded[i+1,j-1],padded[i-1,j],padded[i+1,j],padded[i-1,j+1],padded[i,j+1],padded[i+1,j+1])-padded[i,j]
      count<- count+1
      featMat[count,,d]<- neighbor8
    }
  }
}
predMAT<- test(fit_train,featMat,test.gbm =T)
predMAT<- test(nnet_train,featMat,test.nnet = T)
predArray<- array(predMAT,c(rows*2,cols*2,3))
testt<- Image(predArray,colormode = Color)
plot(testt)
#writeImage(ImageObject,filepath)
#########################################################

superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  library(magrittr)
  library(grid)
  library(raster)
  
  n_files <- length(list.files(LR_dir))

  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR@.Data)
    
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    rows=dim(imgLR)[1]
    cols=dim(imgLR)[2]
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for (d in 1:3) {
      padded <- matrix(0,nrow = rows+2,ncol = cols+2)
      padded[2:(rows+1),2:(cols+1)] <- imgLR[,,d]
      count <- 0
      for (i in 2:(rows+1)) {
        for (j in 2:(cols+1)) {
          neighbor8 <- c(padded[i-1,j-1],padded[i,j-1],padded[i+1,j-1],padded[i-1,j],padded[i+1,j],padded[i-1,j+1],padded[i,j+1],padded[i+1,j+1])-padded[i,j]
          count<- count+1
          featMat[count,,d]<- neighbor8
        }
      }
    }

    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    #predMAT_gbm<- test(fit_train,featMat,test.gbm =T)
    #predMAT_nnet<- test(nnet_train,featMat,test.nnet = T)
    

    ### step 3. recover high-resolution from predMat and save in HR_dir
    predArray <- array(predMAT,c(rows*2,cols*2,3))
    predicted_image <- Image(predArray,colormode = Color)
    #writeImage(predicted_image,filepath)
    
  }
}