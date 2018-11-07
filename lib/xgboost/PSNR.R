#####################
### Compute PSNR  ###
#####################

### Authors: Gabriel Benedict
### Project 3

psnr <- function(reconst_dir, original_dir){
  
  n_files <- length(list.files(reconst_dir))
  PSNR <- array(NA, n_files)
  
  for(i in 1:n_files){  
    
    reconstObj <- readImage(paste0(reconst_dir,  "img_", sprintf("%04d", i), ".jpg"))
    originalObj <- readImage(paste0(original_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    reconst <- as.array(reconstObj@.Data)
    original <- as.array(originalObj@.Data)

    width <- dim(reconst)[1]
    height <- dim(reconst)[2]
    
    MSE <- sum((original - reconst)^2) / (3 * width * height)
    PSNR[i] <- 20 * log10(1) - 10 * log10(MSE)
  
    print(i)
  }
  
  return(PSNR)
}