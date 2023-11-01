library(EBImage)
library(imager)


rootwd <- getwd()
setwd("image_files")

folder_list <- list.files()

nfolders <- length(folder_list)
for(curfolder in 1:nfolders)
{
  setwd(rootwd)
  setwd("image_files")
  setwd(folder_list[curfolder])
  
  tiflist <- Sys.glob("*.tif")
  ntifs <- length(tiflist)
 for(curtif in 1:ntifs)
 {
   
   curimg <- readImage(tiflist[curtif])
   img.gray <-curimg[,,1:4]
   
   img.gray <- curimg
   colorMode(img.gray) <- Grayscale
   thresh.gray <-thresh(img.gray, 10, 10, 0.15)
  # plot(thresh.gray)
   kern <- makeBrush(5, shape='disc') 
   dilate.gray <- EBImage::dilate(thresh.gray,kern)
   
   
 }
   
}