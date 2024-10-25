library(here)
library(mltools)
library(keras)
library(tfdatasets)
library(data.table)
library(dplyr)

here()

setwd("G:\\Workspace\\img-proc\\may-batch")
setwd("G:\\Workspace\\img-proc\\Clipped_Canny_LastOnes")
flist <- Sys.glob("*.tif")
nf <- length(flist)

meantheta <- rep(0, nf)
medslopevec <- rep(0, nf)
medthetavec <- rep(0, nf)
madslopevec <- rep(0, nf)
madthetavec <- rep(0, nf)

namevec <- rep(NA, nf)


for(curf in 1:nf)
{
  setwd("G:\\Workspace\\img-proc\\Clipped_Canny_LastOnes")
  
  fin <- flist[curf]
  img.oir <-  OpenImageR::readImage(fin)
  y = readImage(flist[curf])[,,1]
  #y = readImage(flist[curf])
  #y <- t(y)
  x = thresh(y, 10, 10, 0.05)
  nmask = fillHull(x )
  #nmask = thresh(nuc, 10, 10, 0.05)
  #nmask = opening(nmask, makeBrush(5, shape='disc'))
  nmask = fillHull(nmask)
  nmask = bwlabel(nmask)
  ctmask = opening(y>0.1, makeBrush(3, shape='disc'))
  cmask = propagate(y, nmask, ctmask)
  ctile = untile(cmask, c(3, 3))
  display(ctile, title='Tiles')
  ntile <- dim(ctile)[3]
  # for(curtile in 1:ntile)
  # {
  #
  #temptile <- ctile[,,curtile]
  temptile <-cmask
  ## compute shape features
  fts = computeFeatures.shape(temptile)
  # fts
  ftc = computeFeatures.moment(temptile)
  #ftc
  thetavec <- ftc[,5]
  slopevec <- -tan(thetavec)
  outvec <- cbind(thetavec, slopevec)
  fout <- gsub("*.tif", "", fin)
  #fout <- paste(fout, "-tile-", curtile, "-theta.txt", sep = "")
  fout <- paste(fout, "-theta.txt", sep = "")
  write.table(thetavec, fout, row.names = F, quote = F)
  medslopevec[curf] <- median(slopevec)
  medthetavec[curf] <- median(thetavec)
  madslopevec[curf] <- mad(slopevec)
  madthetavec[curf] <- mad(thetavec)
  namevec[curf] <- gsub("*.tif", "", fin)
  setwd("G:\\Workspace\\img-proc\\Clipped_Canny_LastOnes\\output")
  
  fout <- gsub("*.tif", "", fin)
  nr <- dim(temptile)[1]
  nc <- dim(temptile)[2]
  # fout <- paste(fout, "-tile-", curtile, "-img.png")
  fout <- paste(fout, "-theta-img.png")
  png(fout)
  image(temptile[nr:1,])
  m <- median(slopevec)
  b <- .5*(1-m)
  xseq <- seq(0, 1, by = .1)
  yseq <- m * xseq + b
  lines(xseq, yseq, col = "green")
  titlestr <- paste("MED=", signif(median(thetavec), 3), " , MAD=",
                    signif(mad(thetavec), 3), sep = "")
  title(titlestr)
  dev.off()
  fout <- gsub("*.tif", "", fin)
  fout <- paste(fout, "-slope-img.png")
  png(fout)
  image(temptile[nr:1,])
  m <- median(slopevec)
  b <- .5*(1-m)
  xseq <- seq(0, 1, by = .1)
  yseq <- m * xseq + b
  lines(xseq, yseq, col = "green")
  titlestr <- paste("MED=", signif(median(slopevec), 3), " , MAD=",
                    signif(mad(slopevec), 3), sep = "")
  title(titlestr)
  dev.off()
  # }
  #  slopevec <- atan(thetavec)
  #  slopehist <- hist(slopevec)
  #  meanslope <- mean(slopevec)
  #  medianslope <- median(slopevec)
  #  modeslope <- mode(slopevec)
  #  modeslope <- slopehist$mids[which.max(slopehist$counts)]
  #  fout <- paste("curtile-", curf, ".png", sep = "")
  #
  #  png(fout)
  #
  #  nr <- dim(cmask)[1]
  #  cmat <- matrix(cmask, nrow = nr)
  # # ctmat <- matrix(t(cmat))
  #  image(t(cmat)[,nr:1])
  #
  #
  #  xseq <- seq(0, 1, by = .1)
  #  #cur.rads <- max.per * pi / 180
  #  m <- -atan(medianslope)
  #  b <- .5*(1-m)
  #  yseq <- m * xseq + b
  #  lines(xseq, yseq, col = "green")
  #  dev.off()
}

outmat.slopes <- cbind(namevec, medslopevec, medthetavec, madslopevec, madthetavec)
getwd()
rownames(outmat.slopes) <- flist
write.table(outmat.slopes, "slope-table-med-mad-lastones.txt", row.names = F, sep = ",", quote = F)
