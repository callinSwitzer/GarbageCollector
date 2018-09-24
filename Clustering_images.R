## Callin Switzer
## 24 Sept 2018
## group images of petals, according to their color


# Setup
# install.packages("imager")
library(imager)
library(ggplot2)
library(tidyverse)



# 1. Read in images from a directory
# these images are in the form of png files
# R reads them in with 4 color channels - red, green, blue, and transparency
imageDirectory <- "D:\\Dropbox\\dataAnalysisForOthers\\FlowerColorPhenotyping"

# keep only images that end with .png
imageFileNames <- dir(imageDirectory, 
                      full.names = TRUE)[grepl(pattern = "png", 
                                               dir(imageDirectory, 
                                                   full.names = FALSE))]

# 2. Clean images (remove transparency)
# function to remove the transparency channel
removeAlpha <- function(img){
  # note that this won't work for images with
  # only 3 channels
  
  as.tbl(as.data.frame(img)) %>%
    spread(key = cc, value = value, fill = 0) %>%
    filter(`4` == 1) %>%
    select(-`4`) %>%
    gather(key = cc, value = value, `1`:`3`) %>%
    mutate(cc = as.integer(cc)) 
}


# remove alpha channel for each image
imageList = imlist()
for(ii in 1:length(imageFileNames)){
  tmp = load.image(imageFileNames[ii])
  imageList[[ii]] <- removeAlpha(tmp)
}


# show images -- previously transparent parts are shown in black
par(mfrow = c(3,3))
for(ii in 1:length(imageList)){
  plot(as.cimg(as.data.frame(imageList[[ii]])))
}




# loop through images


histList = list()
for(jj in 1:length(imageList)){
  
  # convert to colorspaces from RGB to Lab (a perceptually uniform colorspace -
  # the numerical change corresponds to visually perceived change)
  im2 <- RGBtoLab(as.cimg(as.data.frame(imageList[[jj]])))
  
  
  # filter out data where luminance is > 0 (i.e. 
  # get rid of black)
  bdf <- as.tbl(as.data.frame(im2)) %>%
    # remove black
    spread(key = cc, value = value) %>%
    filter(`1` > 0) %>%
    gather(key = cc, value = value, `1`:`3`) 
  
  
  # concatenate histograms for each of L, a, and b
  # into a single vector, so that we can later calculate 
  # a distance matrix
  aa <- c(hist(bdf$value[bdf$cc == "1"], 
               breaks = seq(-100, 100,length.out = 100 ))$density, 
          hist(bdf$value[bdf$cc == "3"], 
               breaks = seq(-100, 100,length.out = 100 ))$density, 
          hist(bdf$value[bdf$cc == "3"], 
               breaks = seq(-100, 100,length.out = 100 ))$density)
  # append to list
  histList[[jj]] = aa
  
}

# show example of historgrams in Lab colorspace
ggplot(bdf,aes(value,col=cc)) + 
  geom_histogram(bins=30)+
  facet_wrap(~ cc)


# convert list to tbl
labDF <- as.tbl(as.data.frame(do.call(rbind, histList)))


# drop columns that are duplicated
labDF <- labDF[!duplicated(as.list(labDF))]

# double check that all rows have been standardized
rowSums(labDF)


# 3. Cluster images using heirarchical clustering
# note that many other clustering methods are available, 
# such as K-nearest neighbors

# first calculate distance matrix
# dist(): compute the distances between the rows of a data matrix

distMatrix <- dist(labDF)

# use heirarchical clustering

hcl = hclust(distMatrix)
plot(hcl)

par(mar = c(2,2,2,2))
m <- rbind(c(1, 1,1,1),c(1, 1,1,1), c(2, 3,4,5), c(6,7,8,9))
layout(m)
layout.show(9)
plot(hcl)


for(ii in 1:length(imageList)){
  plot(as.cimg(as.data.frame(imageList[[ii]])), main = ii, ylim = c(0, 500))
}

