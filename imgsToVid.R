# how to convert images

getwd()
setwd("C:/Users/calli/Desktop/")
ii =1


newDir = "ImgsToVid"

dir.create(newDir)


for(ii in 1:10){
  x = rnorm(10)
  y = rnorm(10)
  imgName = formatC(x = ii, width = 4, flag = "0") # add leading zeros
  fnme = file.path(newDir,paste0(imgName, ".png") )
  png(filename = fnme, width = 4, height = 4, units = "in", res = 1000)
  plot(x, y)
  dev.off()
  
}



# run ffmpeg in terminal

# converting a directory of png images to a video
# files are saved with leading zeros
# -r is output frame rate
# first image is 0001.png
# this allows the file to be played on many platforms: -c:v libx264 -pix_fmt yuv420p 
# this specifies the scaling algorithm, but doesn't do any actual scaling: -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2"


# change directory to the directory with all the images
#### cd ImgsToVid

#### ffmpeg -start_number 1 -r 29 -i %04d.png -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" -c:v libx264 -pix_fmt yuv420p -y output.mp4

