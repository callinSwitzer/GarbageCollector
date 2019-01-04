#Bayes Beta-Bernoulli Model
  
  
  

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "car", "plyr", "tidyr")
ipak(packages)



# dataDir = "D:/Dropbox/UW/ExperWinter2018/BeeTwoFlowerChoiceData/"
# figDir = "D:/Dropbox/UW/ExperWinter2018/BeeTwoFLowerChoiceFigures/"

# mac directories
dataDir = "/Users/cswitzer/Dropbox/UW/ExperWinter2018/BeeTwoFlowerChoiceData"
figDir= "/Users/cswitzer/Dropbox/UW/ExperWinter2018/BeeTwoFlowerChoiceFigures"






# load in data
csvList = list.files(dataDir)


for (ii in csvList){
  
  
  tmp = read.csv(file.path(dataDir, ii), stringsAsFactors = FALSE)
  
  tmp$ID = ii
  
  if(ii == csvList[1]) newDF = tmp
  else newDF = rbind(tmp, newDF)
}  


newDF$rewardStatus = tolower(newDF$rewardStatus)
summaryDF = as.data.frame(xtabs(~newDF$ID + newDF$rewardStatus + newDF$accNum))

# convert to wide format
summary_wide = spread(summaryDF,   newDF.accNum, Freq)
summary_wide2 = spread(summaryDF,   newDF.rewardStatus, Freq)

# insert treatment
summary_wide$treatment = sapply(summary_wide$newDF.ID, FUN = function(x) newDF[newDF$ID == x, "treatment"][1])

# remove rows where both rewarded the bee
summary_wide = summary_wide[summary_wide$treatment != "Dev2/ai0_True__Dev2/ai1_True", ]

# make sure at one column is 0
summary_wide$`Dev2/ai0` ==0 | summary_wide$`Dev2/ai1` == 0

# combine to make a visit column
summary_wide$visits = summary_wide$`Dev2/ai0` + summary_wide$`Dev2/ai1`

# make wide
sw3 = spread(summary_wide[, c("newDF.ID", "newDF.rewardStatus", "visits", "treatment")],   newDF.rewardStatus, visits)

# refref: add hive humber
sw3$colonyNum = sapply(sw3$newDF.ID, FUN = function(x) newDF[newDF$ID == x, "colonyNum"][1])
sw3$IT = sapply(sw3$newDF.ID, FUN = function(x) newDF[newDF$ID == x, "ITSpan_mm"][1])

# ______________________

# add index
index = numeric()
for(ii in 1:nrow(newDF)){
  if(ii == 1) tmpID_old = "9999999"
  tmpID_new = newDF$ID[ii]
  if(tmpID_new != tmpID_old){
    ctr = 1
  }
  else ctr = ctr + 1
  index[ii] = ctr
  tmpID_old = tmpID_new
}

newDF$index = index
# save newDF
# write.csv(newDF, file = file.path(dataDir, "buzzingChoiceData_30May2018.csv"))



binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, ...)
}

newDF2 <- newDF[newDF$index <= 100, ]

# convert visit time to a timestamp
ttms = newDF2$timestamp %>% as.POSIXct(format="%Y_%m_%d__%H_%M_%OS")

# calculate time differences, based on timestamps
id = newDF2$ID[200]

timeDiffF = function(id){
  tmp = newDF2[newDF2$ID == id, ]
  
  tmsReformat = paste(substr(tmp$timestamp, 1, 20), substr(tmp$timestamp, 22, 999), sep = ".")
  
  ttms = tmsReformat %>% as.POSIXct(format="%Y_%m_%d__%H_%M_%OS")
  
  diffTms = as.numeric(ttms)
  
  for(jj in 1:nrow(tmp)){
    diffTms[jj] <- as.numeric(difftime(ttms[jj], ttms[1], units = "min"))
  }
  
  return(diffTms)
  
}
options(digits.mins = 3)

timeDiffs = sapply(unique(newDF2$ID), FUN = function(x) timeDiffF(x))
dd1  = as.data.frame(unlist(timeDiffs))
dd1$ID = paste0(sapply(rownames(dd1), function(x) strsplit(x, ".csv")[[1]][1]), ".csv")
dd1$index = sapply(rownames(dd1), function(x) strsplit(x, ".csv")[[1]][2]) %>% as.numeric

colnames(dd1)[1] = "TimeSinceStart"

#merge
newDF2$logicalReward = as.numeric(newDF2$rewardStatus == "true")
ndf3 <- merge(newDF2, dd1)

# plot
ggplot(ndf3[, ], aes(x = TimeSinceStart, y = logicalReward)) + 
  geom_point(position = position_jitter(height = 0.1), size = 0.5) +
  binomial_smooth(color = 'red')+  
  geom_line(alpha = 0.2) + 
  facet_wrap(~ID) + 
  theme_classic() + 
  xlab("Time since first buzz (min)")+
  ylab("Reward (1: True, 0: False)")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(), 
    #axis.text.x = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size=0.7)
  )



ggplot(ndf3[ndf3$ID == unique(ndf3$ID)[9], ], aes(x = TimeSinceStart, y = logicalReward)) + 
  geom_point(position = position_jitter(height = 0.1), size = 0.5) +
  binomial_smooth(color = 'red')+  
  geom_line(alpha = 0.2) + 
  theme_classic() + 
  xlab("Time since first buzz (min)")+
  ylab("Reward (1: True, 0: False)")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(), 
    #axis.text.x = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size=0.7)
  )




BernTrials = ndf3[ndf3$ID == unique(ndf3$ID)[9], "logicalReward"]


# beta prior
x = seq(0,1, length.out = 1000)
# shape 1 = a, shape 2 = b
# prior for first flower
a1 = 1.5
b1 = 1.5
# prior for second flower
a2 = 1.5
b2 = 1.5

# update prior after one trial
ii = 1
# plot
dens = dbeta(x, shape1 = a1, shape2 = b1)
dens2 = dbeta(x, shape1 = a2, shape2 = b2)

plot(x = x, y = dens, type = "l", col = 'red', ylim = c(0,max(c(dens, dens2))), ylab = "f(x)")
lines(x = x, y = dens2, col = 'blue')

{
# update flowers
print(BernTrials[0:ii])
if(BernTrials[ii] == 0){
  a1 = a1
  b1 = b1 + 1
}
if(BernTrials[ii] == 1) {
  a2 = a2 + 1
  b2 = b2
}

ii = ii + 1

# plot
dens = dbeta(x, shape1 = a1, shape2 = b1)
dens2 = dbeta(x, shape1 = a2, shape2 = b2)

plot(x = x, y = dens, type = "l", col = 'red', ylim = c(0,max(c(dens, dens2))), ylab = "f(x)", bty = 'l')
lines(x = x, y = dens2, col = 'blue')

# shade 95% quantiles

upper1= qbeta(0.975, a1, b1, ncp = 0, lower.tail = TRUE)
lower1= qbeta(0.975, a1, b1, ncp = 0, lower.tail = FALSE)

cord.x1 <- c(lower1,seq(lower1,upper1,length.out = 1000),upper1) 
cord.y1 <- c(0,dbeta(seq(lower1,upper1,length.out = 1000), a1, b1 ),0)

polygon(cord.x1,cord.y1,col=rgb(1, 0, 0,0.5),border = NA)


upper2= qbeta(0.975, a2, b2, ncp = 0, lower.tail = TRUE)
lower2= qbeta(0.975, a2, b2, ncp = 0, lower.tail = FALSE)

cord.x2 <- c(lower2,seq(lower2,upper2,length.out = 100),upper2) 
cord.y2 <- c(0,dbeta(seq(lower2,upper2,length.out = 100), a2, b2 ),0)

polygon(cord.x2,cord.y2,col=rgb(0, 0, 1,0.5), border = NA)
}
