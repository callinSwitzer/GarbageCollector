# Callin Switzer
# 20 Aug 2018
# Learning model



ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse")
ipak(packages)

{if(grepl(pattern = "wind", ignore.case = TRUE, x = Sys.info()[1])){
  # windows
  dataDir = "D:/Dropbox/dataAnalysisForOthers/WrightLearningModel/Data/"
  figDir = "D:/Dropbox/dataAnalysisForOthers/WrightLearningModel/Figures/"
}
  else{
    # mac
    dataDir = "/Users/cswitzer/Dropbox/dataAnalysisForOthers/WrightLearningModel/Data/"
    figDir= "/Users/cswitzer/Dropbox/dataAnalysisForOthers/WrightLearningModel/Figures/"
  }}


print(dataDir)

dir.create(file.path(figDir), showWarnings = FALSE)
# set ggplot theme
theme_set(theme_classic() + theme(axis.text=element_text(colour="black")))



rawDat <- read_delim(file.path(dataDir, "BuzzLatency_wright-learn-curve_rawData.csv"), delim = "\t")




rawDat

rawDat <- rawDat %>%
  group_by(BeeID) %>%
  mutate(cumLatency = cumsum(BuzzLatency)) %>%
  arrange(BeeID, VisitNo) %>%
  mutate(cumAvgLatency = cumLatency / VisitNo) %>%
  ungroup() %>%
  mutate(trt = grepl(pattern = "b",ignore.case = TRUE, x = .$BeeID))
  

rawDat

ggplot(rawDat, aes(x = VisitNo, y = cumAvgLatency)) + 
  geom_point() + 
  facet_wrap(~BeeID) + 
  scale_x_log10() + 
  scale_y_log10() + 
  stat_smooth(method = "lm")


# fit lmer
library(lme4)

m1 <- lmer(log(cumAvgLatency, base = 10) ~ log(VisitNo, base = 10) +  trt + (1|BeeID), data = rawDat)
summary(m1)

plot(m1)




a = 10^(1.08036)
b = -0.61806


plot(x = rawDat$VisitNo, y = rawDat$cumAvgLatency)
curve(a*x^b, from= 1, to = 10, add= TRUE)





