fle = file.choose()
fle


dd = read.csv(fle)
nrow(dd)


head(dd)

dd$time

dd <- dd[dd$pressure > 0, ]
hist(dd$pressure)

aa = as.POSIXct(dd$time)
library(lubridate)
bb = aa - hours(4) 



dd["temp.F."] = dd$temp * 9/5 + 32

dd$light <- 1024 - dd$light
quartz()
par(mfrow = c(5,1))
par(mai = c(0.4, 0.5, 0.2, 0.5))
for(ii in colnames(dd)[2:6]){
  #plot(x = bb, y = dd[,ii], main = ii, ylab = ii,  xlab = "time", pch = ".", type = "l")
  preds = predict(loess(dd[,ii] ~ as.numeric(bb), span = 0.005))
  plot(x = bb, y = preds, main = ii, ylab = ii,  xlab = "time", pch = ".", type = "l")
  
}

