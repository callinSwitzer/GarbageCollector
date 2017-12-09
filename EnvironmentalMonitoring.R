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


car::scatterplotMatrix(dd)


dd["temp.F."] = dd$temp * 9/5 + 32

par(mfrow = c(3,2))
for(ii in colnames(dd)[2:6]){
  #plot(x = bb, y = dd[,ii], main = ii, ylab = ii,  xlab = "time", pch = ".", type = "l")
  preds = predict(loess(dd[,ii] ~ as.numeric(bb), span = 0.05))
  plot(x = bb, y = preds, main = ii, ylab = ii,  xlab = "time", pch = ".", type = "l")
  
}

