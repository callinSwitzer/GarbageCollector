fle = file.choose()
fle

directory = "E:"

ii = 30





first = TRUE
for(ii in 30:44){
  fle = file.path(directory, paste0("LOGGER", ii, ".csv"))
  tmp = tryCatch(read.csv(fle), error = function(e) NA)
  print(ii)
  
  if(first){
    dd = data.frame(tmp)
    first = FALSE
  }
  else{
    dd = rbind(dd, tmp)
  }
}

nrow(dd)
dd = dd[complete.cases(dd),]
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


x11()
par(mfrow = c(5,1))
par(mai = c(0.4, 0.5, 0.2, 0.5))
for(ii in colnames(dd)[2:6]){
  #plot(x = bb, y = dd[,ii], main = ii, ylab = ii,  xlab = "time", pch = ".", type = "l")
  preds = predict(loess(dd[,ii] ~ as.numeric(bb), span = 0.005))
  plot(x = bb, y = preds, main = ii, ylab = ii,  xlab = "time", pch = ".", type = "l")
  
}

length(preds)
plot(preds)
graphics.off()
