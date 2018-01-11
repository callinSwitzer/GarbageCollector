
aa <- file.choose()
aa

tmp <- read.csv(aa, header = TRUE, stringsAsFactors = FALSE)

tail(tmp)


tmp$time2 <- as.POSIXct(tmp$time, format = "%Y/%m/%d %H:%M:%S") - 3600*4

tail(tmp)

tmp$tmpF = tmp$temp * 9/5 + 32
plot(tmpF ~ time2, data = tmp, type = 'l')


tmp$light = 1024 - tmp$light

plot(light ~ time2, data = tmp, type = 'l')



par(mfrow = c(3,1))
plot(light ~ time2, data = tmp[1:10000, ], type = 'l')


plot(light ~ time2, data = tmp[10000:30000, ], type = 'l')

plot(light ~ time2, data = tmp[30000:nrow(tmp), ], type = 'l')


dev.off()

