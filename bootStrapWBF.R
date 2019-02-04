fle <- file.choose()

library(tidyverse)


av <- read_csv(fle)


t.test(x = av$`wbf(aud)`, av$`wbf(vid)`, paired = TRUE)

hist(av$`wbf(aud)` - av$`wbf(vid)`)


bootFun <- function(av){
  # resample individuals
  dfSamp = av[sample(1:nrow(av), replace = TRUE), 2:3 ]
  meanDiff <- mean(dfSamp$`wbf(aud)` - dfSamp$`wbf(vid)`)
  return(meanDiff)
}


bootSamps <- replicate(100000, bootFun(av))

hist(bootSamps, breaks = 50)
abline(v = 0, col = "red")
mean(bootSamps < 0) * 2


boxplot(av$`wbf(aud)`, av$`wbf(vid)`)
