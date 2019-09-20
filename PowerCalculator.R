system.time({
  rr = rnorm(1024*2**12)
  fr = fft(rr)
})

length(fr)


system.time({
  rr = rnorm(104729)
  fft(rr)
})




# sample size calculator
# t-test
# person heights
# min scientific difference = 5 inches
# alpha = 0.05, beta = 0.8
# prelim data

prelimData = rnorm(n = 10, mean = 72, sd = 4)
mean(prelimData)
sd(prelimData)


prelimData2 = rnorm(n = 10, mean = 65, sd = 4)
mean(prelimData2)
sd(prelimData2)


# two sample t-test
pooledVar = (9*var(prelimData) + 9*var(prelimData2))/18
pooledVar


ssize = 10
tt = (mean(prelimData) - mean(prelimData2)) / (sqrt(pooledVar)*sqrt(1/ssize + 1/ssize))
tt

t.test(prelimData, prelimData2, var.equal = TRUE)


calcPower <- function(n){
  prelimData = rnorm(n = n, mean = 72, sd = 4)
  prelimData2 = rnorm(n = n, mean = 71, sd = 4)

  
  # two sample t-test
  pooledVar = ((n-1)*var(prelimData) + (n-1)*var(prelimData2))/(n + n -2)

  tt = (mean(prelimData) - mean(prelimData2)) / (sqrt(pooledVar)*sqrt(1/n + 1/n))
  
  # return pvalue
  return(1-pt(tt, df = n+n-2))
}


reps = replicate(n = 1000, calcPower(600))
hist(reps)
mean(reps < 0.05) # returns the power
