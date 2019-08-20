X1 = rpois(100000, lambda = 1)

X2 = rpois(100000, lambda = 5)


diffdist = X2 + X1
hist(X1)
hist(X2)
hist(diffdist)




bootDiff = function(){
  
  x1 = rnorm(100)
  x2 = rnorm(100)
  
  diff = x2 - x1
  mod = lm(diff ~ I(1:100))
  return(summary(mod)$coef[2,4])
}


pvals = replicate(10000, bootDiff())
hist(pvals)


bootDiff2 = function(){
  
  x1 = rnorm(100)
  x2 = rnorm(100)
  
  diff = x2^2 - x1^2
  mod = lm(diff ~ I(1:100))
  return(summary(mod)$coef[2,4])
}


pvals = replicate(10000, bootDiff2())
hist(pvals)



perLoad_L = (runif(100, 0, 100))

perLoad_H = (perLoad_L + 50)  + rnorm(100)

plot(perLoad_L, perLoad_H)


deltaPercLoad = perLoad_H - perLoad_L
avgPercLoad = (perLoad_H + perLoad_L) / 2

plot(avgPercLoad, deltaPercLoad)



xx = rnorm(100)

ww = rnorm(100)


yy = rnorm(100)


plot(xx, yy) 

plot(xx*ww, yy*ww)

m1 = lm(yy ~ xx)

summary(m1)

m2= lm(I(yy*ww) ~ I(xx*ww))
summary(m2)

## boot3


bootDiff3 <- function(){
  xx = rnorm(100)
  ww = rnorm(100)
  yy = rnorm(100)
  m1 = summary(lm(yy ~ xx))
  # plot(xx, yy)
  # plot(I(yy*ww) ~ I(xx*ww))
  m2= lm(I(yy/ww) ~ I(xx/ww))
  
  
  return(c(m1$coefficients[2, 4],summary(m2)$coef[2, 4]) )
  
  
}

reps = replicate(5000, bootDiff3())
plot(t(reps))

hist(t(reps)[,1])
hist(t(reps)[,2])



bootDiff4 <- function(){
  xx = rnorm(100)
  ww = xx + rnorm(100)
  aa = prcomp(data.frame(xx, ww), center = TRUE, scale = TRUE)
  p1 = -predict(aa)[,1] 
  yy = rnorm(100)
  m1 = summary(lm(yy ~ p1))
  # plot(p1, yy)
  # plot(I(yy*ww) ~ p1)
  m2= lm(I(yy*ww) ~ p1)
  
  
  return(c(m1$coefficients[2, 4],summary(m2)$coef[2, 4]) )
  
  
}

reps = replicate(5000, bootDiff4())
plot(t(reps))

hist(t(reps)[,1])
hist(t(reps)[,2])


bootDiff5<- function(){
  xx = runif(100)
  yy = rnorm(100)
  m1 = summary(lm(yy ~ xx))
  # plot(p1, yy)
  # plot(I(yy*ww) ~ p1)
  m2= lm(I(yy*xx) ~ xx)
  
  
  return(c(m1$coefficients[2, 4],summary(m2)$coef[2, 4]) )
  
  
}

reps = replicate(5000, bootDiff5())
plot(t(reps))

hist(t(reps)[,1])
hist(t(reps)[,2])





# percent change could depend on weight


weight = seq(0, 10, length.out = 100)

combWeight = weight + weight *  0.6

combWeight[51:100] = weight[51:100] + weight[51:100] *  0.1

addedWeight = combWeight - weight
plot(addedWeight)

plot(addedWeight, combWeight)

yy = rep(c(1, 0), each = 50)


plot( addedWeight, yy)

percAdd = (addedWeight + xx) / 2
plot(percAdd, xx)



bootDiff6<- function(){
  beeSize = runif(100) + 4
  winglen = beeSize + rnorm(100, sd = 0.05)
  #plot(winglen, beeSize)
  
  changeInAmp = rep(1, length(beeSize))#rnorm(100, mean = -1,  sd = 0.1)
  # (changeInAmp)
  changeInArcLen = changeInAmp * winglen
  
  # plot(beeSize, changeInAmp)
  # plot(changeInArcLen~beeSize)
  
  s1 = summary(lm(changeInAmp~beeSize))
  return(s1$coefficients[2, 4])
  
  }

reps = replicate(10000, bootDiff6())
hist(reps)


plot(beeSize, changeInAmp)

plot(beeSize, changeInArcLen)


###########################################################################
# storks and babies example
# from this paper https://www.jstor.org/stable/pdf/2983064.pdf?refreqid=excelsior%3Ad1796dde58ee466bceccb9f7971bf6ee
numWomenPerCounty = rep(c(1, 2, 3, 4, 5), each = 10 )
numStorks = ceiling(numWomenPerCounty + rnorm(length(numWomenPerCounty), sd = 0.9) + 3)
numBabies = ceiling(numWomenPerCounty* 5 *rnorm(length(numWomenPerCounty),mean = 1, sd = 0.1) + 10)


# stork rate positively correlated with birth rate
plot(numStorks/numWomenPerCounty, numBabies/numWomenPerCounty)
abline(lm(I(numBabies/numWomenPerCounty) ~ I(numStorks/numWomenPerCounty)))


# birth rate vs. numStorks
plot(numStorks, numBabies/numWomenPerCounty)
abline(lm(I(numBabies/numWomenPerCounty) ~ I(numStorks)))


# birth rate vs. numStorks
plot(numStorks, numBabies)
abline(lm(I(numBabies) ~ I(numStorks)))

###########################################################################


###########################################################################
# Hypothetical buzz ratio example
# from this paper: file:///D:/Dropbox/AcademiaDropbox/PapersToCritique/VallejoMarin_BuzzPollinationBodySize/Paper.pdf

beeSize = rnorm(1000,mean = 5, sd = 0.9)

flightFreq = beeSize*20 + 150 + rnorm(1000, sd = 10)
buzzFreq = rnorm(1000, mean = 300, sd = 20) 


plot(buzzFreq ~ beeSize)
plot(flightFreq ~ beeSize)
abline(lm(I(flightFreq) ~ beeSize))


# in this case, buzzFreq is independent of bee size,
# flight freq is positively associated with bee size
plot(I(buzzFreq / flightFreq) ~ beeSize)
abline(lm(I(buzzFreq / flightFreq) ~ beeSize))



##################### flight freq is independent but buzz freq depends on size?
beeSize = rnorm(1000,mean = 5, sd = 0.9)

flightFreq = rnorm(1000, mean = 200, sd = 20)
buzzFreq =  beeSize*20 + 150 + rnorm(1000, sd = 10)


plot(buzzFreq ~ beeSize)
plot(flightFreq ~ beeSize)
abline(lm(I(flightFreq) ~ beeSize))


# in this case, flight freq is independent of bee size,
# buzz freq is positively associated with bee size
plot(I(buzzFreq / flightFreq) ~ beeSize)
abline(lm(I(buzzFreq / flightFreq) ~ beeSize))


##################### what if both flight freq and buzz freq depend on size, but to different degrees
beeSize = rnorm(1000,mean = 5, sd = 0.9)

flightFreq = beeSize*20 + rnorm(1000, mean = 200, sd = 10)
buzzFreq =  beeSize*10 + 350 + rnorm(1000, sd = 10)



plot(buzzFreq ~ beeSize, ylim = c(200, 600))
points(flightFreq ~ beeSize)
abline(lm(I(flightFreq) ~ beeSize))
abline(lm(I(buzzFreq) ~ beeSize))


# in this case, flight freq is independent of bee size,
# buzz freq is positively associated with bee size
plot(I(buzzFreq / flightFreq) ~ beeSize)
abline(lm(I(buzzFreq / flightFreq) ~ beeSize))


summary(lm(buzzFreq ~ beeSize + flightFreq))

car::vif(lm(buzzFreq ~ beeSize + flightFreq))


plot(flightFreq, buzzFreq)




bootDiff7<- function(){
  y1 = rnorm(100)
  y2 = rnorm(100)
  
  x1 = rnorm(100)
  x2 = rnorm(100)
  

  s1 = summary(lm(I(y1 - y2) ~ I(x1 - x2)))
  return(s1$coefficients[2, 4])
  
}

reps = replicate(10000, bootDiff7())
hist(reps)



#######################################################
# simulate 1% loading stuff


bootCoup1 = function(){
  f_h = rnorm(100, mean = 100)
  f_l = rnorm(100, mean = 100)
  
  mstarved = rnorm(100, mean = 100)
  m2h = rnorm(100, mean = 100)
  m2l = rnorm(100, mean = 100)
  mFh = rnorm(100, mean = 100)
  mFl = rnorm(100, mean = 100)
  
  y = (f_h - f_l)*mstarved / 
    ((m2h + mFh - m2l - mFl)*50)
  apl = 25 / mstarved * (m2h + mFh + m2l + mFl - 4*mFl)

  return(summary(lm(y~apl))$coef[2, c(1, 4)])

}

reps = replicate(5000, bootCoup1())
hist(t(reps)[,2])
mean(t(reps)[,2] < 0.05)

plot(t(reps))

hist(t(reps)[,1], breaks = 100)
hist(t(reps)[,2])
mean(t(reps)[,1])


bootCoup2 = function(){
  
  y = rnorm(100)
  x = rnorm(100)
  
  return(summary(lm(y~x))$coef[2, c(1, 4)])
  
}

bootCoup2()
reps = replicate(10000, bootCoup2())
mean(t(reps)[,2] < 0.05)
plot(t(reps))

hist(t(reps)[,1])
hist(t(reps)[,2])
mean(t(reps)[,1])




bootCoup3 = function(){
  
  y = rnorm(100)
  x = rnorm(100)
  
  summary(lm(I(y-x)~x + 0 + I(x) ))
  
  return(summary(lm(I(y-x)~I(( x )/2)))$coef[2, c(1, 4)])
  
}


reps = replicate(10000, bootCoup3())
hist(t(reps)[,2])
mean(t(reps)[,2] < 0.05)
plot(t(reps))

hist(t(reps)[,1])
hist(t(reps)[,2])
mean(t(reps)[,1])
