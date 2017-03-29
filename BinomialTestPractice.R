mat = matrix(c(1,2,3,43,45,5,5,5,66), nrow = 3)
mat

vecProd <- function(vector){
     
     tmp = 1
     for(ii in 1:(length(vector))){
          tmp = tmp * vector[ii]
     }
     
     return(tmp)
}


vps = numeric()
for(jj in 1:nrow(mat)){
     vps[jj] = vecProd(mat[jj,])
}
vps

# faster way
apply(mat, MARGIN = 1, prod)



dta = rbinom(100, size = 1, 0.3)

phat = mean(dta)

CI = c(phat + 1.96 * sqrt(phat * (1-phat) / 100), phat - 1.96 * sqrt(phat * (1-phat) / 100))
CI


ns = sum(dta == 1)
nf = sum(dta == 0)

CI2 <- 1/100*c(ns + 1.96 * sqrt(1/100 * ns * nf), ns - 1.96 * sqrt(1/100 * ns * nf))



xx = rbinom(100, 100, 0.5)
hist(xx)


yy = rbinom(100, 100, 0.3)
hist(yy)


xb = xx - yy
hist(xb)


SE = sqrt(sum((xb - mean(xb))^2) / (length(xb) - 1))


# t-test for diff in means == 0 

tstat <- (mean(xb) - 0)  / (SE / sqrt(100))

tstat
qt(df = 99, p = 0.975)


# 95% CI
ts = qt(df = 99, p = 0.975)
c(mean(xb) - ts * (SE / sqrt(100)), mean(xb) + (SE / sqrt(100)))



# diff in proportions
samp1 = rbinom(size = 1, n = 400,prob = 0.4)
samp2 = rbinom(size = 1, n = 100, prob = 0.5)
p1 = mean(samp1)
p2 <- mean(samp2)

pc = abs(p1 - p2)


df1 <- data.frame(succ = c(sum(samp1), sum(samp2)), 
                  fail = c(sum(!samp1), sum(!samp2)), 
                  trt = c("A","B"))

m1 <- glm(cbind(succ, fail)~ trt, data = df1, family = binomial)
summary(m1)
drop1(m1, test = "LRT")
anova(m1, update(m1, .~. - trt), test = "LRT")


# tests if smok is significantly higher than the reference
# like this:
p1 = 351/605
p2 <- 41/195

pc = p1 - p2
SE = sqrt((p2*(1-p2)) * (1/195))

pc/SE









SE = sqrt( (pc*(1-pc)) * (1/length(samp1) + 1/length(samp2)))
SE


stattt <- (pc) / (SE )

pnorm(stattt, lower.tail = FALSE) * 2



tb = (rbind(table(samp1), table(samp2)))

tb = tb[, 2:1]


prop.test(tb, correct = FALSE)

binom.test(x = sum(samp1), n = length(samp1))


n = length(samp1)
SE = sqrt(p1 * (1-p1) / n )+  0.5/n


abs((p1 - 0.5)/SE)
pnorm(abs((p1 - 0.5)/SE), lower.tail = FALSE) * 2


# exact binomial test, using binomial pmf

sum(samp1)
mean(samp1)

# exact test
pbinom(sum(samp1), size = length(samp1), prob = 0.5) * 2

plot(dbinom(1:400, size = 400, prob = 0.5))


# randomization test

sampsum <- c(samp1, samp2)
hist(sampsum)

# resample (bootstrap) to get a dist of means
perm <- function(o){
     reshuff = sample(sampsum, replace = TRUE)
     s1New <- reshuff[1:length(samp1)]
     s2New <- reshuff[(length(samp1) + 1):length(reshuff)]
     return(mean(s1New) - mean(s2New))
}

perMeans <- replicate(100000, perm())
hist(perMeans)
abline(v = (mean(samp1) - mean(samp2)))
mean(perMeans < (mean(samp1) - mean(samp2))) * 2



phat = 0.01

prior = 0.

phat* (1-phat) / (0.005 / 1.96)^2


1.96 * sqrt(phat* (1-phat) / 100)

(samp1 = rbinom(n = 153, size = 1, prob = 0.001))

phat + 1.96*sqrt(0/153)


# if n is > 1000, then there sin't as much as a problem
n = 1000

CICalc = function(ii){
     samp = rbinom(n = 1, size = n, prob = ii)
     phat = samp / n
     
     CI = c(phat - 1.96 * sqrt(phat * (1-phat)/n), 
            phat + 1.96 * sqrt(phat * (1-phat)/n))
     ii > CI[1] & ii < CI[2]
}




mean(replicate(n = 1000, CICalc(0.1)))

aa = numeric()
for(ii in seq(0.1, 0.9, by = 0.05)){
     # simulate CI
     aa <- append(aa, mean(replicate(n = 10000, CICalc(ii))))
}
plot(x = seq(0.1, 0.9, by = 0.05), y = aa, type = 'l')
abline(h = 0.95, lty = 2)



# calculate bayes credible interval and look at coverage
plot(x = seq(0, 1, length = 100), y = dbeta(x = seq(0, 1, length = 100), shape1 = a0, shape2 = b0), type = 'l')

a0 = phat * n / 50
b0 = (n - phat * n) / 50
bayesCI <- function(ii, n){
     samp = rbinom(n = n, size = 1, prob = ii)
     phat = mean(samp)
     
     a = phat * n
     b = n - (phat * n)
     a / (a + b)
     
     
     a0 = phat * 10
     b0 = 10 - (phat) * 10
     
     a0 / (a0 + b0)
     
     high = qbeta(0.975, shape1 = a0 + a , shape2 =  b0 + n -a)
     low = qbeta(0.025, shape1 = a0 + a, shape2 =  b0 + n -a)
     # print(c(low, high))
     ii > low & ii < high
}

ii = 0.1
bayesCI(ii, 200)


aa = numeric()
for(ii in seq(0.1, 0.9, by = 0.05)){
     # simulate CI
     
     aa <- append(aa, mean(replicate(n = 10000, bayesCI(ii, 200))))
}



plot(x = seq(0.1, 0.9, by = 0.05), y = aa, type = 'l')
abline(h = 0.95, lty = 2)



aa = numeric()
for(ii in seq(0, 1, length.out = 100)){
     aa = append(aa, sqrt(ii * (1-ii) / 100))
}
aa
plot(aa)




# simulate censored datat

dta <- rnorm(mean = 110000, n = 5000, sd = 10000)
hist(dta)


dtaTrunc = dta[dta < 100000]
hist(dtaTrunc)


# now, we want to estimate the mean
mean(dtaTrunc)
sd(dtaTrunc)



# simulation study to find CI for mean of actual distribution


n = 5000

#grid search


aa <- data.frame()
for(kk in seq(4000, 15000, length.out = 10)){
     for (ii in seq(90000, 110000, length.out =  100)){
          # simulate a distribution
          dist = rnorm(n = 5000, mean = ii, sd = kk )
          
          # truncate at 100000
          dist <- dist[dist < 100000]
          
          # calculate mean and sd
          aa <- rbind(aa, c(kk, ii, mean(dist), sd(dist)))
          
     }
}

aa$distMean = abs(aa[,3] - mean(dtaTrunc))
aa$distSD = abs(aa[,4] - sd(dtaTrunc))

aa$totDist = aa$distMean + aa$distSD

aa[which.min(aa$totDist), ]


# zero-in 
aa <- data.frame()
for(jj in 1:10){
     for(kk in seq(8000, 12000, length.out = 10)){
          for (ii in seq(105000, 120000, length.out =  100)){
               # simulate a distribution
               dist = rnorm(n = 5000, mean = ii, sd = kk )
               
               # truncate at 100000
               dist <- dist[dist < 100000]
               
               # calculate mean and sd
               aa <- rbind(aa, c(kk, ii, mean(dist), sd(dist)))
               
          }
     }
} 

aa$distMean = abs(aa[,3] - mean(dtaTrunc))
aa$distSD = abs(aa[,4] - sd(dtaTrunc))

aa$totDist = aa$distMean + aa$distSD

aa = aa[order(aa[,7]), ]
colMeans(aa[1:10, ])
aa[1:10,]


sd(dtaTrunc)
mean(dtaTrunc)

bb <- data.frame()
for(ii in 1:100){
     # simulate a distribution
     dist = rnorm(n = 5000, mean = 110000, sd = 10000)
     
     # truncate at 100000
     dist <- dist[dist < 100000]
     
     # calculate mean and sd
     bb <- rbind(bb, c(mean(dist), sd(dist)))
     
}
bb
colMeans(bb)
c(mean(dtaTrunc), sd(dtaTrunc))


mu = mean(dtaTrunc)
sigg = var(dtaTrunc)


mu = colMeans(bb)[1]
sigg = colMeans(bb)[2]
a = 0
b = 100000
sigg = colMeans(bb)[2]

mu = 113000
sigg = 10620

expectedX = mu -  sigg * dnorm((b - mu)/sigg)/pnorm((b - mu)/sigg)
expectedX


xx = rnorm(100)
x2 = rnorm(100)

yy = xx + x2 + rnorm(100)
plot(yy~xx)
plot(yy~x2)


m1 <- lm(yy~xx)
m2 <- lm(yy ~xx + x2)
anova(m1, m2)
logLik(m1)
logLik(m2)
deviance(m1)
deviance(m2)


# optim for truncated dist'n



# simulation for power

# say you want to know if your product will incease sales by 5%

# you know sales are already around 30%
2216.7 / 104960

n = 1000
sampSizeCal <- function(n, diff = 0.01){
     initialProb = 0.01
     samp1 <- rbinom(n = n, size = 1, prob = initialProb)
     samp2 <- rbinom(n = n, size = 1, prob = initialProb + diff)
     mean(samp1) - mean(samp2)
     ppooled = mean(mean(samp1), mean(samp2))
     SEPooled = sqrt(ppooled * (1-ppooled) * (1/n + 1/n))
     abs(mean(samp1) - mean(samp2)) / SEPooled * 2 > qnorm(0.975)
}
 

pwr = numeric()   
for (ii in 1:10){
     pwr = append(pwr, mean(replicate(10000, sampSizeCal(n = 100*ii))))
}
plot(x = 1:10 * 100, pwr, type = 'l')



sigmax = 7.5
sigmay = 4
estSlope = 0.5



n = 10
aa = numeric()
for(ii in 1:100){
     x = rnorm(n = n, mean = 0, sd = (sigmax))
     # hist(x)
     # sd(x)
     
     y = estSlope * x + rnorm(n, mean = 0, sd = sigmay)
     # plot(y ~x)
     aa = append(aa, summary(lm(y~x))$coefficients[2,4] < 0.05)
}
plot(y~x)
mean(aa)
