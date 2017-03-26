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
