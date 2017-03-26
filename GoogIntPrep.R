# Power Calculations


# Bootstrap


# hypothesis testing (with proportion, etc.)


# Describe your process for approacing a statistics problem
#. Clean, visualize, engineer features.


# update resume: https://www.linkedin.com/pulse/20140929001534-24454816-my-personal-formula-for-a-better-resume



snail = data.frame(temperature = c(38,40,40,40,42,42,42,44,44,44,47,47,47),
                   alive = c(20,20,20,20,19,14,15,5,3,2,0,0,0),
                   dead = c(0,0,0,0,1,6,5,15,17,18,20,20,20)
)

snail

binom.snail <- glm(cbind(alive, dead) ~ temperature, family = binomial, data = snail)
summary(binom.snail)

# convert into bernoulli format
dataLong <- lapply(1:nrow(snail), function(x){
     foo <- snail[x, ]
     data.frame(temp = rep(foo$temperature, foo$alive + foo$dead), 
                alive = c(rep(1, foo$alive), rep(0, foo$dead)))
})
longDF <- do.call("rbind", dataLong)

bern.snail <- glm(alive ~ temp, family = binomial, data = longDF)
summary(bern.snail)

par(mfrow = c(2,1))
plot(residuals(bern.snail, type = "pearson"))
plot(residuals(binom.snail, type = "pearson"))


perRes = (snail$alive - predict(binom.snail, type = 'response')*20) / sqrt(predict(binom.snail, type = 'response')*20 * (20 - predict(binom.snail, type = 'response')*20) / 20)

plot(perRes)



bern.int <- glm(alive ~ 1, family = binomial, data = longDF)
binom.int <- glm(cbind(alive, dead) ~ 1, family = binomial, data = snail)

# calculate reduction in deviance for bernoulli model
bern.int$deviance - bern.snail$deviance

# calculate reduction in deviance for binomial model
binom.int$deviance - binom.snail$deviance


# The Central Limit Theorem states that whenever a random sample of size n is taken from any distribution with mean and variance σ2, then the sample mean X ̄ will be approximately
# 6
# normally distributed with mean μ and variance σ2/n. The larger the value of the sample size n, the better the approximation to the normal.
# This is very useful when it comes to inference. For example, it allows us (if the sample size is fairly large) to use hypothesis tests which assume normality even if our data appear non-normal. This is because the tests use the sample mean X ̄, which the Central Limit Theorem tells us will be approximately normally distributed.


# find the bad classifier


classes <- as.data.frame(t(sapply(X = 1:100, FUN = function(o) sample(c(0,1), size = 10, replace = TRUE, prob = c(0.2, 0.8)))))

library(magrittr)
classes %>% head

classes <- cbind(classes, V11 = sample(c(0,1), size = 100, replace = TRUE, prob = c(0.8, 0.2)))

hcl = hclust(dist(t(classes)))

plot(hcl)

biplot(prcomp(t(classes)))


rmeans = rowMeans(classes)

sses = list()
for(ii in 1:ncol(classes)){
     resids = classes[ , ii] - rmeans
     sses[ii] = sum(resids^2)
}

plot(unlist(sses))



## How to choose which ad to show a person (based on qualilty and bid)
# Use a web scraper to scrape a few hundred thousand web pages and fit some topic models to create a news recommendation engine.


# power is the probability of rejecting the null hypothesis when the alternative hypothesis is true.

# Power calculations
ttsim = function(m1, m2){
     t.test(rnorm(mean = m1, n = 10), rnorm(mean = m2, n = 10))$p.value
} 



# calc variance of array
arr1 = rnorm(100, 2, sd = 3)
hist(arr1)


mean1 = mean(arr1)

# resample
arr_temp <- sample(arr1, replace = TRUE)

# resample mean function
resampMean <- function(o){
     arr_temp <- sample(arr1, replace = TRUE)
     return(mean(arr_temp))
}

mns = sapply(1:10000, resampMean)
hist(mns)

var(mns)
# 9/100 = 0.09



tt_vec = Vectorize(ttsim)


pvals = sapply(1:1000, function(x) ttsim(100, 101))

hist(pvals)


mean(pvals > 0.05)

