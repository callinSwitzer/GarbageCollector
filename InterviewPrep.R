# plot exponential distribution

x = seq(0, 2, by = 0.01)
sum(x)
yy = dexp(x, rate = 3.5)

plot(x, yy, "l")

# calculate expected value
sum(x*yy)/100

# check 
1/3.5


hist(rexp(10000, rate = 3.5), freq = FALSE)
x = seq(0, 5, length.out = 1000)
yy = dexp(x, rate = 3.5)
lines(x, yy, "l")

mean(rexp(10000, rate = 3.5))



# plot normal distribution

x = seq(-4, 8, by = 0.01)
yy = dnorm(x, mean = 3, sd = 1)

plot(x, yy, "l")

# calculate expected value
sum(x*yy)/100

### calculate MLE for exponential dist'n
data = c(3,3,5,7,8)
MLE = length(data)/sum(data)
MLE

hist(data)
mean(data)
1/MLE



#  make example data
xx = rep(c(30, 50, 70, 100), each = 10)
yy = 1/(1+exp(-(xx-50)/15))  * 4798.20 + rnorm(length(xx), sd = 50)
xx = c(0, xx)
yy = c(0, yy)


# fit third-order linear model
m0 = lm(yy ~ I(xx^3) + I(xx^2) + xx)

x_to_predict = data.frame(xx = seq(0, 100, length.out = length(xx)))
lm_preds = predict(m0, newdata = x_to_predict)

# fit quasibinomial model for proportion
m1 = glm(I(yy/max(yy)) ~ xx , family = quasibinomial())

# predict
preds_glm = predict(m1, 
                newdata = x_to_predict, 
                type = "response")

# fit Generalized Additive Model
library(mgcv)
# you have to tune "k" somewhat -- larger means more "wiggliness"
m2 = gam(yy ~ s(xx, k = 4)) 
gam_preds = predict(m2, 
                    newdata = x_to_predict, 
        type = "response")

# plot data and predictions
plot(xx, yy, ylab = "result", xlab = "efficiency")
lines(x_to_predict$xx, 
      preds_glm*max(yy), "l", col = 'red', lwd = 2)
lines(x_to_predict$xx, 
      gam_preds, "l", col = 'blue', lwd = 2)
lines(x_to_predict$xx, lm_preds, 
      "l", col = 'black', lwd = 2, lty = 2)
legend("bottomright", 
       lty = c(0, 1, 1, 2), 
       legend = c("data", "GLM prediction", "GAM prediction", "third-order lm"), 
       pch = c(1, NA_integer_, NA_integer_, NA_integer_), 
       col = c("black", "red", "blue", "black"))



#install.packages("betareg")
library (betareg)
yScaled = yy/max(yy + 1)
max(yScaled)


betaMod <- betareg(yScaled ~ xx) # train model. Tune var names.
summary(betaMod) # model summary
preds = predict(betaMod,  newdata = data.frame(xx = seq(0, 100, length.out = length(xx))), type = "response")
plot(xx, yy)
lines(seq(0, 100, length.out = length(xx)), preds*max(yy + 1))


ggplot(data = df, aes(x = xx, y = yy/max(yy))) +
  geom_point() + 
  binomial_smooth()





# Probability question. There is a group of 1000 people. Each person is independent, and they choose a random number between 1 and 1000. Then a company chooses one number between 1 and 1000.  What is the probability that no one in the group of people chooses the same number as the company?
#   My answer (999/1000) ^ 1000 


rand_draw <- function(nums = 1000){
  samp = sample.int(n= nums, size = nums, replace = TRUE)
  company_draw = sample.int(n= nums, size = 1, replace = TRUE)
  return(!(company_draw %in% samp))
}

reps = replicate(1000, rand_draw())
mean(reps)

# answer 
(999/1000) ^ 1000

# actual answer, based on binomial distribution Bin(n = 1000, p = 1/1000)
choose(1000, 0)*(1/1000)^0*(999/1000)^999

