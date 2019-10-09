<<<<<<< HEAD
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
#x_to_predict = data.frame(xx = 34.5)
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







## FOREST PLOT

install.packages("forestplot")
library(forestplot)

df <- structure(list(
  mean = c(NA, 0.22, 0.20, 0.27),
  lower = c(NA, 0.05, 0.04, 0.01),
  upper = c(NA, 0.95, 1.08, 9.12)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -4L),
  class = "data.frame")

tabletext <- cbind(
  c("", "Pooled", "Group 1", "Group 2"),
  c("N", "4334", "3354", "980"),
  c("HR (95% CI)", "0.22 (0.05, 0.95)", "0.20 (0.04, 1.08)", "0.27 (0.01, 9.12)"),
  c("p-value", "0.042", "0.061", "0.467")
)

ggfp <- forestplot(tabletext,
                   df,
                   new_page = TRUE,
                   is.summary = c(TRUE, rep(FALSE, 3)),
                   clip = c(0, 2),
                   colgap = unit(5, "mm"),
                   line.margin = unit(2, "mm"),
                   lineheight = unit(1, "in"),
                   txt_gp = fpTxtGp(label = gpar(cex = 1),
                                    ticks = gpar(cex = 1)),
                   align = c("l", "c", "c", "c"),
                   boxsize = 0.2,
                   xticks = seq(0, 2.0, 0.5), 
                   zero = 1,
                   col = fpColors(box = "royalblue",
                                  line = "darkblue"),
                   mar = unit(c(-1, 0.5, -2, 0.5), "in"))

ggfp

tiff("C:/Users/calli/Documents/GitRepos/GarbageCollector/forestplot.tiff", units = "in", width = 9, height = 7, res = 300)
forestplot(tabletext,
           df,
           new_page = TRUE,
           is.summary = c(TRUE, rep(FALSE, 3)),
           clip = c(0, 2),
           colgap = unit(5, "mm"),
           line.margin = unit(2, "mm"),
           lineheight = unit(1, "in"),
           txt_gp = fpTxtGp(label = gpar(cex = 1),
                            ticks = gpar(cex = 1)),
           align = c("l", "c", "c", "c"),
           boxsize = 0.2,
           xticks = seq(0, 2.0, 0.5), 
           zero = 1,
           col = fpColors(box = "royalblue",
                          line = "darkblue"),
           mar = unit(c(-1, 0.5, -2, 0.5), "in"))
dev.off()
=======
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


summary(m1)

b0 = -3.669082
b1 = 0.075185 

p1 = (exp(b0 + b1*x_to_predict$xx) / (1 + exp(b0 + b1*x_to_predict$xx)))*max(yy)



# predict
preds_glm = predict(m1, 
                newdata = x_to_predict, 
                type = "response")


plot(p1, preds_glm)



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
choose(1000, 0)*(1/1000)^0*(999/1000)^1000



## Calculate a CI for proportion
set.seed(123)
data = c(1,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1, 1)
table(data)
hist(data)
mean(data)

# This uses the chisq distribution
prop.test(sum(data == 1), length(data), correct = FALSE)

# this is an exact test
binom.test(sum(data == 1), length(data))


# this is right for calculating a p-value for the exact test
(dbinom(15, length(data), 0.5) + 
  dbinom(16, length(data), 0.5) + 
    dbinom(17, length(data), 0.5)  )*2


# this is also right 
# notice that q = 14 instead of 15 here, because it's p > x instead of p >= x
pbinom(q = 14, size = 17, prob = 0.5, lower.tail = FALSE)*2


# CI for prortion based on normal dist'n
phat = 15/17
sd_p = sqrt(phat*(1-phat))

p_U = phat + 1.96*sd_p/sqrt(17)
p_L = phat - 1.96*sd_p/sqrt(17)
c(p_L, p_U)


# CI based on GLM
mod1 = glm(c(15, 2) ~ 1, family = binomial)



binom.test(7, 12, p = 0.75, alternative = "two.sided")

# this is how the binomial test is calculated (instead of just multiplying by 2)
pbinom(7, 12, 0.75) + 1-pbinom(11, 12, 0.75)


(  dbinom(7, 12, 0.75) + 
  dbinom(6, 12, 0.75) + 
  dbinom(5, 12, 0.75) + 
  dbinom(4, 12, 0.75) + 
  dbinom(3, 12, 0.75) + 
  dbinom(2, 12, 0.75) + 
  dbinom(1, 12, 0.75) + 
  dbinom(0, 12, 0.75) + 
    dbinom(12, 12, 0.75))


plot(dbinom(1:12, 12, 0.75))



##################
choose(4,4) * choose(47,3) / choose(52, 4)
5/48  


install.packages("DMwR")
library(DMwR)

var1 = rep(c('a','a','a','c','e',NA), 10**6)
var2 = rep(c('p1','p1','p1','p2','p3','p1'), 10**6)
var3 = rep(c('o1','o1','o1','o2','o3','o1'), 10**6)

df = data.frame('v1'=var1,'v2'=var2,'v3'=var3)
df

knnOutput <- DMwR::knnImputation(df, k = 5) 
knnOutput

##### Multinomial logistic regression

# make repeating vectors of 6M rows each
var1 = rep(c('a','a','a','c','e',NA), 10**6)
var2 = rep(c('p1','p1','p1','p2','p3','p1'), 10**6)
var3 = rep(c('o1','o1','o1','o2','o3','o1'), 10**6)
df = data.frame('v1'=var1,'v2'=var2,'v3'=var3)
head(df, 15)


library(nnet)
# fit multinomial model on only complete rows
imputerModel = multinom(v1 ~ (v2+ v3)^2, data = na.omit(df))

# predict missing data
system.time({
  predictions = predict(imputerModel, newdata = df[is.na(df$v1), ])
})
# fill in predictions
df$multinom_preds[is.na(df$v1)] = as.character(predictions)
head(df, 15)



############# random forest imputation --- TOO BIG, must store all data
# make repeating vectors of 6M rows each
var1 = rep(c('a','a','a','c','e',NA), 10**6)
var2 = rep(c('p1','p1','p1','p2','p3','p1'), 10**6)
var3 = rep(c('o1','o1','o1','o2','o3','o1'), 10**6)
df = data.frame('v1'=var1,'v2'=var2,'v3'=var3)
head(df, 15)


# library(randomForest)
# trainSet <- na.omit(df)
# predictSet <- df[is.na(df$v1), ]
# 
# rfModel <- randomForest(v1 ~ ., data = trainSet, importance = FALSE)


############ Naive Bayes
var1 = rep(c('a','a','a','c','e',NA), 10**6)
var2 = rep(c('p1','p1','p1','p2','p3','p1'), 10**6)
var3 = rep(c('o1','o1','o1','o2','o3','o1'), 10**6)
df = data.frame('v1'=var1,'v2'=var2,'v3'=var3)
head(df, 15)

library(e1071)
trainSet <- na.omit(df)
predictSet <- df[is.na(df$v1), ]
Naive_Bayes_Model=naiveBayes(v1 ~ ., data = trainSet)
system.time({
  Naive_Bayes_Predictions = predict(Naive_Bayes_Model, newdata = predictSet)
})
Naive_Bayes_Predictions

df$Naive_Bayes_Predictions[is.na(df$v1)] = as.character(Naive_Bayes_Predictions)
head(df, 15)


###########################################################################
### Impute missing categories
###########################################################################

# make data with 6M rows
var1 = rep(c('a','a','a','c','e',NA), 10**6)
var2 = rep(c('p1','p1','p1','p2','p3','p1'), 10**6)
var3 = rep(c('o1','o1','o1','o2','o3','o1'), 10**6)
df = data.frame('v1'=var1,'v2'=var2,'v3'=var3)
head(df, 15)


## Multinomial imputation
library(nnet)
# fit multinomial model on only complete rows
imputerModel = multinom(v1 ~ (v2+ v3)^2, data = df[!is.na(df$v1), ])

# predict missing data
predictions = predict(imputerModel, newdata = df[is.na(df$v1), ])


#### Naive Bayes

library(naivebayes)
library(fastDummies)
# convert to dummy variables
dummyVars <- fastDummies::dummy_cols(df, 
                                     select_columns = c("v2", "v3"), 
                                     ignore_na = TRUE)
head(dummyVars)

# create training set
X_train <- na.omit(dummyVars)[, 4:ncol(dummyVars)]
y_train <- na.omit(dummyVars)[, "v1"]

X_to_impute <- dummyVars[is.na(df$v1), 4:ncol(dummyVars)]


Naive_Bayes_Model=multinomial_naive_bayes(x = as.matrix(X_train), 
                                          y = y_train)

# predict missing data
Naive_Bayes_preds = predict(Naive_Bayes_Model, 
                                  newdata = as.matrix(X_to_impute))


# fill in predictions
df$multinom_preds[is.na(df$v1)] = as.character(predictions)
df$Naive_Bayes_preds[is.na(df$v1)] = as.character(Naive_Bayes_preds)
head(df, 15)




# v1 v2 v3 v2_p1 v2_p2 v2_p3 v3_o1 v3_o2 v3_o3
# 1    a p1 o1     1     0     0     1     0     0
# 2    a p1 o1     1     0     0     1     0     0
# 3    a p1 o1     1     0     0     1     0     0
# 4    c p2 o2     0     1     0     0     1     0
# 5    e p3 o3     0     0     1     0     0     1
# 6 <NA> p1 o1     1     0     0     1     0     0






xx = 1:100
yy = xx + rnorm(100)
m1 <- lm(yy ~ xx)
summary(m1)
confint(m1, level = 0.9)

# use 0.05 since it's a two-sided test
qt(p = 0.05, df = 43, lower.tail = FALSE)

summary(m1)$coef[2,2]

summary(m1)$coef[2,1] + qt(p = 0.05, df = 98, lower.tail = FALSE)*summary(m1)$coef[2,2]
summary(m1)$coef[2,1] - qt(p = 0.05, df = 98, lower.tail = FALSE)*summary(m1)$coef[2,2]
confint(m1, level = 0.9)

1.002615 - qt(p = 0.05, df = 43, lower.tail = FALSE)*0.003609


