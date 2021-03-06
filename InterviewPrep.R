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
var1 = rep(c('a','a','a','c','e',NA), 10)
var2 = rep(c('p1','p1','p1','p2','p3','p1'), 10)
var3 = rep(c('o1','o1','o1','o2','o3','o1'), 10)
df = data.frame('v1'=var1,'v2'=var2,'v3'=var3)
head(df, 15)


## Multinomial imputation
library(nnet)
# fit multinomial model on only complete rows
imputerModel = multinom(v1 ~ (v2+ v3), data = df[!is.na(df$v1), ])

summary(imputerModel)

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



### FUZZY Matching
company <- read.table(text = "
          CompanyName
          'MERCK SHARP & DOHME CORPORATION'
          'GILEAD SCIENCES INC'
          'BOEHRINGER INGELHEIM PHARMACEUTICALS, INC.'
          'ABBVIE, INC.'
          'JANSSEN SCIENTIFIC AFFAIRS, LLC'
          'BOEHRINGER INGELHEIM PHARMA GMBH & CO.KG'
          'ASAHI INTECC CO., LTD.'
          'Asahi Intecc USA Inc'
", header = TRUE, stringsAsFactors = FALSE)


distMatrix = adist(company$CompanyName)

# hclust
plot(hclust(as.dist(distMatrix), method = "single"), labels = company$CompanyName)


# how to scrape HTML Table
library("rvest")
url <- "https://www.epa.gov/wqc/national-recommended-water-quality-criteria-human-health-criteria-table"
table_list <- url %>%
  read_html() %>%
  # I copied
  html_nodes(xpath='/html/body/section/div[2]/div[1]/div/div/table') %>%
  html_table() 

html_table = table_list[[1]]
head(html_table[, 1:2])





aa = c(1,2,2,2,2,4,4,4,4,4,4)
bb = 1:length(aa)
plot(bb, aa)
a2 = as.ordered(as.factor(aa))


summary(lm(bb ~ a2))


a3 = factor(a2, ordered = FALSE)
a3

cc = data.frame(a2, bb)
cc
cc$a2
cc$a2 <- factor(cc$a2, ordered = FALSE)
cc$a2


load("C:\\Users\\calli\\Desktop\\wavedata.rda")
plot(x = wavedata$DateTime[1:2500], y = wavedata$swDepth.m[1:2500], type = "l")

fft_calculator = function(timeSignal, log_rate){
  # calculate fft
  n = length(timeSignal)
  Y = fft(timeSignal)/n
  amplitudeAndPhase = Y[(1:(n/2))]*2 # these are complex numbers
  
  # calculate fft frequencies
  k = 0:n
  Ts = n/log_rate
  frq = k/Ts # two side frequency range
  frq = frq[1:(n/2)] # one side frequency range
  
  return(list('amplitudeAndPhase' = amplitudeAndPhase, "frq" =  frq, "Y" = Y, "frq2" = k/Ts))
}


fft_data = fft_calculator(wavedata$swDepth.m[1:2500] - mean(wavedata$swDepth.m[1:2500]), 4)


amplitudeAndPhase = fft_data[["amplitudeAndPhase"]]
frq = fft_data[["frq"]]

plot(abs(amplitudeAndPhase), x = frq, type = "l", xlab = "freq (cycles per second)", 
     ylab = "amplitude", main = "DFT spectrum", xlim = c(0, 3))

inv_fft = fft(fft_data[["Y"]], inverse = TRUE)
plot(x = wavedata$DateTime[1:2500], y = Re(inv_fft), type = "l")
lines(x = wavedata$DateTime[1:2500], y = wavedata$swDepth.m[1:2500] - mean(wavedata$swDepth.m[1:2500]), col = "red", type = "l")

####################### PREDICT THE FUTURE ###############
initialVal = 0
xx2 = seq(0, 7*60, by = 0.25)

for (ii in 1:length(fft_data[["Y"]])){
  initialVal = initialVal + fft_data[["Y"]][ii] * cos(2*pi*xx2*fft_data[["frq2"]][ii] - 
                                                        atan(Im(fft_data[["Y"]][ii])/Re(fft_data[["Y"]][ii])))
}

plot(wavedata$swDepth.m[1:1500] - mean(wavedata$swDepth.m[1:1500]), type = "l")
lines(Re(inv_fft), type = "l", col = "red")
lines(Re(initialVal), type = "l", col = "blue")


lines(wavedata$swDepth.m[1:1500]- mean(wavedata$swDepth.m[1:1500]), type = "l", col = "red")

inv_fft = fft(fft_data[["Y"]], inverse = TRUE)
lines(Re(inv_fft), col= 'red', type = "l")



####################
library(signal)
xx = seq(0, 10.5, by = 0.001)
yy = 3 * sin(2 * pi * xx * 1.4 + (90 / 360) * (2*pi))  +  3 * sin(3 * pi * xx * 3)
yy = hanning(length(yy))*yy
plot(xx, yy, type = "l")




fft_data = fft_calculator(yy, 1000)


amplitudeAndPhase = fft_data[["amplitudeAndPhase"]]
frq = fft_data[["frq"]]

plot(abs(amplitudeAndPhase), x = frq, type = "l", xlab = "freq (cycles per second)", 
     ylab = "amplitude", main = "DFT spectrum", xlim = c(0, 100))

#plot(abs(fft_data[["Y"]]), type = "l")

#fft_data[["Y"]][abs(fft_data[["Y"]]) < 0.9] = 0

which.max(abs(fft_data[["Y"]]))
initialVal = 0
xx2 = seq(0, 8, by = 0.001)

for (ii in 1:length(fft_data[["Y"]])){
  phase = atan2(Im(fft_data[["Y"]][ii]), Re(fft_data[["Y"]][ii]))
  initialVal = initialVal + abs(fft_data[["Y"]][ii]) * cos(2*pi*xx2*fft_data[["frq2"]][ii] + phase)
}

plot(yy, type = "l", col = "red", xlim = c(0,10)*1000)
points(Re(initialVal), pch = "."); lines(yy, col = "red", lwd = 2)


inv_fft = fft(fft_data[["Y"]], inverse = TRUE)
lines(Re(inv_fft), col= 'blue', type = "l")
yy2 = 3 * sin(2 * pi * xx2 * 1.4 + (90 / 360) * (2*pi))  +  3 * sin(3.3 * pi * xx2 * 3)
lines(5000:length(yy2), yy2[5000:length(yy2)], col = "red")




below_cutoff = abs(fft_data[["Y"]]) < 1
fft_data[["Y"]][below_cutoff] = 0
inv_fft = fft(fft_data[["Y"]], inverse = TRUE)
plot(Re(inv_fft), col= 'blue', type = "l")
lines(yy, type = "l", col = "red")

plot(abs(fft_data[["Y"]]), type = "l")
#####################################################
xx = seq(0, 10, by = 0.01)
y = function(xx){
  3 * sin(2 * pi * xx * 1.4 + (45 / 360) * (2*pi))# +  
    #3 * sin(2 * pi * xx * 3) 
}
yy = y(xx) + rnorm(length(xx))
plot(xx, yy, type = "l")


m1 = lm(yy ~ xx + sin(2*pi*xx*1.4)+cos(2*pi*xx*1.4))
xplot = seq(10, 12, by = 0.01)
plot(xx, yy, type = "l", xlim = c(min(xx), max(xplot)))

lines(xplot, predict(m1, newdata = data.frame(xx = xplot)), col= 'red')
lines(xplot, y(xplot), col= 'red')



library(forecast)
m1 = auto.arima(yy, stationary = TRUE, )
plot(forecast(m1,h=500))



m2 = Arima(yy, c(10, 0, 2))
plot(forecast(m2,h=500))
lines(x = 1001:(1000 + length(xplot)), y = y(xplot), col = "red")


#################################################################
SSTlm2 <- lm(yy ~ xx + sin(2*pi*xx)+cos(2*pi*xx) + sin(4*pi*xx)+cos(4*pi*xx) + sin(6*pi*xx)+cos(6*pi*xx))
summary(SSTlm2)
plot(xx, yy)
lines(xx, SSTlm2$fitted.values)








#######################
t = xx = seq(0, 2, by = 0.001)
y = 3 * sin(2 * pi * xx * 1.4 + (90 / 360) * (2*pi))#  +  3 * sin(2 * pi * xx * 3) + 0.3 * sin(2 * pi * xx * 30)
t = xx = 1:length(y)
plot(xx, y, type = "l")

ssp <- spectrum(y)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(y ~ xx +  sin(2*pi*xx/per)+cos(2*pi*xx/per))
summary(reslm)

rg <- diff(range(y))
plot(y~xx,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
lines(fitted(reslm)~xx,col=4,lty=2)   # dashed blue line is sin fit

# including 2nd harmonic really improves the fit
reslm2 <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm2)
lines(fitted(reslm2)~t,col=3)    # solid green line is periodic with second 




Time = xx = seq(0, 2, by = 0.001)
temperature = 3 * sin(2 * pi * xx * 1.4 + (90 / 360) * (2*pi))#  +  3 * sin(2 * pi * xx * 3) + 0.3 * sin(2 * pi * xx * 30)
Time = 1:length(temperature)

xc<-cos(2*pi*Time/366)
xs<-sin(2*pi*Time/366)
fit.lm <- lm(temperature~xc+xs)

# access the fitted series (for plotting)
fit <- fitted(fit.lm)  

# find predictions for original time series
pred <- predict(fit.lm, newdata=data.frame(Time=Time))    

plot(temperature ~ Time)
lines(fit, col="red")
lines(Time, pred, col="blue")




xx = seq(0, 5, length.out = 1000)
Degrees = 3 * sin(2*pi*0.6*xx)

library(forecast)

Degrees.ts <- ts(Degrees, start=c(2010,1), frequency=12)

Degree.trend <- auto.arima(Degrees.ts)

degrees.forecast <- forecast(Degree.trend, h=12, level=c(80,95), fan=F)

plot(degrees.forecast, las=1, main="", xlab="Time", ylab="Degrees")

## plot binomial (100, 0.5)
xx = 0:100

bin <- function(xx){
  choose(100, xx)*0.5^xx*0.5^(100 -xx)
}
fx = sapply(xx, function(xx) bin(xx))
plot(xx, fx)
sum(fx[75:length(fx)])

fxx = dnorm(xx, mean = 50, sd = 5 )
lines(xx, fxx)


8!
8

factorial(8)

choose(8,1) + choose(8,2) + choose(8,3) + choose(8,4) + choose(8,5) + choose(8,6) + choose(8,7) + choose(8,8)
factorial(5)

choose(5,1) + choose(5,2) + choose(5,3) + choose(5,4) + choose(5,5)

factorial(3)
choose(4,1) + choose(4,2) + choose(4,3) + choose(4,4)

combn(letters[1:4],2)
