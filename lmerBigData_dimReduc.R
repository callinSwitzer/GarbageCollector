# train = file.choose()

#tt = read.csv("/Users/callinswitzer/Dropbox/MachineLearning/p3Data/train.csv")
ttab = tapply(INDEX = tt$user, X = tt$plays, median)
head(ttab)

ttdf <- data.frame(plays = ttab, user = names(ttab))
rownames(ttdf) <- NULL
head(ttdf)
summary(ttdf)

artistMeanPlays = read.csv("/Users/callinswitzer/Documents/GitRepos/CS181Regression/p3/ArtistMeanPlays.csv")
head(artistMeanPlays)

artistMeanPlays$scaledArtist = as.numeric(scale(artistMeanPlays$plays, center = TRUE, scale = TRUE))
colnames(artistMeanPlays)[2:3] = c("meanPlaysPerArtist", "artistMeanPlays_scaled")
head(artistMeanPlays)

hist(artistMeanPlays$artistMeanPlays_scaled)


test = read.csv("/Users/callinswitzer/Dropbox/MachineLearning/p3Data/test.csv")
summary(test)
colnames(test)

tsm <- test[1:100, ]

submit <- merge(x = test, y = ttdf, by= 'user')
head(submit)

submit2 <- merge(x= submit, y = artistMeanPlays, by = 'artist')
head(submit2)
nrow(submit2)

submit2$newPred = submit2$plays + submit2$artistMeanPlays_scaled

# here's another method
submit_sm = submit2[, c('Id', 'newPred')]
head(submit_sm)

colnames(submit_sm)[2] = "plays"



submit_sm <- submit_sm[order(submit_sm$Id), ]
head(submit_sm)
submit_sm$plays[submit_sm$plays < 0] = 0

sum(submit_sm$plays < 0)

# beat the baseline: refref -- move to Python!
write.csv(submit_sm, "/Users/callinswitzer/Dropbox/MachineLearning/p3Data/Sub4_IdMeansAdjArtist3.csv", row.names = FALSE)

bb = read.csv("/Users/callinswitzer/Dropbox/MachineLearning/p3Data/Sub4_IdMeansAdjArtist2.csv")
sum(bb$plays < 0 )


tt = read.csv("/Users/callinswitzer/Documents/GitRepos/CS181Regression/p3/TrainSmall.csv")

aa = rnorm(100, mean = 100)
bb = rnorm(100, mean = 100)*(3)

sd(aa)
sd(bb)

sd(aa) / mean(aa)
sd(bb) / mean(bb)

aa = sample(c(0,1), replace = TRUE, size = 100, prob = c(0.1, 0.9))
mean(aa)


bb = sample(c(0,1), replace = TRUE, size = 100, prob = c(0.5, 0.5))
mean(bb)

sd(aa) / mean(aa)
sd(bb) / mean(bb)

hist(rnorm(n = 1000, mean = mean(aa), sd = sqrt(mean(aa) * (1-mean(aa)))))
hist(rnorm(n = 1000, mean = mean(bb), sd = sqrt(mean(bb) * (1-mean(bb)))), add = TRUE, col = 'red')


aa = rpois(100, lambda = 50)

sd(aa)^2 / mean(aa)
hist(aa)

bb = rpois(100, lambda = 1)
sd(bb)^2 / mean(bb)

hist(bb)


hist(bb)

#tt = read.csv("train.csv")
#user = file.choose()
profiles = read.csv("/Users/callinswitzer/Dropbox/MachineLearning/p3Data/profiles.csv")
head(profiles)

## edit profiles
# edit data:
profiles$age[profiles$age < 0 | profiles$age > 100] = NA

profiles$ageNA = is.na(profiles$age)
profiles$age[profiles$ageNA] = mean(profiles$age, na.rm = TRUE)
summary(profiles)

profiles$sex <- as.factor(profiles$sex)

library(plyr)
profiles$sex <- mapvalues(profiles$sex, from = "", to = "NA")

nrow(profiles)

# get average num of plays per user
userAvgs = tapply(tt$plays, INDEX = tt$user, FUN = median)
userAvgs[1:10]


X = profiles


mm = model.matrix(~X$sex + X$country + X$ageNA + X$age)
colSums(mm)

ppc = prcomp(mm[,-1], center = TRUE, scale = TRUE)

raw <- ppc$x[,1:2]
plot(raw[1:1000,1], raw[1:1000,2])
kmc <- kmeans(raw, 10, iter.max = 10000)
plot(raw[1:1000,1], raw[1:1000,2], col = kmc$cluster, pch = 20)

profiles$cluster = kmc$cluster

trainFull = merge(tt, y = profiles[, c("user", "cluster")], by = "user")
head(trainFull)

mod1 <- glm(plays ~ as.factor(cluster), data = trainFull, family = poisson())
summary(mod1)

trainFull$preds <- predict(mod1, type = 'response')
head(trainFull)

#profiles = read.csv("profiles.csv")

tt$uaPair <- NULL
tt$X <- NULL
library(lme4)


head(tt)
head(profiles)


# merge profiles with training set
trainFull = merge(tt, profiles, by = "user")
nrow(trainFull)

summary(trainFull)

# edit data:
trainFull$age[trainFull$age < 0 | trainFull$age > 100] = NA

trainFull$ageNA = is.na(trainFull$age)
trainFull$age[trainFull$ageNA] = mean(trainFull$age, na.rm = TRUE)
summary(trainFull)

trainFull$sex <- as.factor(trainFull$sex)

library(plyr)
trainFull$sex <- mapvalues(trainFull$sex, from = "", to = "NA")

nrow(trainFull)


head(trainFull)
#mod1 <- glmer(plays ~ (1|artist) + (1|sex) + (1|age) + (1|ageNA) + (1|user), family = "poisson", data = trainFull)

X = droplevels(trainFull[, colnames(trainFull) %in% c("sex", "age", "country", "ageNA")])
head(X)
table(X$country)
mm = model.matrix(~X$sex + X$country + X$ageNA + X$age)
colSums(mm)

ppc = prcomp(mm[,-1], center = TRUE, scale = TRUE)

raw <- ppc$x[,1:2]
plot(raw[,1], raw[,2])

# install.packages("speedglm")
# library(speedglm)

# sglm = speedglm(plays ~ age + user + artist, family = poisson('log'), data = trainFull)
head(X)

kmc <- kmeans(raw, 2, iter.max = 10000)
kmc$cluster

plot(raw[,1], raw[,2], col = kmc$cluster, pch = 20)

library(cluster)
# dd = daisy(X)
# gg = hclust(dd)
# gps = cutree(gg, k = 20)
# trainFull$userGp = as.factor(gps)

# install.packages('sparcl')
library(sparcl)
# colors the leaves of a dendrogram
y = gps
ColorDendrogram(hc, y = y, labels = names(y), main = "jj", 
                branchlength = 80)


mod_glm <- glm(plays ~ userGp + sex, family = poisson, data = trainFull)
summary(mod_glm)

preds = predict(mod_glm, type = 'response')
trainFull$preds = preds
head(trainFull)
