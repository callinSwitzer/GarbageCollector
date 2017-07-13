# train = file.choose()

#tt = read.csv("/Users/callinswitzer/Dropbox/MachineLearning/p3Data/train.csv")

tt = read.csv("/Users/callinswitzer/Documents/GitRepos/CS181Regression/p3/TrainSmall.csv")


#tt = read.csv("train.csv")
#user = file.choose()
profiles = read.csv("/Users/callinswitzer/Dropbox/MachineLearning/p3Data/profiles.csv")
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

#install.packages("biglm")
library(biglm)



smDS <- trainFull[sample(x = 1:nrow(trainFull), replace = FALSE, size = 1000), ]
nrow(smDS)
smDS = droplevels(smDS)

length(levels(smDS$artist))

length(unique(smDS$artist))

mod_bglm = bigglm(plays ~  artist, family = poisson('log'), data = trainFull, maxit = 1000, chunksize = 100000)
summary(mod_glm)




### NEW IDEAS -- reduce dimensions of people


mod_glm = glm(plays ~ artist, family = poisson('log'), data = smDS)
summary(mod_glm)



#####
stripGlmLR = function(cm) {
     cm$y = c()
     cm$model = c()
     
     cm$residuals = c()
     cm$fitted.values = c()
     cm$effects = c()
     cm$qr$qr = c()  
     cm$linear.predictors = c()
     cm$weights = c()
     cm$prior.weights = c()
     cm$data = c()
     
     
     cm$family$variance = c()
     cm$family$dev.resids = c()
     cm$family$aic = c()
     cm$family$validmu = c()
     cm$family$simulate = c()
     attr(cm$terms,".Environment") = c()
     attr(cm$formula,".Environment") = c()
     
     cm
}

stripGlmLR(glm(plays ~ artist +  age, family = poisson('log'), data = smDS,
               y=FALSE, model=FALSE))

getModelSize = function(n) {
     data = synthFrame(n)
     model = stripGlmLR(glm(y~xN+xC,data=data,
                            family=binomial(link='logit'),
                            y=FALSE, model=FALSE))
     length(serialize(model, NULL))
}

getModelSize(mod_glm)


valSm <- read.csv("/Users/callinswitzer/Documents/GitRepos/CS181Regression/p3/ValSmall.csv")

# edit small validation set
valFull = merge(valSm, profiles, by = "user")
valFull$age[valFull$age < 0 | valFull$age > 100] = NA

valFull$ageNA = is.na(valFull$age)
valFull$age[valFull$ageNA] = mean(valFull$age, na.rm = TRUE)
summary(valFull)

nrow(valFull)


preds <- predict(mod_glm, newdata = valFull, type = 'response')
