# Callin Switzer
# 17 Feb 2017
# LMER prediction intervals and CI's 

library(lme4)


set.seed(271828)
data(sleepstudy)

library(magrittr)
sleepstudy %>% head

fm1 <- lmer(Reaction ~ Days + (1|Subject), data=sleepstudy)
summary(fm1)




library(ggplot2)
theme_set(theme_bw())

pp = data.frame(preds = predict(fm1), Days = sleepstudy$Days, subj = sleepstudy$Subject)

# look at data
ggplot(sleepstudy, aes(x = Days, y = Reaction, color = Subject)) + 
     geom_point() 

ggplot(sleepstudy, aes(x = Days, y = Reaction, color = Subject)) + 
     # geom_point() + 
     # geom_line() + 
     geom_point(data = pp, aes(x = Days, y = preds, color = subj))

# base R plot
plot(sleepstudy$Reaction ~ sleepstudy$Days)
lines(predict(fm1, re.form = NA), x = sleepstudy$Days) # plot fitted line
curve(251.41 + x * 10.47, 0, 9, add = TRUE) # how that curve is predicted




# construct prediction interals
mySumm <- function(.) {
     predict(., newdata=sleepstudy, re.form=NULL)
}
####Collapse bootstrap into median, 95% Prediction Interval
sumBoot <- function(merBoot) {
     return(
          data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
                     lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
                     upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
          )
     )
}

##lme4::bootMer() two possible methods
# if use.u is FALSE, this generates new random effects and random errors
# if use.u is TRUE, then the levels of the random effects are essentially fixed
system.time(
     boot1 <- lme4::bootMer(fm1, mySumm, nsim=100, use.u=TRUE, type="parametric")
)

PI.boot1 <- sumBoot(boot1)
PI.boot1




## bootstrap confidence interval, subtracting variation due to individuals
pframe <- data.frame(Days = unique(sleepstudy$Days))
pframe$Reaction <- 0
pp <- predict(fm1, newdata = pframe, re.form=NA, type = 'response')

mm <- model.matrix(terms(fm1), pframe)
predFun<-function(.) (mm%*%fixef(.) )
bb<-bootMer(fm1,FUN=predFun,nsim=200) #do this 200 times
bb_se<-apply(bb$t,2,function(x) quantile(x, probs = c(0.025, 0.975)))
pframe$blo<-bb_se[1,]
pframe$bhi<-bb_se[2,]
pframe$predMean <- pp


# calculate an "average" prediction interval, based on all individuals
predMeans = data.frame(predMean = tapply(PI.boot1$fit, sleepstudy$Days, mean ), 
                       predUpr = tapply(PI.boot1$upr, sleepstudy$Days, mean ), 
                       predLwr = tapply(PI.boot1$lwr, sleepstudy$Days, mean ), 
                       Days = unique(sleepstudy$Days))


# plot raw data, colored by subject
ggplot(sleepstudy, aes(x = Days, y = Reaction, color = Subject)) + 
      geom_point() 


# plot confidence interval for an average individual (i.e. not taking random effects into account)
ggplot(sleepstudy, aes(x = Days, y = Reaction, color = Subject)) + 
     geom_point() + 
     geom_line(data = pframe, aes(x = Days, y = predMean), color = 'red' ) + 
     geom_line(data = pframe, aes(x = Days, y = bhi), color = 'red', lty = 2 ) + 
     geom_line(data = pframe, aes(x = Days, y = blo), color = 'red', lty = 2) 

# plot showing predictions and CI's for each individual
ggplot(sleepstudy, aes(x = Days, y = Reaction, color = Subject)) + 
     geom_point() + 
     geom_line(data = PI.boot1, aes(x = sleepstudy$Days, y = fit, color = sleepstudy$Subject)) + 
     geom_line(data = PI.boot1, aes(x = sleepstudy$Days, y = lwr, 
                                    color = sleepstudy$Subject), lty = 2) + 
     geom_line(data = PI.boot1, aes(x = sleepstudy$Days, y = upr, 
                                    color = sleepstudy$Subject), lty = 2)

# plot showing average predictions and intervals (accounting for random effects)
ggplot(sleepstudy, aes(x = Days, y = Reaction, color = Subject)) + 
     geom_point() + 
     geom_line(data = predMeans, aes(x = Days, y = predMean), color = 'red') + 
     geom_line(data = predMeans, aes(x = Days, y = predLwr), color = 'red', lty = 2) + 
     geom_line(data = predMeans, aes(x = Days, y = predUpr), color = 'red', lty = 2)



### another method for fixed effects only
library(merTools)
pframe$Subject = 999 # not a subject from our dataset
# can change indlue.resid.var
preds <- predictInterval(fm1, newdata = pframe, n.sims = 10000, stat = "mean", include.resid.var = TRUE)
preds$Days = unique(sleepstudy$Days)   


# plot showing average predictions and intervals (including residual variance)
ggplot(sleepstudy, aes(x = Days, y = Reaction, color = Subject)) + 
     geom_point() + 
     geom_line(data = preds, aes(x = Days, y = fit), color = 'red') + 
     geom_line(data = preds, aes(x = Days, y = upr), color = 'red', lty = 2) + 
     geom_line(data = preds, aes(x = Days, y = lwr), color = 'red', lty = 2)

fm1
