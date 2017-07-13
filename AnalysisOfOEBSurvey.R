df1 <- data.frame(answer = c("yes", "no", "yes", "no"), 
                  gender = c("f", "f", "m", "m"), 
                  count = c(18, 19, 20, 7))
df1


df2 <- data.frame(gender = c("f", "m"), 
                               yes = c(18, 20), 
                               no = c(19, 7))
df2


m1 <- glm(cbind(yes, no) ~ gender, family = binomial, data= df2)
summary(m1)

m2 <- update(m1, .~. - gender)

anova(m1, m2, test = "LRT")

# Are you aware of GSAS-related sources of funding to support your research?
df3 <- data.frame(gender = c("f", "m"), 
                  yes = c(10, 14), 
                  no = c(29, 13))


m1 <- glm(cbind(yes, no) ~ gender, family = binomial, data= df3)
summary(m1)

m2 <- update(m1, .~. - gender)

anova(m1, m2, test = "LRT")



#"Q59 - Would you be interested in
#options for covering your stipend aside from teaching, such as curatorial
#internships in the MCZ/HUH or outreach internships in the HMNH/AA?"

df4 <- data.frame(gender = c("f", "m"), 
                  veryInt = c(33, 16), 
                  someInt = c(4, 9), 
                  notInt = c(2,2))

df4


df5 <- data.frame(interest = c(rep("veryInt", 33 + 16), 
                               rep("someInt", 4 + 9), 
                               rep("notInt", 2 + 2)), 
                  gender = c(rep(c("f", "m"), df4$veryInt), 
                             rep(c("f", "m"), df4$someInt), 
                             rep(c("f", "m"), df4$notInt)
                             ))

library(MASS)
pmod <- polr(interest ~ gender, data = df5)
summary(pmod)

pm2 <- update(pmod, .~. - gender)

anova(pmod, pm2, test = "Chisq")

prop.test(c(9, 0), n = c(37, 27))


#Summary (19 responses): One major theme of concern related to scheduling - just getting all your DAC members in the same room for a sufficient among of time to have a constructive conversation. DAC members should be prepared to spend 1.5-2 hours in the meeting.

df6 <- data.frame(interest = c(rep("weekly", 10 + 9), 
                               rep("biWeek", 11 + 12), 
                               rep("month", 7 + 6), 
                               rep("lessMonth", 9 + 0)), 
                  gender = c(rep(c("f", "m"), c(10, 9)), 
                             rep(c("f", "m"), c(11, 12)), 
                             rep(c("f", "m"), c(7, 6)), 
                             rep(c("f", "m"), c(9, 0))
                  ))


df6$interest <- factor(df6$interest, levels = c("lessMonth", "month", "biWeek", "weekly"), ordered = TRUE)


pm2 <- polr(interest ~ gender, data = df6)
summary(pm2)
predict(pm2, type = "probs")


pm3 <- update(pm2, .~. - gender)

anova(pm2, pm3, test = "Chisq")

pm3

deviance(pm3) / df.residual(pm3)  # should be 1, this model looks overdispersed

predict(pm3, type = "probs")

require(nnet)

df6$interest <- as.character(df6$interest)

relevel(df6$interest, ref = "lessMonth")

mm1 <- multinom(interest ~ gender, data = df6)
summary(mm1)


mm2 <- update(mm1, .~. - gender)
anova(mm2, mm1)

z <- summary(mm1)$coefficients/summary(mm1)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


predict(mm1, type = "probs")

preds <- data.frame(gender = c("f", "m"), predict(mm1, type = "probs", newdata = data.frame(gender = c("f", "m"))))
actual <- as.data.frame.matrix(t(prop.table(xtabs(~df6$interest + df6$gender), margin = 2)))
actual$gender <- rownames(actual)
actual

library(plyr)
library(reshape2)


mdfAct <- melt(t(actual))

mdfAct <- mdfAct[mdfAct$Var1 != "gender", ]
mdfAct$value <- as.numeric(as.character(mdfAct$value))

mdfPred <- melt(t(preds))
mdfPred <- mdfPred[mdfPred$Var1 != "gender", ]
mdfPred$value <- as.numeric(as.character(mdfPred$value))

mdfPred$type = "predicted"
mdfAct$type = "actual"

mdf <- rbind(mdfPred, mdfAct)
mdf

mdf$Var2 <- mapvalues(mdf$Var2, from = c(1,2), to = c("f", "m"))

library(ggplot2)

ggplot(mdf, aes(x = Var1, y = value, color = type)) + 
     geom_point() + 
     facet_wrap(~Var2)

