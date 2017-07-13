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
