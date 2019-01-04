x = c(1:100, 1:100)
y = c(-x[1:50]*.30 + 5, x[51:100]*0.4-20, x[1:50]*0.1 +5, x[51:100]*0 - 5) + rnorm(200)


trtorder <- rep(x = c("rewardedFirst", "rewardedSecond"), each = 100)
rewardTF <- rep(c("T", "F", "F", "T"), each = 50)

length(y)

library(tidyverse)
theme_set(theme_bw())


df1 <- as.tbl(data.frame(x, y, trtorder, rewardTF)) %>%
  mutate(visitNum = x)
df1$visitNum[51:100]= df1$visitNum[51:100] - 50
df1$visitNum[151:200]= df1$visitNum[151:200] - 50
df1$visitNum



ggplot(df1, aes(x = x, y = y)) + 
  geom_point() + 
  facet_grid(~trtorder)


ggplot(df1, aes(x = visitNum, y = y)) + 
  geom_point(aes(color = rewardTF)) + 
  facet_grid(~trtorder)

df1$OrderReward = interaction(df1$trtorder, df1$rewardTF)
levels(df1$OrderReward)




# fit model
m10 <- lm(y ~ visitNum * trtorder * rewardTF, data = df1)
summary(m10)

m1 <- update(m10, .~. - visitNum:trtorder , data = df1)
summary(m1)

summary(m10)$coef
summary(m1)$coef


df1$preds = predict(m1)



ggplot(df1, aes(x = visitNum, y = preds)) + 
  geom_point(aes(color = rewardTF)) + 
  geom_point(aes(color = rewardTF, y = y)) + 
  facet_grid(~trtorder)

BIC(m10, m1)

library("mgcv")

g00 = gam(y ~
              trtorder*rewardTF + # treatment order
              s(visitNum, by = interaction(trtorder, rewardTF)), data = df1)

df1$gamPreds = predict(g00)
ggplot(df1, aes(x = visitNum, y = gamPreds)) + 
  geom_point(aes(color = rewardTF)) + 
  geom_point(aes(color = rewardTF, y = y)) + 
  facet_grid(~trtorder)

# start with gamm so I can show change by visit number
par(mfrow = c(3,3))
aab <- plot(g00)
summary(g00) # Summary for paper 
dev.off()

g0 <- gam(y ~
            trtorder+rewardTF + # treatment order
            s(visitNum, by = interaction(trtorder, rewardTF)), data = df1)


BIC(g00, g0)

df1$gamPreds2 = predict(g0)
ggplot(df1, aes(x = visitNum, y = gamPreds2)) + 
  geom_point(aes(color = rewardTF)) + 
  geom_point(aes(color = rewardTF, y = y)) + 
  facet_grid(~trtorder)
