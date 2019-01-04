

for(ii in 1:100){
  x = 1:100
  x2 = x + rnorm(1, 0, sd = 30)
  y = x2 + rnorm(100, mean = 0, sd = rnorm(1, 30, 10)) > 50
  
  if(ii == 1){
    tmp = data.frame(ii, x, y)
  }
  else{
    tmp = rbind(tmp, data.frame(ii, x, y))
  }
  
}

t2 <- t(as.data.frame.matrix(with(tmp, xtabs(~y + ii))))
t3 <- data.frame(t2)

t3$ii = row.names(t2)

plot(tmp$x, tmp$y)


library(lme4)

m1 <- glmer(cbind(TRUE., FALSE.) ~ (1 |ii), family= binomial, data = t3)
summary(m1)

points(tmp$x, predict(m1, type = 'response', re.form = NA), pch = 20)

lines(tmp$x, predict(m1, type = 'response'), pch = 20, col = 'grey40')


plot(predict(m1, type = 'response'), t3$TRUE. / 100)
plot(fitted(m1), resid(m1))
