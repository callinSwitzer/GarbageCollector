# make dataset


# 4 time points
times = c(1,2,3,4)
cat1 = c(1,2)
cat2 = c(1,2)

varDF = expand.grid(times, cat1, cat2)


response = rnorm(16)
summary(lm(response~ varDF$Var1 + varDF$Var2 + varDF$Var3))
