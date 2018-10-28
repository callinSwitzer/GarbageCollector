#censored data
# https://stats.stackexchange.com/questions/49443/how-to-model-this-odd-shaped-distribution-almost-a-reverse-j


set.seed(17)
n.data <- 2960
coeff  <- c(-0.001, 0.005)
sigma  <- 0.005
x      <- rnorm(n.data, 0.5)
y      <- as.vector(coeff %*% rbind(rep(1, n.data), x) + rnorm(n.data, 0, sigma))
y.cen           <- y
y.cen[y < 0]    <- 0
y.cen[y > 0.01] <- 0.01
data = data.frame(list(x, y.cen))

hist(y.cen)
