
x <- seq(-10, 10, length.out = 100)
y <- seq(-5, 10, length.out = 100)

zz <- as.matrix(sapply(1:100, FUN = function(i) sort(rbeta(100, abs(i/5 - 10) , 3))), byrow = TRUE)
zz <- (zz/ sum(zz))
image(zz)




indxs <- matrix(1:10000, byrow = TRUE, ncol = 100)

ismps <- sample(indxs, replace = TRUE, prob = zz)
hist(ismps)


# find x and y values




linMap <- function(x, from, to) {
     # Shifting the vector so that min(x) == 0
     x <- x - min(x)
     # Scaling to the range of [0, 1]
     x <- x / max(x)
     # Scaling to the needed amplitude
     x <- x * (to - from)
     # Shifting to the needed level
     x + from
}


remapVals = lapply(1:10000, FUN = function(i) which(indxs == ismps[i], arr.ind=TRUE))

rrs = sapply(1:10000, FUN = function(i) remapVals[[i]][1])

hist(rrs)
cls = sapply(1:10000, FUN = function(i) remapVals[[i]][2])
hist(cls)


i = 1
xboot = sapply(1:10000, FUN = function(i) x[remapVals[[i]][1]])
yboot = sapply(1:10000, FUN = function(i) y[remapVals[[i]][2]])

plot(xboot, yboot, col = rgb(0,0,0,0.1), pch = 15)


i = 1
hist(sort(rbeta(100, 1 , 10)))

hist(zz)

z <- matrix(runif(10000), nrow = 10)


z <- z / sum(colSums(z))
image(z)


smp = sample(xx,replace = TRUE, prob = z)


inds = which(xx == 89, arr.ind=TRUE)
inds


x1[inds[1]]
y1[inds[2]]

