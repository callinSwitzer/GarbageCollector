# unit spheres


x1 = seq(-1, 1, length = 100)
x_1 = sqrt(1-x1^2)

plot(x1, x_1)

x_2 = (1-abs(x1))
plot(x1, x_2)

x_3 = sapply(1:length(x1), function(x) 1-max(abs(x1)))
plot(x1, x_3)



x1 = c(0.1, 0.5)
x2 = c(0.35, 0.75)
x3 = c(0.28, 1.35)
x4 = c(0, 1.01)

X = matrix(c(x1, x2, x3, x4), byrow = TRUE, ncol = 2)

dist(X, method = "euclidian")
plot(hclust(dist(X, method = "maximum")))

par(pty="s")
plot(X, asp = 1)
text(X, labels = c("x1", "x2", "x3", "x4"), adj = -1)


distt <- function(xx1, xx2){
     sqrt(sum((xx1 - xx2)^2))
}
distt(x1, x2)

sqrt(sum((x1 - x2)^2))



distt1 <- function(xx1, xx2){
     sum(abs(xx1 - xx2))
}
distt1(x1, x2)
distt1(x2, x3)
distt1(x3, x4)
distt1(x4, x1)
distt1(x2, x4)

disttinf <- function(xx1, xx2){
     max(abs(xx1 - xx2))
}
disttinf(x1, x2)
disttinf(x2, x3)
disttinf(x3, x4)
disttinf(x2, x4)




(x1[2] + x2[2]) / 2

points(0.225, y = 0.625)

distt(c(0.225, 0.625), x4)

distt1(c(0.225, 0.625), x3)

disttinf(c(0.225, 0.625), x3)
disttinf(c(0.225, 0.625), x4)

(0.85 + 0.6) / 2
# install.packages('proxy')

# l1 norm
proxy::dist(X, method = distt1)

# l2 norm
proxy::dist(X, method = distt)

# linf norm
proxy::dist(X, method = disttinf)


# dist matrix for l1 norm
plot(hclust(proxy::dist(X, method = distt1)))

# dist matrix for l2 norm
plot(hclust(proxy::dist(X, method = distt)))

# dist matrix for linf norm
plot(hclust(proxy::dist(X, method = disttinf)))



# make dendrograms
plot(hclust(proxy::dist(X, method = distt), method = 'average'), hang = -1)
hcAv = hclust(proxy::dist(X, method = distt), method = 'average')


pdf('clusters2.pdf', width = 8, height = 8)
par(mfrow = c(2,2))

# average
plot(as.dendrogram(hcAv), main = "average")

# centroid
hh = hclust(proxy::dist(X, method = distt), method = 'complete')
hh$height = c(0.35, 0.45, 0.6109902)
plot(as.dendrogram(hh), main = 'centroid')


#plot(as.dendrogram(hclust(proxy::dist(X, method = distt), method = 'ward.D2')), main = 'centroid')


# minimum
plot(as.dendrogram(hclust(proxy::dist(X, method = distt), method = 'single')), main = 'minimum')

# minimum
plot(as.dendrogram(hclust(proxy::dist(X, method = distt), method = 'complete')), main = 'maximum')
dev.off()


(distt(x1, x4) + distt(x1, x3) + distt(x2, x4) + distt(x2, x3)) / 4



distt(c(0.225, 0.625), x4)

distt(c(0.225, 0.625), x3)

(x1[2] + x2[2] + x4[2]) / 3

distt(c(0.15, 0.753), x3)
