dataset = data.frame(matrix(rexp(26 * 7, rate = 10), 26, 7))

dataset


hist(dataset[, 1])


subset = sample(1:16, size = 8)

dataset[subset,] = matrix(rexp(8 * 7, rate = 3), 8, 7)

dataset[dataset > 1] = 1

hist(unlist(dataset))

cluster = kmeans(dataset$X1, centers = 2)
cluster$cluster
plot(rowSums(dataset))


library(ggplot2)

ggplot(dataset, aes(x = row.names(dataset), y = rowSums(dataset))) + 
  geom_point(aes(color = as.factor(cluster$cluster))) 
  
