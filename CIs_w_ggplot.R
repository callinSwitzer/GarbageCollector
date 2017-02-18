## Callin Switzer
## 18 Feb 2017
## Ideas for representing Seed set



## make some fake data
n = 200
set.seed(123)
fakedta = rpois(n, lambda = c(seq(1, to = 3, length.out = n/2), seq(2,3.5, length.out = n/2)))
fakedta[(n/2):length(fakedta)] <- fakedta[(n/2):length(fakedta)] +1
fakedta = fakedta / max(fakedta) + rnorm(n, sd = 0.2)
fakedta[fakedta< 0] = 0
fakedta <- data.frame(S_S = fakedta, group  = rep(c(1,2), each = n/2), S_I = rep(1:(n/2), 2) / 50)

# plot by color in base plot function
plot(x = fakedta$S_I, y = fakedta$S_S, col = fakedta$group, pch = 20)


## here's a quick way to draw a CI with ggplot
library(ggplot2)
theme_set(theme_classic())

# plot with curve + conf interval
ggplot(fakedta, aes(x = S_I, y = S_S, color = factor(group))) + 
     geom_point() + 
     scale_color_manual(name = "Groups", values = c('red', 'blue')) + 
     stat_smooth(method = "lm", formula = y ~ exp(x), size = 1, se = TRUE) 

# plot with pure linear model
ggplot(fakedta, aes(x = S_I, y = S_S, color = factor(group))) + 
     geom_point() + 
     scale_color_brewer(name = "group", palette = "Set1") + 
     stat_smooth(method = "lm", formula = y ~ x, size = 1, se = TRUE) 

# plot with curve with new colors
ggplot(fakedta, aes(x = S_I, y = S_S, color = factor(group))) + 
     geom_point() + 
     scale_color_brewer(name = "group", palette = "Set1") + 
     stat_smooth(method = "lm", formula = y ~ exp(x), size = 1, se = TRUE, alpha = 0.2) 



# greyscale + different shapes for each group
ggplot(fakedta, aes(x = S_I, y = S_S, color = factor(group), shape = factor(group))) + 
     geom_point() + 
     theme(legend.position = ) + 
     scale_color_manual(name = "group", values = c('grey40', 'grey60')) + 
     scale_shape_manual(name = "group", values = c(20, 17)) + 
     stat_smooth(method = "lm", formula = y ~ exp(x), size = 1, se = TRUE, alpha = 0.2)
