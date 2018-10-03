install.packages("xkcd")

library(xkcd)


vignette("xkcd-intro")
ggplot() + 
  geom_point(aes(x=mpg, y=wt), data=mtcars) +
  theme(text = element_text(size = 16, family = "xkcd"))
library(extrafont)
