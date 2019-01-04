install.packages("riverplot")
library(riverplot)
x<- riverplot.example()
plot(x)
plot(x, srt=90, lty=1)


# you'll have your own state labels
states <- LETTERS[1:3]

# the states should be factors, with all 5 levels. In this
# example we're using character, but you'll want a factor,
# because that way if a given level is absent it'll still
# get tabulated later as a 0
Dat <- data.frame(id = 1:88, 
                  state1 = rep("Primary",size=88), 
                  state2 = rep("Secondary",size=65))

# create a node object
nodes <- data.frame(ID = paste0(rep(states,each=2),rep(1:2,3)), x = rep(1:2,3))

#tablulate edge weights (transition counts) somehow
trans1 <- table(paste(Dat$state1,Dat$state2))

# starting and ending nodes for each edge (flow)
N1 <- paste0(rep(states,each=3),1)
N2 <- paste0(rep(states,3),2)
# create edge object
edges <- data.frame(N1 = N1, N2 = N2, Value = as.vector(trans1))

# this is key, you want a riverplot object, this makes it for you given
# an edges and nodes data.frame.
#?makeRiver
Dat2 <- makeRiver(nodes, edges)
plot(Dat2)


install.packages("networkD3")


library(networkD3)
nodes = data.frame("name" = 
                     c("All Women",
                       "Primary Education", # Node 0
                       "Secondary Education", # Node 1
                       "Higher Education", 
                       "Leave education"))# Node 3
links = as.data.frame(matrix(c(
  0, 1, 88,
  0, 4, 12,
  1, 2, 65,
  1, 4, 23,# Each row represents a link. The first number # represents the node being conntected from.
  2, 3, 38, 
  2, 4, 65- 38), 
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sn = sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 20, nodeWidth = 20, nodePadding = 100, 
              margin = list(top = 100, right = 100, bottom = 100, left = 100))

png("C:/Users/calli/Desktop/Sankey.png")
sn
dev.off()
savePlot(filename = "C:/Users/calli/Desktop/Sankey.png", type = "png")





nodes = data.frame("name" = 
                     c("",
                       "Assistant Professor",
                       "Associate Professor", # Node 0
                       "Tenured Professor", 
                       "Other"))# Node 3
links = as.data.frame(matrix(c(
  1, 2, 41.7, 
  2, 3, 28.2,
  1, 4, 48.4 - 41.7, 
  2, 4, 41.7 - 28.2
), 
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sn = sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "source", Target = "target",
                   Value = "value", NodeID = "name",
                   fontSize= 30, nodeWidth = 20, nodePadding = 100, 
                   margin = list(top = 100, right = 100, bottom = 100, left = 100))

sn




men  = c(100 - 48.4, 100 - 41.7, 100 - 28.1)
women = 100 - men
women



ndf = data.frame(Gender = rep(c("Male     ", "Female     "), each = 3), 
           Level = rep(c("Assistant", "Associate", "Tenured"), 2), 
           Percentage = c(men, women))
library(ggplot2)


ggplot(ndf, aes(x = Level, y = Percentage, fill = Gender)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  scale_fill_viridis_d(option = "B", begin= 0.2, end = 0.9) + 
  theme_classic() + 
  labs(x = "Professor Role") + 
  theme(legend.position = c(0.5, 1), 
        legend.direction = "horizontal", 
        axis.text = element_text(color = "black", size = 5), 
        axis.title = element_text(color = "black", size = 5), 
        legend.text = element_text(color = "black", size = 5), 
        legend.background = element_blank(),
        legend.title = element_blank(), 
        legend.key.size = unit(0.4,"line"), 
        legend.spacing.x = unit(.2, 'line'), 
        axis.line = element_blank(), 
        axis.ticks.x = element_blank()) + 
  scale_y_continuous(breaks = c(0, 50, 100)) + 
  geom_hline(aes(yintercept = 50), lty = 2, color = 'grey', size = .1)
  

ggsave("C:/Users/calli/Desktop/MenWomenProf.png", width =2, height = 3, units = "in", dpi = 300)
