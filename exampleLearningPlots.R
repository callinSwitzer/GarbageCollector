
library(tidyverse)



time_data = seq(-2, 9, length.out = 10)
learning = exp(time_data) / (1 + exp(time_data))*0.95



df = data.frame("Time" = 1:length(time_data), 
                "Success" = learning, 
                "tp" = "Animal")





# machine learning

time_data = seq(-2, 5, length.out = 50)
learning = exp(time_data) / (1 + exp(time_data))*0.99



df2 = data.frame("Time" = 1:length(time_data), 
                 "Success" = learning, 
                 "tp" = "Machine")






dfc = df %>%
  full_join(df2)
dfc  


ggplot(dfc, aes(x = Time, y = Success, color = tp)) + 

  geom_line(lwd = 1) +
  geom_point(size = 1) + 
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.title = element_blank(), 
        legend.position = c(0.79, 0.3), 
        legend.background = element_rect(fill = "grey90")) + 
  labs(x = "Number of experiences") + 
  scale_y_continuous(breaks = c(0,1), limits = c(-0.1, 1.1), expand = c(0, 0)) + 
  geom_hline(aes(yintercept = 0), lty = 2) + 
  geom_hline(aes(yintercept = 1), lty = 2) + 
  scale_color_manual(values = c(  
                                 rgb(144/255, 119/255, 96/255), rgb(120/255, 32/255, 8/255)))


ggsave("C:\\Users\\calli\\Desktop\\Rplot.png", dpi = 500, width = 5/2, height = 4/2)
