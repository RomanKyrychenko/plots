# Барчарт-накладка

library(ggplot2)
library(dplyr)

poriv <- data_frame(variable=c("чоловіки", "жінки", "18-34", "35-49", "50-64", "65+"),
                    Ukraine=c(26.38,31.91,30.35,33.33,29.11,21.15),
                    US=c(14.83,14.34,30,18,10,7.7))

ggplot() + 
  geom_bar(data=poriv[1:2,], aes(x= variable, y= Ukraine),stat = "identity", fill = '#278DBC') +
  geom_bar(data=poriv[1:2,], aes(x= variable, y= US), stat="identity", fill = "navyblue", width = .6) +
  scale_y_continuous(limits=c(0,40)) +
  xlab("") + 
  ylab("") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size=.05, color="gray" ),
    legend.position = "top",
    panel.background = element_rect(fill='white', colour='white'),
    axis.ticks = element_blank()
  )