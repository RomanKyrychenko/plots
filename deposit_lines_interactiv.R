library(readxl)
library(lubridate)
library(readr)
library(plyr)
library(ggplot2)
library(scales)
library(ggiraph)

deposit <- read_excel("~/plots/deposit.xlsx")
names(deposit) <- c("Банк","01-01-2014","01-04-2014","01-07-2014","01-10-2014","01-01-2015","01-04-2015",
                    "01-07-2015","01-10-2015","01-01-2016","01-04-2016","01-07-2016","01-10-2016")

#or 
# deposit <- read_csv("https://raw.githubusercontent.com/RomanKyrychenko/plots/master/deposit.csv")

deposit2 <- reshape2::melt(deposit, id.vars="Банк")
deposit2$value <- parse_number(deposit2$value)
deposit2$variable <- dmy(deposit2$variable)
status_bank <- capital2[c(1,4)] %>% group_by(Банк,Статус) %>% summarise(freq=n())
deposit2<- left_join(deposit2,status_bank[1:2],by="Банк")

gg_path_3 = ggplot(deposit2) +
  geom_path_interactive(aes(variable, value, group=Банк, data_id = Банк,
                                tooltip = paste(Банк,"\n",Статус), onclick = paste(Банк,"\n",Статус),color = Статус), size = .3) + 
  scale_y_continuous(limits=c(0, 200000000), labels = comma) +
  scale_color_manual(values = c("#2b8cbe","#ece7f2")) +
  ylab("") +
  xlab("") +
  labs(title = "Власний капітал банків",
       subtitle = "Динаміка за 2014-2016 рокі \n в тисячах гривень",
       caption = "Джерело: Національний банк України") +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dotted"), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6,color = "black"),
        axis.title.y = element_text(margin = margin(r = 20), color = "grey70"),
        axis.title.x = element_text(margin = margin(t = 20), color = "darkslategrey"),
        plot.title = element_text(size = 12, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 8, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 6, margin = margin(t = 10), color = "grey70", hjust = 0),
        strip.text = element_text(size = 4, angle = 0),
        strip.background=element_rect(colour = "white", fill="white"),
        text = element_text(family = "Arial"))
ggiraph(code = {print(gg_path_3)}, hover_css = "stroke:#3182bd;stroke-width:3px;", width = 1, height = 5,zoom_max = 4)