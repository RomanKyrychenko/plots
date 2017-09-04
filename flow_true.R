#devtools::install_github("hrbrmstr/streamgraph")
#Sys.setlocale("LC_ALL","Ukrainian")
#library(streamgraph)
#
#temy <- readxl::read_excel("~/Теми для 27-го.xlsx",sheet = 2)
#temy$Година <- as.numeric(temy$Година)
#streamgraph(temy, Тема, Охоплення, Година, width = NULL, height = NULL,
#            offset = "silhouette", interpolate = "cardinal", interactive = F,
#            scale = "numeric", top = 20, right = 40, bottom = 30, left = 70)

library(ggplot2)
library(scales)

library(ggrepel)

#medt <- temy[-10] %>% group_by(Година) %>% summarise(n=median(`Охоплення`))
#temy <- left_join(temy,medt,by="Година")
#ifelse(Тональність=="Нейтрал",Охоплення,-Охоплення)


temy <- readxl::read_excel("Теми_Саакашвілі.xlsx",sheet = 1)[1:4]

temy$Дата <- c(lubridate::ymd_hms(paste(temy$Дата[1:70],"20:59:59")), lubridate::ymd_hms(paste(temy$Дата[71:80],"11:27:00"))+3600*24)

library(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 9))

ggplot(temy)+
  stat_smooth(aes(fill=Тема, y=ifelse(Тональність=="Нейтрал", Контакти, -Контакти),x=Дата), 
              geom = 'area', method = 'loess', span = 1/3, alpha = 2/3, position="stack") + 
  geom_hline(yintercept = 0, size=1) +
  geom_label(aes(max(Дата)-3600*12,max(ifelse(Тональність=="Нейтрал", Контакти, -Контакти)),
                 label=paste0("Нейтрал\n",format(sum(Контакти[Тональність=="Нейтрал"]),big.mark=" ")),family="PT Sans"),size=6,color="#6baed6")+
  geom_label(aes(max(Дата)-3600*12,min(ifelse(Тональність=="Нейтрал", Контакти, -Контакти)),
                 label=paste0("Негатив\n",format(sum(Контакти[Тональність=="Негатив"]),big.mark=" ")),family="PT Sans"),size=6,color="#fc4e2a")+
  geom_point(aes(lubridate::ymd_hms("2017-07-26 11:27:00"),0),size=5) +
  geom_text_repel(data=data.frame(x=lubridate::ymd_hms("2017-07-26 11:27:00"),y=50000),aes(x,y,label="Пост Мосійчука у Facebook")) +
  scale_y_continuous(labels = format_format(scientific=F,big.mark = " "),expand = c(0,0),breaks = c(1000000,750000,500000,250000,0,-250000))+
  xlab("") +
  ylab("Контакти") +
  scale_fill_manual(values=mycolors)+
  scale_x_datetime(expand = c(0.01,1),breaks = date_breaks("1 day"),labels = date_format("%d/%m")) + 
  theme_minimal() + theme(
    text=element_text(family="PT Sans"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed",size = .05, color="grey"),
    axis.title.y = element_text(family="PT Sans",size = 16),
    axis.text = element_text(family="PT Sans",size = 14)
  )

#ggplot(temy)+
#  stat_smooth(aes(y= Контакти,x=Дата), geom = 'area', method = 'loess', span = 1/3, alpha = 2/3, position="stack") + 
#  scale_y_continuous(labels = format_format(scientific=F))+
#  xlab("") +
#  ylab("") +
#  scale_fill_manual(values=mycolors)+
#  theme_minimal() + theme(
#    legend.position = "bottom",
#    axis.title.y = element_text(family="PT Sans"),
#    axis.text = element_text(family="PT Sans")
#  )

persons <- readxl::read_excel("ТБ+Інет(персон%2c джерела).xlsx",sheet = 1)[1:4]

ggplot(persons,aes(reorder(Персона,Охоплення),Охоплення)) +
  geom_bar(stat="identity", fill="lightblue") +
  coord_flip() + facet_wrap(~Тональність+Статус, scales = "free_y") +
  scale_y_continuous(labels = format_format(scientific=F,big.mark = " "),expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  xlab("") +
  ylab("Контакти") +
  theme_minimal() + theme(
    strip.text = element_text(family="PT Sans",size = 14,face = "bold"),
    text=element_text(family="PT Sans"),
    legend.position = "bottom",
    panel.margin = unit(0, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed",size = .05, color="grey"),
    axis.title.y = element_text(family="PT Sans",size = 16),
    axis.text = element_text(family="PT Sans",size = 14)
  )

net <- readxl::read_excel("ТБ+Інет(персон%2c джерела).xlsx",sheet = 2)[c(1,2,4)]
library(tidyr)
net <- spread(net,Тональність,Охоплення)
net$Нейтрал <- ifelse(is.na(net$Нейтрал),0,net$Нейтрал)

#Код для створення двостороннього барчарту

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

g.mid<-ggplot(net,aes(x=1,y=reorder(Канал, Нейтрал)))+geom_text(aes(label=Канал))+
  ggtitle("")+
  ylab(NULL)+
  theme(axis.title=element_blank(),
        text=element_text(family="PT Sans"),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))

g1 <- ggplot(data = net, aes(x = reorder(Канал, Нейтрал), y = Негатив)) +
  geom_bar(stat = "identity",fill="#fdd49e") + ggtitle("Негатив") +
  ylab("") +
  xlab("") +
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        text=element_text(family="PT Sans"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed",size = .05, color="grey"),
        axis.text = element_text(family="PT Sans",size = 14),
        axis.ticks = element_blank(), 
        plot.margin = unit(c(1,-1,1,0), "mm"),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_reverse(labels = format_format(scientific=F,big.mark = " "),expand = c(0.1,1),
                  limits=c(max(c(net$Негатив,net$Нейтрал),na.rm=T),0)) + coord_flip()

g2 <- ggplot(data = net, aes(x = reorder(Канал, Нейтрал), y = Нейтрал)) +xlab(NULL)+
  geom_bar(stat = "identity",fill="#d9f0a3") + ggtitle("Нейтрал") +
  scale_y_continuous(labels = format_format(scientific=F,big.mark = " "),expand = c(0.1,1),
                     limits=c(0,max(c(net$Негатив,net$Нейтрал),na.rm=T)))+
  ylab("") +
  xlab("") +
  theme(axis.title = element_blank(), 
              axis.text.y = element_blank(), 
              text=element_text(family="PT Sans"),
              legend.position = "bottom",
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(linetype = "dashed",size = .05, color="grey"),
              axis.text = element_text(family="PT Sans",size = 14),
              axis.ticks = element_blank(), 
              axis.line = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(3.5/9,2/9,3.5/9))


#TV

tv <- readxl::read_excel("ТБ+Інет(персон%2c джерела).xlsx",sheet = 3)[c(1,3,4)]
library(tidyr)
tv <- spread(tv,Тональність,Охоплення)
tv$Нейтрал <- ifelse(is.na(tv$Нейтрал),0,tv$Нейтрал)

#Код для створення двостороннього барчарту

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

g.mid<-ggplot(tv,aes(x=1,y=reorder(Канал, Нейтрал)))+geom_text(aes(label=Канал))+
  ggtitle("")+
  ylab(NULL)+
  theme(axis.title=element_blank(),
        text=element_text(family="PT Sans"),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))

g1 <- ggplot(data = tv, aes(x = reorder(Канал, Нейтрал), y = Негатив)) +
  geom_bar(stat = "identity",fill="#fdd49e") + ggtitle("Негатив") +
  ylab("") +
  xlab("") +
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        text=element_text(family="PT Sans"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed",size = .05, color="grey"),
        axis.text = element_text(family="PT Sans",size = 14),
        axis.ticks = element_blank(), 
        plot.margin = unit(c(1,-1,1,0), "mm"),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_reverse(labels = format_format(scientific=F,big.mark = " "),expand = c(0.1,1),
                  limits=c(max(c(tv$Негатив,tv$Нейтрал),na.rm=T),0)) + coord_flip()

g2 <- ggplot(data = tv, aes(x = reorder(Канал, Нейтрал), y = Нейтрал)) +xlab(NULL)+
  geom_bar(stat = "identity",fill="#d9f0a3") + ggtitle("Нейтрал") +
  scale_y_continuous(labels = format_format(scientific=F,big.mark = " "),expand = c(0.1,1),
                     limits=c(0,max(c(tv$Негатив,tv$Нейтрал),na.rm=T)))+
  ylab("") +
  xlab("") +
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        text=element_text(family="PT Sans"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed",size = .05, color="grey"),
        axis.text = element_text(family="PT Sans",size = 14),
        axis.ticks = element_blank(), 
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))

