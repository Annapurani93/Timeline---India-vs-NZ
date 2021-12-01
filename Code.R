library(tidyverse)
library(tidytuesdayR)
library(ggtext)
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
tuesdata$matches->matches1
glimpse(matches)

matches1%>%
  filter((team1=="India" & team2=="New Zealand")|
           (team1=="New Zealand" & team2=="India"))%>%
  select(team1,team2,winner,margin,margin_type,series,venue,match_date)%>%
  select(-c(team1,team2))%>%
  data.frame()->data


ggplot(data,aes(x=as.character(match_date),y=0,col=winner,label=str_wrap(paste(series,":",winner,"won by",margin,margin_type)),10))+
  labs(col="Winner")+
  scale_color_manual(values=c("#0e9aa7","#fe8a71"),labels=c("India","New Zealand"))+
  geom_hline(yintercept=0, 
             color = "#e6e6ea", size=0.3)+
  geom_segment(aes(y=margin,yend=0,xend=as.character(match_date)), color='#e6e6ea', size=0.2)+
  geom_point(aes(y=0), size=3)+
  geom_text(aes(x=as.character(match_date),y=margin+1.2))+
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        axis.text.x = element_text(colour="#e6e6ea", face="bold"),
        legend.position = "none",
        plot.background=element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,2,1,2),"cm"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_markdown(size=16, face="bold",margin=margin(b=15)),
        plot.subtitle=element_markdown(size=14,margin=margin(b=30)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=30)))+
  labs( y="NUMBER OF WINS",
        title="<span style='color:#0e9aa7'>INDIA <span style='color:#e6e6ea'>VERSUS <span style='color:#fe8a71'>NEW ZEALAND</span>",
        subtitle="<span style='color:#fe8a71'>New Zealand<span style= 'color:#e6e6ea'> is currently ranked #1 in the ICC Test and ODI rankings for men. The country's matches against <span style='color:#0e9aa7'>India <span style='color:#e6e6ea'>have always been<br> interesting - be it the Test, the ODI or the T20 matches. In November 2021, <span style='color:#0e9aa7'>India <span style='color:#e6e6ea'>won 3-0 in the T20 matches it played against <span style='color:#fe8a71'>New Zealand<span style='color:#e6e6ea'>.<br> However, a month before that <span style='color:#fe8a71'>New Zealand<span style='color:#e6e6ea'> beat <span style='color:#0e9aa7'>India<span style='color:#e6e6ea'> in World T20. 
        Below is a timeline of the matches the countries played against each other <br>between 1996 and 2005, marking the winners and the margin by which they won<br><br>
        <span style='color:#0e9aa7'>India <span style='color:#e6e6ea'>and <span style='color:#fe8a71'>New Zealand<span style='color:#e6e6ea'> played 30 matches against each other between 1995 and 2005, and <span style='color:#fe8a71'>New Zealand<span style='color:#e6e6ea'> won 17 of them. Here's a look</span>",
        caption = "Data from ESPN Cricinfo via Tidy Tuesday| Analysis and design: @annapurani93")->plot


    

ggsave("timelineNZ1.png",plot,width=40,height=30)      
