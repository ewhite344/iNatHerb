#FIGURE 3 CODE
#PLOSOne inat vs herbaria
#Elizabeth White - last updated 10/30/23

rm(list = ls())
library(tidyverse)

##FIG3 
#read in score data (from ImageAnt scoring process) with scores counted

totalscores <- totalscores %>%
  filter(taxonomic_identification == "1" | taxonomic_identification == "0" | taxonomic_identification == "U" | taxonomic_identification == "O")%>%
  group_by(data.type, Family, taxonomic_identification) %>%
  summarize(count = n()) 

p <- ggplot(totalscores)+
  geom_jitter(aes(x = taxonomic_identification, y = count, fill = data.type), shape =21, size = 4, color = "black")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("")+
  ylab("Number of Observations")

q <- p + scale_x_discrete(labels = c("Incorrect", "Correct", "Outdated", "Unsure")) +
  theme(legend.position = c(0.82, 0.9), legend.box.background = element_rect(color="white", size=1),
        axis.text.x=element_text(color="black",vjust=0.6,size=19),
        axis.text.y=element_text(color="black",size=17),
        axis.title.x=element_text(size=19,face="bold"),
        axis.title.y=element_text(size=19,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title = element_text(size=19),
        legend.text = element_text(size=17),
        panel.border = element_blank(),
        panel.background=element_blank())+ 
  scale_fill_manual(values = c("white", "black"), name = "Data Type", labels = c("Herbarium Specimens", "iNaturalist RG Observations")) + theme(legend.key=element_rect(fill="white"))
q