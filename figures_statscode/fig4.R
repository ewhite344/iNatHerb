#FIGURE 4 CODE
#PLOSOne inat vs herbaria
#Elizabeth White - last updated 10/30/23

rm(list = ls())
library(tidyverse)

#read in raw score data (from ImageAnt scoring process)
total <- read.csv("https://raw.githubusercontent.com/ewhite344/iNatHerb/main/total_rawdata.csv", sep = ',')

#add column with counts of how many IDs were scored 'correct'
total2 <- total%>%
  group_by(Family, data.type, taxonomic_identification)%>%
  dplyr::summarize(counts = n())%>%
  filter(taxonomic_identification == 1)

family_counts <- total%>%
  group_by(data.type, Family)%>%
  dplyr::summarize(count = n())

comb <- merge(total2, family_counts, by = c("Family", "data.type"))

#make new count column numeric
total2 <- comb%>%
  rowwise()%>%
  mutate(props = as.numeric(counts)/as.numeric(count))

#plot 'correct' scores by family
p <- ggplot(total2)+
  geom_point(aes(x = Family, y = props, fill = data.type), shape = 21, size = 7, color = "black")+
  ylim(0,1.0)

p + theme_bw() + theme(axis.text.x=element_text(angle=45,color="black",vjust=0.6,size=17),
                       axis.text.y=element_text(color="black",size=17),
                       axis.title.x=element_text(size=19,face="bold"),
                       axis.title.y=element_text(size=19,face="bold"),
                       axis.line = element_line(colour = "black"),
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),
                       panel.border = element_blank(),
                       legend.title = element_text(size=19),
                       legend.text = element_text(size=17),
                       panel.background=element_blank())+
  xlab("Family")+
  ylab("Proportion Correct Observations")+
  scale_fill_manual(values = c("white", "black"),name = "Data Type", labels = c("Herbarium", "iNaturalist"))
