# FIGURE 1 - Barchart of RG/Total observations for all of iNat
#PLOSOne inat vs herbaria
#Elizabeth White - last updated 10/30/23 - contact: elizabethwhite1@ufl.edu

rm(list = ls())
library(tidyverse)

#read in csv with all represented families on iNat as of 2022
#initial download and data organization of this CSV by CJ Campbell
everywhere <- read.csv("https://raw.githubusercontent.com/ewhite344/iNatHerb/main/all_inatFamilies.csv", sep = ',')

#combine families into single rows
col<-everywhere%>%
  group_by(family)%>%
  transmute(Total=sum(n))
sums<-col[!duplicated(col),]

#filter only families with over 500 Research-Grade (RG) Observations
just.rg<-everywhere[everywhere$quality_grade=="research"& everywhere$n>500,]
combined<-left_join(just.rg,sums,by='family')

#add column with proportion RG/total observations
prop<-transform(combined, new=n/Total)

#organize families of interest in descending order
ordered<-prop[order(prop$new),]
ordered$num<-seq.int(nrow(ordered))
just <- ordered%>%
  filter(family == "Melanthiaceae" | family == "Apocynaceae"| family == "Ericaceae"| family == "Gentianaceae"| family == "Fabaceae"| family == "Asteraceae"| family == "Fagaceae"| family == "Cyperaceae"| family == "Juglandaceae"| family == "Ulmaceae")

#plot barchart of RG/Total Proportions for the ten families in this study
plot <- ggplot(data = just, mapping = aes(x = reorder(family, -new), y = new))+
  geom_bar(stat = "identity", fill = "black")+
  xlab("Family")+
  ylab("Proportion Research Grade/Total Observations on iNaturalist")

plot + theme_bw() +theme(axis.text.x=element_text(angle=45,color="black",vjust=0.6,size=15),
                         axis.text.y=element_text(color="black",size=18),
                         axis.title.x=element_text(size=15,face="bold"),
                         axis.title.y=element_text(size=15,face="bold"),
                         axis.line = element_line(colour = "black"),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         panel.border = element_blank(),
                         panel.background=element_blank())
