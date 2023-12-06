#STATISTIC ANALYSES
#iNaturalist vs Herbaria PLOSOne
#Elizabeth White- last changed 12/3/23
#contact: elizabethwhite1@ufl.edu

#####
rm(list=ls())

library(tidyverse)

total<- read.csv('/Volumes/ext.drive/inat images/data/score_csvs/total_rawdata.csv')

total <- read.csv('https://raw.githubusercontent.com/ewhite344/iNatHerb/main/total_rawscores.csv')

#separating data types
inat <- total%>%
  filter(data.type == "inat")
herb <- total%>%
  filter(data.type =="herb")

#FILTER JUST CORRECT
corr_onlyinat <-inat%>%
  filter(taxonomic_identification == "1")
corr_onlyherb <- herb%>%
  filter(taxonomic_identification == "1")

##TWO PROPORTION Z TEST FOR CORRECT BETWEEN INAT+HERBARIA
prop.test(x = c(4210, 3263), n = c(4996, 4274), p = NULL, alternative = "two.sided",
          correct = TRUE)

#FILTER JUST UNSURE
unsure_onlyinat <-inat%>%
  filter(taxonomic_identification == "U")
unsure_onlyherb <- herb%>%
  filter(taxonomic_identification == "U")

##TWO PROPORTION Z TEST FOR UNSURE BETWEEN INAT+HERBARIA
prop.test(x = c(692, 621), n = c(4996, 4274), p = NULL, alternative = "two.sided",
          correct = TRUE)

#FILTER JUST OUTDATED
out_onlyinat <-inat%>%
  filter(taxonomic_identification == "O")
out_onlyherb <- herb%>%
  filter(taxonomic_identification == "O")

##TWO PROPORTION Z TEST FOR OUTDATED BETWEEN INAT+HERBARIA
prop.test(x = c(26, 152), n = c(4496, 3876), p = NULL, alternative = "two.sided",
          correct = TRUE)

#FILTER JUST INCORRECT
in_onlyinat <-inat%>%
  filter(taxonomic_identification == "0")
in_onlyherb <- herb%>%
  filter(taxonomic_identification == "0")

##TWO PROPORTION Z TEST FOR OUTDATED BETWEEN INAT+HERBARIA
prop.test(x = c(60, 128), n = c(4496, 3876), p = NULL, alternative = "two.sided",
          correct = TRUE)




