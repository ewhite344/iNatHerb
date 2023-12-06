#FIG 5 - BIAS HISTOGRAM
#PLOSOne inat vs herbaria
#Elizabeth White - last updated 10/30/23 - contact: elizabethwhite1@ufl.edu

rm(list = ls())
library(tidyverse)

#part 1: herbaria

setwd("path/to/downloaded/repository/raw_occurrencedownloads/raw_herb")

folder_path <- ("same/path/as/above")

# Get a list of all CSV files in the folder (not including media files)
csv_files <- list.files(folder_path, pattern = "_hobs\\.csv$", full.names = TRUE)
data_frames <- map(csv_files, ~ read_csv(.x) %>% mutate(filename = basename(.x)))

# combine dataframes from folder
combined_df_base <- do.call(rbind, data_frames)
just_sp <- combined_df_base%>%
  select('gbif:canonicalName', 'filename')
just_sp$filename <- sub("_.*", "", just_sp$filename)
colnames(just_sp) <- c('species', 'family')

#add column for genus
just_sp$genus <- sapply(strsplit(just_sp$species, " "), `[`, 1)

#get counts for each family, genus, and species in herbaria dataset
fam_count_herb <- just_sp%>%
  group_by(family)%>%
  dplyr::summarize(counts.herb = n())
gen_count_herb <- just_sp%>%
  group_by(genus)%>%
  dplyr::summarize(counts.herb = n())
spec_count_herb <- just_sp%>%
  group_by(species)%>%
  dplyr::summarize(counts.herb = n())

#part 2: redo everything above for inat

setwd("path/to/downloaded/repository/raw_occurrencedownloads/raw_inat")
folder_path2 <- ("same/path/as/above")

# Get a list of all CSV files in the folder
csv_files2 <- list.files(folder_path2, full.names = TRUE)
data_frames2 <- map(csv_files2, ~ read_csv(.x) %>% mutate(filename = basename(.x)))

#extract only columns of interest - scientific name, format dataframe
selected_columns_list <- lapply(data_frames2, function(df) df[, c("scientific_name", "filename")])
combined_df_base2 <- do.call(rbind, selected_columns_list)
combined_df_base2$filename <- sub("_.*", "", combined_df_base2$filename)
combined_df_base2$filename <- sub(" .*", "", combined_df_base2$filename)
colnames(combined_df_base2) <- c('species', 'family')
combined_df_base2$genus <- sapply(strsplit(combined_df_base2$species, " "), `[`, 1)

#get counts for each family, genus, and species in inat dataset
fam_count_inat <- combined_df_base2%>%
  group_by(family)%>%
  dplyr::summarize(counts.inat = n())
gen_count_inat <- combined_df_base2%>%
  group_by(genus)%>%
  dplyr::summarize(counts.inat = n())
spec_count_inat <- combined_df_base2%>%
  group_by(species)%>%
  dplyr::summarize(counts.inat = n())

#remove erroneous/NA bars
gen_count_inat <- gen_count_inat[136:669,]

#join inat and herbaria dataframes
joined_spec <- full_join(spec_count_herb, spec_count_inat)
joined_fam <- full_join(fam_count_herb, fam_count_inat)

#make everything lowercase
spec_count_inat$species <- tolower(spec_count_inat$species)
gen_count_inat$genus <- tolower(gen_count_inat$genus)

#######
#making the barchart
#######

#filter NAs (herbaria)
herb_nona <- spec_count_herb%>%
  filter(!is.na(species))

#format dataframe
herb_nona <- herb_nona[sapply(strsplit(as.character(herb_nona$species)," "),length)>1,]

#(STATS) calculate variance for herbaria dataframe
mean_herb <- mean(herb_nona$counts.herb)
herb_dev <- sd(herb_nona$counts.herb)
herb_dev/mean_herb * 100

#create barchart of species present in herbarium dataset
barplot(herb_nona$counts.herb, ylim = c(0,10000), xlab = "Species Present in Herbarium Dataset", ylab = "Number of Observations")

#filter NAs (inaturalist)
inat_nona <- spec_count_inat%>%
  filter(!is.na(species))
inat_nona <- inat_nona%>%
  filter(species != "false")

#format dataframe
inat_nona <- subset(inat_nona, !grepl("[0-9]", species))

#(STATS) calculate variance for inaturalist dataframe
mean_inat <- mean(inat_nona$counts.inat)
inat_dev <- sd(inat_nona$counts.inat)
inat_dev/mean_inat * 100

#create barchart of species present in inaturalist dataset
barplot(inat_nona$counts.inat, ylim = c(0,10000), xlab = "Species Present in iNaturalist Dataset", ylab = "Number of Observations")

#combine two barcharts into one figure
par(mfrow = c(2, 1))
par(mar=c(5,6,4,1)+.1)
barplot(inat_nona$counts.inat, ylim = c(0,10000), cex.lab = 2.5, cex.axis = 1.2, xlab = "Species Present in iNaturalist Dataset", ylab = "Number of Observations")
barplot(herb_nona$counts.herb, ylim = c(0,10000),cex.lab = 2.5, cex.axis = 1.2, xlab = "Species Present in Herbarium Dataset", ylab = "Number of Observations")
