#IDIGBIO IMAGE DOWNLOAD FUNCTION
#iNaturalist vs herbaria PLOSOne
#Last changed 12/3/23
#code by Elizabeth White and Rob Guralnick
#contact elizabethwhite1@ufl.edu

##################
#Downloads 500 randomly sampled herbarium images row by row from URLs in iDigBio download CSV
##################

rm(list=ls())

#set up a folder for images

#this is set up for mac, if PC - change "HOME" to "USERPROFILE"
#change to whatever you want to name it, I have it on the desktop but change to wherever you want it
desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
images_folder_path <- file.path(desktop_path, "herbarium_images")

#check if the folder already exists and if not, create it
if (!dir.exists(images_folder_path)) {
  dir.create(images_folder_path)
  cat("Folder 'herbarium_images' created.\n")
} else {
  cat("Folder 'herbarium_images' already exists. \n")
}

setwd("path/to/createdFolder/herbarium_images")

#sets the objects as a couple of the raw csvs already present in the repository
#but you could use any csv downloaded from your own iDigBio occurrences, just change the path
obs<-read.csv("https://raw.githubusercontent.com/ewhite344/iNatHerb/main/raw_downloads/raw_herb/cyperaceae_hobs.csv")
media<-read.csv("https://raw.githubusercontent.com/ewhite344/iNatHerb/main/raw_downloads/raw_herb/cyperaceae_media.csv")

#merge two csvs (need to do this to get ID and URL together)
herb.all<-merge(x=obs,y=media,by="coreid", na.rm=T)
herb<-herb.all[,c("gbif.canonicalName","coreid","ac.accessURI","idigbio.eventDate")]

#randomly sample 500 rows to download
herb500<-herb[sample(1:nrow(herb),500),]

#function to extract image URLs and download to folder created above
downlaod_images <- function(herb500,size="medium",outpath="."){
  for (i in 1:dim(herb500)[1]){
    iurl <- herb500$ac.accessURI[i]
    iurl <- gsub("medium",size,iurl)
    iname <- paste(outpath,herb500$coreid[i]," ",herb500$gbif.canonicalName[i]," ",herb500$idigbio.eventDate[i],".jpg",sep="")
    tryCatch(
      {download.file(iurl,iname, mode = 'wb')},
      error = function(err) {print(paste("MY_ERROR:  ",err))}
    )
    Sys.sleep(5)
  }
}

#execute function 
#(this takes a while, but you should see images filling in to the created folder)
downlaod_images(herb500,size="original","")
