#INATURALIST IMAGE DOWNLOAD FUNCTION
#iNaturalist vs herbaria PLOSOne
#Last changed 12/3/23
#code by Elizabeth White and Rob Guralnick
#contact elizabethwhite1@ufl.edu

##################
#Downloads 500 randomly sampled iNaturalist images row by row from URLs in iDigBio download CSV
#(only downloads FIRST image associated with observation)
##################

rm(list=ls())

#set up a folder for images

#this is set up for mac, if PC - change "HOME" to "USERPROFILE"
#change to whatever you want to name it, I have it on the desktop but change to whatever location
desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
images_folder_path <- file.path(desktop_path, "inat_images")

#check if the folder already exists and if not, create it
if (!dir.exists(images_folder_path)) {
  dir.create(images_folder_path)
  cat("Folder 'inat_images' created.\n")
} else {
  cat("Folder 'inat_images' already exists. \n")
}

#change working directory to created folder
setwd("path/to/createdFolder/inat_images")

cypdat<-read.csv('https://raw.githubusercontent.com/ewhite344/iNatHerb/main/raw_downloads/raw_inat/cyperaceae_obs.csv')


cypdat500<-cypdat[sample(1:nrow(cypdat),500),]

#function to extract image URLs and extract FIRST image associated with iNat observation
#(this only extracts one image per observation, which is not ideal)
downlaod_images <- function(cypdat500,size="medium",outpath="."){
  for (i in 1:dim(cypdat500)[1]){
    iurl <- cypdat500$image_url[i]
    iurl <- gsub("medium",size,iurl)
    iname <- paste(outpath,cypdat500$id[i]," ",cypdat500$scientific_name[i]," ",cypdat500$observed_on[i],".jpg",sep="")
    tryCatch(
      {download.file(iurl,iname, mode = 'wb')},
      error = function(err) {print(paste("MY_ERROR:  ",err))}
    )
    Sys.sleep(5)
  }
}

#execute function
#(this takes a while, but you should see images filling in to the created folder)
downlaod_images(cypdat500,size="original","")