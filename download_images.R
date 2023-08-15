####USED ON EACH FAMILY CSV INDIVIDUALLY

#INATURALIST IMAGE DOWNLOAD

rm(list=ls())
setwd("/Volumes/ext.drive/inat images/cyperaceae")
cypdat<-read.csv('cyperaceae obs.csv')
cypdat500<-cypdat[sample(1:nrow(cypdat),500),]
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
downlaod_images(cypdat500,size="original","")

#IDIGBIO IMAGE DOWNLOAD

rm(list=ls())
setwd("/Volumes/ext.drive/inat images/cyperaceae_herb")
obs<-read.csv("cyperaceae_hobs.csv")
media<-read.csv("cyperaceae_media.csv")
herb.all<-merge(x=obs,y=media,by="coreid", na.rm=T)
herb<-herb.all[,c("gbif.canonicalName","coreid","ac.accessURI","idigbio.eventDate")]

#extract images,download
herb500<-herb[sample(1:nrow(herb),500),]
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
downlaod_images(herb500,size="original","")

