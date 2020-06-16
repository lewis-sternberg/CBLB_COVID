####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","varhandle","zoo","survey", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
setwd("C:/git/CBLB_COVID/")
output.dir <- "C:/git/CBLB_COVID/SPA"

zips <- list.files(path="data/SPA/",pattern="*.zip",full.names=T)
for(zip in zips){
  unzip(zip,exdir="SPA_unzip")
}

DTA <- list.files(path="SPA_unzip/",pattern="*.DTA",full.names=T)
pb = txtProgressBar(max=length(DTA),style=3)
for(i in 1:length(DTA)){
  setTxtProgressBar(pb,i)
  file <- DTA[i]
  fileBase <- basename(file)
  fileName <- substr(fileBase,1,nchar(fileBase)-4)
  fileName <- tolower(fileName)
  output.filename = paste0(output.dir,"/",fileName,".RData")
  if(!file.exists(output.filename)){
    data <- read.dta(file,convert.factors=F)
    save(data,file=output.filename)
    rm(data)
  }
}
close(pb)

