#COVID Index as a new dimension for CBLB
#Lewis Sternberg May 2019

list.of.packages <- c("tidyverse","WDI","readxl","reshape2","ggplot2","data.table","jsonlite","rvest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
setwd("C:/git/CBLB_COVID/")

####INFORM####
#source: https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Epidemic
nms <- names(read_xlsx("data/INFORM.xlsx",sheet="INFORM Epi 2020 (a-z)",n_max=0))
type <- ifelse(nms=="EPIDEMIC INFORM RISK","numeric","guess")
INFORM <- setDT(read_xlsx("data/INFORM.xlsx",sheet="INFORM Epi 2020 (a-z)",col_types=type,))
INFORM <- INFORM[-1,c(1,2,44)]
INFORM$rank <- rank(-INFORM[,3],ties.method = "min")

####WB####
#population aged 65 and above - SP.POP.65UP.TO.ZS (less is better)
#Percentage of population in urban settings - SP.URB.TOTL.IN.ZS (less is better)
#Population with basic handwashing facilities - SH.STA.HYGN.ZS (more is better))
#Hospital beds per 1000 people - SH.MED.BEDS.ZS (more is better)
#Doctors per 1000 people - SH.MED.PHYS.ZS (more is better)
#1 indicates most vulnerable

WB_indicators <- c("SP.POP.65UP.TO.ZS","SP.URB.TOTL.IN.ZS","SH.STA.HYGN.ZS","SH.MED.BEDS.ZS","SH.MED.PHYS.ZS")
for(indicator in WB_indicators[1:2]){
  WB <- setDT(WDI(country="all",indicator=indicator,extra=T, start=1980))
  WB <- WB[WB[region!="Aggregates"&!is.na(get(indicator)),.I[year==max(year)],by=country]$V1][,2:5]
  WB$rank <- rank(-WB[,2],ties.method="min")
  assign(indicator,WB)
}
for(indicator in WB_indicators[3:5]){
  WB <- setDT(WDI(country="all",indicator=indicator,extra=T, start=1980))
  WB <- WB[WB[region!="Aggregates"&!is.na(get(indicator)),.I[year==max(year)],by=country]$V1][,2:5]
  WB$rank <- rank(WB[,2],ties.method="min")
  assign(indicator,WB)
}

####mean people per sleeping room####
sleeping <- fromJSON("https://api.dhsprogram.com/rest/dhs/data?breakdown=national&indicatorIds=HC_PPRM_H_MNP&lang=en&f=json")
sleeping <- data.table(sleeping$Data)
sleeping <- sleeping[sleeping[,.I[SurveyYear==max(SurveyYear)],by=CountryName]$V1]
sleeping$rank <- rank(-sleeping$Value,ties.method="min")
sleeping <- select(sleeping, CountryName,SurveyYear, Value, rank)

####universal health coverage index####
UHC <- setDT(read.csv("http://apps.who.int/gho/athena/api/GHO/UHC_INDEX_REPORTED/?format=csv"))
names(UHC)[which(names(UHC)=="COUNTRY")] <- "iso3c"
names(UHC)[which(names(UHC)=="Numeric")] <- "value"
UHC <- UHC[UHC[iso3c!="",.I[YEAR==max(YEAR)],by=iso3c]$V1][,c(3,6,8)]
UHC$rank <- rank(UHC$value,ties.method="min")

####Global Food Security Index####
GFSI <- fread("data/GFSI.csv")
GFSI$rank <- rank(GFSI$Score,ties.method = "min")

####Prevaslence of raised blood pressure####
hypertension <- setDT(read.csv("http://apps.who.int/gho/athena/api/GHO/BP_03/?format=csv"))
names(hypertension)[which(names(hypertension)=="COUNTRY")] <- "iso3c"
names(hypertension)[which(names(hypertension)=="Numeric")] <- "value"
hypertension <- hypertension[hypertension[iso3c!=""&SEX=="BTSX",.I[YEAR==max(YEAR)],by=iso3c]$V1][,c(3,6,10)]
hypertension$rank <- rank(-hypertension$value,ties.method="min")

####Diabetes####
urls <- paste0("https://www.diabetesatlas.org/data/en/country/",1:221,"/.html")
dataIndex <- 1
dataList <- list()
pb <- txtProgressBar(max=length(urls),style=3)
for(i in 1:length(urls)){
  setTxtProgressBar(pb,i)
  url <- urls[i]
  html <- read_html(url)
  dat <- html_nodes(html,xpath='//*[@id="idf-country-data"]')%>%
    html_table()
  dat <- dat[[1]]
  country <- html_nodes(html,xpath='//*[@id="idf-print-area"]/div[1]/div[2]/h1') %>%
   html_text()
  if(dat[4,3]=="-"){ #using 2010 data when 2019 data is missing
    dataList[[dataIndex]] <- as.data.table(cbind(country,as.numeric(dat[4,2])))
  }else{dataList[[dataIndex]] <- as.data.table(cbind(country,as.numeric(dat[4,3])))
    }
  dataIndex <- dataIndex+1
}
close(pb)
diabetes <- rbindlist(dataList)
names(diabetes)[2] <- "prevalence"
diabetes$prevalence <- as.numeric(diabetes$prevalence)
diabetes$rank <- rank(-diabetes$prevalence, ties.method = "min")

####IHME####
IHME <- fread("data/IHME.csv")
#Chronic respiratory diseases
respiratory <- IHME[IHME[cause=="Chronic respiratory diseases"&metric=="Percent",.I[year==max(year)],by=location]$V1][,c(2,7,8)]
respiratory$rank <- rank(-respiratory$val,ties.method="min")
#Cardiovascular diseases
cardio <- IHME[IHME[cause=="Cardiovascular diseases"&metric=="Percent",.I[year==max(year)],by=location]$V1][,c(2,7,8)]
cardio$rank <- rank(-cardio$val,ties.method="min")

####WFP####
#Source: https://dataviz.vam.wfp.org/global-coverage-proteus-food-security-index-oct-2019
WFP <- fread("data/proteus_2017.csv")
WFP$rank <- rank(-WFP$`Proteus index`, ties.method="min")
WFP <- WFP[,c(1,6,8)]
