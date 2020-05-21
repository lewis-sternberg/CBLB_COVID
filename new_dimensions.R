#COVID Index as a new dimension for CBLB
#Lewis Sternberg May 2019
#For all rankings 1 indicators most vulnerable

list.of.packages <- c("tidyverse","WDI","readxl","reshape2","ggplot2","data.table","jsonlite","rvest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
setwd("C:/git/CBLB_COVID/")
lookup <- fread("data/lookup.csv")

####INFORM####
#source: https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Epidemic
nms <- names(read_xlsx("data/INFORM.xlsx",sheet="INFORM Epi 2020 (a-z)",n_max=0))
type <- ifelse(nms=="EPIDEMIC INFORM RISK","numeric","guess")
INFORM <- setDT(read_xlsx("data/INFORM.xlsx",sheet="INFORM Epi 2020 (a-z)",col_types=type,))
INFORM <- INFORM[-1,c(1,2,44)]
INFORM$rank <- rank(-INFORM[,3],ties.method = "min")
names(INFORM)[2] <-"iso3c"

####WB####
#population aged 65 and above - SP.POP.65UP.TO.ZS (less is better)
#Percentage of population in urban settings - SP.URB.TOTL.IN.ZS (less is better)
#Population with basic handwashing facilities - SH.STA.HYGN.ZS (more is better))
#Hospital beds per 1000 people - SH.MED.BEDS.ZS (more is better)
#Doctors per 1000 people - SH.MED.PHYS.ZS (more is better)

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
sleeping$CountryName[which(sleeping$CountryName=="Congo Democratic Republic")]="Congo, Democratic Republic of"
sleeping <- merge(sleeping, lookup[,c(1,3)],by.x="CountryName",by.y="Country Name",all.x=T)
names(sleeping)[which(names(sleeping)=="ISO 3")] <- "iso3c"

####universal health coverage index####
UHC <- setDT(read.csv("http://apps.who.int/gho/athena/api/GHO/UHC_INDEX_REPORTED/?format=csv"))
names(UHC)[which(names(UHC)=="COUNTRY")] <- "iso3c"
names(UHC)[which(names(UHC)=="Numeric")] <- "value"
UHC <- UHC[UHC[iso3c!="",.I[YEAR==max(YEAR)],by=iso3c]$V1][,c(3,6,8)]
UHC$rank <- rank(UHC$value,ties.method="min")

####Global Food Security Index####
GFSI <- fread("data/GFSI.csv")
GFSI$rank <- rank(GFSI$Score,ties.method = "min")
GFSI$Country[which(GFSI$Country=="Congo (Dem. Rep.)")]="Congo, Democratic Republic of"
GFSI$Country[which(GFSI$Country=="Cote d’Ivoire")]="Cote d'Ivoire"
GFSI <- merge(GFSI, lookup[,c(1,3)],by.x="Country",by.y="Country Name",all.x=T)
names(GFSI)[which(names(GFSI)=="ISO 3")] <- "iso3c"

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
   html_text()%>%
    trimws("both")
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
diabetes[country=="Macau"]$country<-"China, Macao SAR"
diabetes[country=="US Virgin Islands"]$country<-"Virgin Islands, US"
diabetes <- merge(diabetes, lookup[,c(1,3)],by.x="country",by.y="Country Name",all.x=T)
names(diabetes)[which(names(diabetes)=="ISO 3")] <- "iso3c"

####IHME####
IHME <- fread("data/IHME.csv")
IHME[location=="Virgin Islands, U.S."]$location<-"Virgin Islands, US"
IHME[location=="Taiwan (Province of China)"]$location<-"Taiwan Province of China"
IHME <- merge(IHME, lookup[,c(1,3)],by.x="location",by.y="Country Name",all.x=T)
names(IHME)[which(names(IHME)=="ISO 3")] <- "iso3c"
#Chronic respiratory diseases
respiratory <- IHME[IHME[cause=="Chronic respiratory diseases"&metric=="Percent",.I[year==max(year)],by=location]$V1][,c(2,7,8,11)]
respiratory$rank <- rank(-respiratory$val,ties.method="min")
#Cardiovascular diseases
cardio <- IHME[IHME[cause=="Cardiovascular diseases"&metric=="Percent",.I[year==max(year)],by=location]$V1][,c(2,7,8,11)]
cardio$rank <- rank(-cardio$val,ties.method="min")

####WFP####
#Source: https://dataviz.vam.wfp.org/global-coverage-proteus-food-security-index-oct-2019
WFP <- fread("data/proteus_2017.csv")
WFP[Country=="Lao Pdr"]$Country <- "Lao"
WFP[Country=="Gambia, The"]$Country <- "The Gambia"
WFP[Country=="Macao Sar, China"]$Country <- "China, Macao SAR"
WFP[Country=="Hong Kong Sar, China"]$Country <- "Hong Kong"
WFP[Country=="Venezuela, Rb"]$Country <- "Venezuela"
WFP[Country=="Macedonia, Fyr"]$Country <- "Macedonia"
WFP[Country=="Bahamas, The"]$Country <- "The Bahamas"
WFP[Country=="St. Vincent and The Grenadines"]$Country <- "St. Vincent and the Grenadines"
WFP <- merge(WFP, lookup[,c(1,3)],by.x="Country",by.y="Country Name",all.x=T)
names(WFP)[which(names(WFP)=="ISO 3")] <- "iso3c"
WFP$rank <- rank(-WFP$`Proteus index`, ties.method="min")
WFP <- WFP[,c(1,6,8,9)]

####merge####
#only keeps rank and iso3c
dats <- c("WFP","cardio","diabetes","hypertension","GFSI","UHC","sleeping",WB_indicators,"INFORM")
for(dat in dats){
  temp <- get(dat)
  temp <- select(temp,iso3c,rank)
  names(temp)[which(names(temp)=="rank")] <- paste0(dat,"_rank")
  assign(dat,temp)
}
indicators_ranks <- lookup[!duplicated(lookup$`ISO 3`)][,c(1,3)]
names(indicators_ranks)[which(names(indicators_ranks)=="ISO 3")] <- "iso3c"
indicators_ranks <- Reduce(function(x,y) merge(x,y, by="iso3c",all.x=T),
                    list(indicators_ranks ,WFP,cardio,diabetes,hypertension,GFSI,UHC,sleeping,INFORM,SP.POP.65UP.TO.ZS,
                         SP.URB.TOTL.IN.ZS, SH.STA.HYGN.ZS,SH.MED.BEDS.ZS, SH.MED.PHYS.ZS))
fwrite(indicators_ranks, "data/output - new dimensions rankings.csv")
