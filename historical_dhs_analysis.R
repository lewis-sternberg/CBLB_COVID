####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","varhandle","zoo","survey","dplyr","WDI","boot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

memory.limit(32000)


wd <- "C:/git/MICS_recode/"
wd2 <- "C:/git/MICS_recode/project_data/DHSauto/"
wd3 <- "C:/git/CBLB_COVID/data"
setwd(wd)
povcalcuts <- fread("project_data/p20-p80 data.csv")

povcalyears <- c(2015:2018)
povcalcuts <- povcalcuts[CoverageType %in% c("N", "A"), .(RequestYear=povcalyears,
                             P20Headcount=approx(RequestYear, P20Headcount, povcalyears)$y,
                             ExtPovHC=approx(RequestYear, ExtPovHC, povcalyears)$y,
                             P80Headcount=approx(RequestYear, P80Headcount, povcalyears)$y),
                         by=.(CountryCode, CountryName, CoverageType)]

missingpovcal <- dplyr::setdiff(setnames(subset(WDI(start=2011, end=2011, extra=T), region != "Aggregates")[,c("iso3c","country")],c("CountryCode","CountryName")), povcalcuts[,c("CountryCode","CountryName")])
missingpovcal <- cbind(missingpovcal[rep(seq_len(nrow(missingpovcal)), length(povcalyears)),], RequestYear=rep(povcalyears, each=nrow(missingpovcal)))
povcalcuts <- rbind(povcalcuts, missingpovcal, fill=T)

dhsmeta <- fread("project_data/dhs_meta_data20190524.csv")
dhsmeta <- subset(dhsmeta, Recode.Structure.!="DHS-I")

dhsmeta$Country.[which(dhsmeta$Country.=="Cape Verde")] <- "Cabo Verde"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo")] <- "Congo, Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Congo Democratic Republic")] <- "Congo, Democratic Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Egypt")] <- "Egypt, Arab Republic of"
dhsmeta$Country.[which(dhsmeta$Country.=="Gambia")] <- "Gambia, The"
dhsmeta$Country.[which(dhsmeta$Country.=="Yemen")] <- "Yemen, Republic of"

names(dhsmeta)[which(names(dhsmeta)=="Country.")] <- "CountryName"

dhsmeta$filename=paste0(dhsmeta$dhs_cc,"HR",dhsmeta$dhs_recode_code,"DT")
dhsmeta$brfile <- tolower(paste0(dhsmeta$dhs_cc,"BR",dhsmeta$dhs_recode_code,"FL"))
dhsmeta=dhsmeta[which(!is.na(dhsmeta$dhs_cc)),]

dhsmeta2 <- unique(dhsmeta[,c("CountryName","surveyyr","filename","brfile")])
brfiles <- dhsmeta2$brfile
dhsmeta2$brfile <- NULL

variables <- c("sleeping")
grid <- as.data.table(expand.grid(filename=unique(dhsmeta2$filename), RequestYear=povcalyears, variable=variables))

dhsmeta2 <- merge(grid, dhsmeta2, all=T)

povcalcuts <- join(dhsmeta2,povcalcuts,by=c("CountryName","RequestYear"))

names(povcalcuts)[which(names(povcalcuts)=="CountryCode")] <- "iso3"
povcalcuts$hc<- povcalcuts$P20Headcount
povcalcuts$extreme <- povcalcuts$ExtPovHC
povcalcuts$u20 <- povcalcuts$P80Headcount
keep <- c("iso3","RequestYear","surveyyr","hc","filename","extreme","variable","u20")
povcalcuts <- povcalcuts[,keep, with=F]
povcalcuts <- povcalcuts[order(povcalcuts$filename,povcalcuts$RequestYear),]

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  if(is.na(prob)){
    return(c(NA,NA,NA)) #NA,NA,NA,NA,NA,NA BATMAN
  } else {
    df <- data.frame(x,w)
    if(na.rm){
      df <- df[which(complete.cases(df)),]
    }
    #Sort
    df <- df[order(df$x),]
    sumw <- sum(df$w)
    df$cumsumw <- cumsum(df$w)
    #For each percentile
    cutList <- c()
    cutNames <-c()
    for(i in 1:length(prob)){
      p <- prob[i]
      pStr <- paste0(round(p*100,digits=2),"%")
      sumwp <- sumw*p
      df$above.prob <- df$cumsumw>=sumwp
      thisCut <- df$x[which(df$above.prob==TRUE)[1]]
      cutList <- c(cutList,thisCut)
      cutNames <- c(cutNames,pStr)
    }
    names(cutList) <- cutNames
    return(cutList)
  }
}

sum.var <- function(x){
  if(length(x) > 0){
    bs <- boot(x, statistic=function(y, i) sum(y[i]), R=max(100, length(x)+1))
    out <- as.numeric(var(bs$t))
  } else {
    out <- 0
  }
  return(out)
}

rm(grid,dhsmeta,dhsmeta2)
gc()

####Run function####
setwd(wd2)

#######################################################################################################
groups <- c("age")

datayears <- c(2015)
#######################################################################################################

rdatas <- list.files(pattern="*.RData",ignore.case=T)
rdatas <- substr(rdatas,1,nchar(rdatas)-6)
existingbrfiles <- rdatas[substr(rdatas, 3,4)=="br"]
missing.br <- paste0(setdiff(brfiles, existingbrfiles),".RData")
dataList <- list()
dataIndex <- 1
last_filename <- ""
povcalcuts.years <- povcalcuts[RequestYear %in% datayears]
longlist <- list()
longindex <- 1

#pb <- txtProgressBar(max=nrow(povcalcuts.years),style=3)
# Loop through every povcalcut
for(i in 1:nrow(povcalcuts.years)){
  povcal_subset <- povcalcuts.years[i,]
  if(povcal_subset$iso3!="IND"){
  #setTxtProgressBar(pb, i-1)
  # Pull some coded info out of the dir name
  country <- tolower(substr(povcal_subset$filename,1,2))
  recode <- tolower(substr(povcal_subset$filename,3,4))
  phase <- tolower(substr(povcal_subset$filename,5,6))
  subphase <- substr(povcal_subset$filename,5,5)
  rdata_name <- paste0(country,recode,phase,"fl")
  variable <- tolower(povcal_subset$variable)
  if(substr(rdata_name,0,6) != last_filename){
    if(!(rdata_name %in% rdatas)){ next; }
    if(exists("pr")){rm(pr)}
    pr_patha <- paste0(country,"pr",phase)
    pr_path <- paste0(tolower(pr_patha),"fl.RData")
    load(pr_path)
    pr <- as.data.table(data)
    remove(data)
    keep <- c("hvidx","hhid","hv001","hv002","hv005","hv024","hv025","hv219","hv220","hv271","hv104",
              "hv105","hv109","hv112","hv140","hc70","v106","hv113","hv024","hv216","hv009")
    pr <- subset(pr, select= (colnames(pr) %in% keep))
    pr <- subset(pr, select= (colnames(pr) %in% keep))
    gc()
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    if(exists("br")){rm(br)}
    br_patha <- paste0(country,"br",phase)
    br_path <- paste0(tolower(br_patha),"fl.RData")
    if(!(br_path %in% missing.br)){
      load(br_path)
      br <- as.data.table(data)
      remove(data)
      keep <- c("v001","v002","b3","v008","v005","b7","hw5","b4")
      br <- subset(br, select= (colnames(br) %in% keep))
      gc()
      names(br)[which(names(br)=="v001")] <- "cluster"
      names(br)[which(names(br)=="v002")] <- "household"
      names(br)[which(names(br)=="v005")] <- "sample.weights"
      br$weights <- br$sample.weights/1000000
    }else{
      br <- data.frame(p20=NA, sex=NA)
    }
    
    #Rename sample.weights var
    names(pr)[which(names(pr)=="hv005")] <- "sample.weights"
    pr$weights <- pr$sample.weights/1000000
    
    #Urban/rural
    if(phase>1){
      names(pr)[which(names(pr)=="hv025")] <- "urban.rural"
    }else{
      names(pr)[which(names(pr)=="v102")] <- "urban.rural"
    }
    pr$urban <- NA
    pr$urban[which(pr$urban.rural==1)] <- 1
    pr$urban[which(pr$urban.rural==2)] <- 0
    
    # Wealth
    if("hv271" %in% names(pr)){
      pr$hv271 <- pr$hv271/100000
      names(pr)[which(names(pr)=="hv271")] <- "wealth"
    }else{
      wi_patha <- paste0(country,"wi",phase)
      wi_path <- paste0(tolower(wi_patha),"fl.RData")
      if(file.exists(wi_path)){
        load(wi_path)
        wi <- as.data.table(data)
        remove(data)
      }else{
        wi_patha <- paste0(country,"wi",(as.numeric(phase)-1)) #May be India-specific
        wi_path <- paste0(tolower(wi_patha),"fl.RData")
        if(file.exists(wi_path)){
          load(wi_path)
          wi <- as.data.table(data)
          remove(data)
        } else {
          next;
        }
      }
      setnames(wi,"whhid","hhid")
      pr<- join(pr,wi,by=c("hhid"))
      rm(wi)
      names(pr)[which(names(pr)=="wlthindf")] <-"wealth"
    }
    gc()
    # 
    # # Education
    # if(length(names(pr)[which(names(pr)=="hv109")])==1){
    #   names(pr)[which(names(pr)=="hv109")] <- "education"
    #   recode.educ <- function(x){
    #     if(is.na(x)){return(NA)}
    #     else if(x==8 | x==9){return(NA)}
    #     else if(x==0 | x==1){return("1")}
    #     else if(x==2 | x==3 ){return("2")}
    #     else if(x==4){return("3")}
    #     else if(x==5){return("4")}
    #     else{return(NA)}
    #   }
    # } else{
    #   if(length(names(pr)[which(names(pr)=="v106")])==1){
    #     names(pr)[which(names(pr)=="v106")] <- "education"
    #     recode.educ <- function(x){
    #       if(is.na(x)){return(NA)}
    #       else if(x==8 | x==9){return(NA)}
    #       else if(x==0 ){return("1")}
    #       else if(x==1){return("2")}
    #       else if(x==2){return("3")}
    #       else if(x==3){return("4")}
    #       else{return(NA)}
    #     }
    #   } else {
    #     pr$education <- NA
    #   }
    # }
    # pr$education <- sapply(pr$education,recode.educ)
    
    # Age
    names(pr)[which(names(pr)=="hv105")] <- "age"
    
    #Sleeping
   # if("hv216"&"hv009" %in% names(pr)){
      pr$sleeping <- pr$hv009/pr$hv216
   # }

    
    # # Sex
    # names(pr)[which(names(pr)=="hv104")] <- "sex"
    # names(br)[which(names(br)=="b4")] <- "sex"
    
    # ID vars
    names(pr)[which(names(pr)=="hv001")] <- "cluster"
    names(pr)[which(names(pr)=="hv002")] <- "household"
    #names(pr)[which(names(pr)=="hv024")] <- "region"
    names(pr)[which(names(pr)=="hvidx")] <- "line"
    names(pr)[which(names(pr)=="hv112")] <- "mother.line"
    names(pr)[which(names(pr)=="hv113")] <- "father.line"
    pr$mother.line[which(pr$mother.line==99)] <- NA
    pr$father.line[which(pr$father.line==99)] <- NA
    
    
  #   # Mother Educ
  #   dat <- select(pr, hhid, line, education)
  #   names(dat)[c(2:3)] <- c("mother.line","education_mother")
  #   pr <- left_join(pr,dat,by=c("hhid","mother.line"))
  #   
  #   # father Educ
  #   dat <- select(pr, hhid, line, education)
  #   names(dat)[c(2:3)] <- c("father.line","education_father")
  #   pr <- left_join(pr,dat,by=c("hhid","father.line"))
  #   pr <- as.data.table(pr)
  #   
  #   #max Educ
  #   if(!(class(pr$education) == "list")){
  #     pr[, education_max:=max(education_father, education_mother, na.rm=T), by=1:nrow(pr)]
  #     pr[is.infinite(education_max)]$education_max <- NA
  #   } else
  #   { pr$education_max <- as.numeric(NA)}
  #   
  #   # Head vars
  #   names(pr)[which(names(pr)=="hv219")] <- "head.sex"
  #   names(pr)[which(names(pr)=="hv220")] <- "head.age"
  #   
  #   #Child height for age
  #   if(length(names(pr)[which(names(pr)=="hc70")])){
  #     names(pr)[which(names(pr)=="hc70")] <- "child.height.age"
  #     pr$child.height.age <- pr$child.height.age/100}
  #   if(length(names(pr)[which(names(br)=="hw5")])){
  #     names(br)[which(names(br)=="hw5")] <- "child.height.age"
  #     br$child.height.age <- br$child.height.age/100}
  #   
  #   #Birth registration
  #   names(pr)[which(names(pr)=="hv140")] <- "birth.cert"
  #   #0 - neither certificate or registered
  #   #1 - has certificate
  #   #2 - registered, no certificate
  #   #3 - registered, no certificate
  #   #6 - other
  #   #8 - dk
  #   pr$birth.reg <- as.numeric(NA)
  #   pr$birth.reg[which(pr$birth.cert %in% c(0,6,8,9))] <- 0
  #   pr$birth.reg[which(pr$birth.cert %in% c(1,2,3))] <- 1
  #   
    pr$iso3 <- povcal_subset$iso3
    pr$year <- povcal_subset$surveyyr

    longlist[[longindex]] <- pr
    longindex <- longindex + 1
  }
  # 
  # #region for CMR
  # #   if(toupper(country)=="CM"&length(names(pr)[which(names(pr)=="hv024")])==1){
  # if(country=="cm"){
  #   if(length(names(pr)[which(names(pr)=="hv024")])==1){
  #     names(pr)[which(names(pr)=="hv024")] <- "subnational"
  #     recode.subnational <- function(x){
  #       if(is.na(x)){return(NA)}
  #       else if(x==1){return("1")}
  #       else if(x==2 | x==12){return("2")}
  #       else if(x==4 ){return("3")}
  #       else if(x==5){return("4")}
  #       else if(x==3 | x==6){return("5")}
  #       else if(x==7){return("6")}
  #       else if(x==8){return("7")}
  #       else if(x==10){return("8")}
  #       else if(x==11){return("9")}
  #       else if(x==9){return("10")}
  #       else{return(NA)}
  #     }
  #     pr$subnational <- sapply(pr$subnational,recode.subnational)
  #   }
  # }
  # 
  # else if(toupper(country)=="BD"|toupper(country)=="UG"){
  #   names(pr)[which(names(pr)=="hv024")] <- "subnational"
  # }
  
  # Poverty
  povcalcut <- povcal_subset$hc
  extcut <- povcal_subset$extreme
  u20cut <- povcal_subset$u20
  cuts <- c(povcalcut,extcut,u20cut)
  #if(length(wealthvar) == 0){ next; }
  povperc <- weighted.percentile(pr$wealth,pr$weights,prob=cuts)
  pr$p20 <- (pr$wealth < povperc[1])
  pr$ext <- (pr$wealth < povperc[2])
  pr$u20 <- (pr$wealth > povperc[3])
  message(paste0(rdata_name," ",povcal_subset$RequestYear," ",i,"/",nrow(povcalcuts.years)))
  
  if(!(br_path %in% missing.br)){
    #Merge br
    pr.pov <- pr[,.(p20=mean(p20,na.rm=T)),by=.(cluster,household)]
    pr.pov$p20 <- floor(pr.pov$p20)
    pr.pov$p20 <- as.logical(pr.pov$p20)
    br$p20 <- NA
    br <- merge(br[,p20:=NULL],pr.pov,by=c("cluster","household"),all.x=T)
  }
  # Birth certificate
  if(variable == "birth.registration"){
    #message("Registration")
    
    groupstodo <- groups[groups %in% names(pr)]
    pr.birth.reg <- pr[!(is.na(birth.reg)) & !is.na(weights)]
    
    if(nrow(pr.birth.reg) > 0){
      dat <- dcast.data.table(pr.birth.reg, paste(paste(groupstodo[groupstodo!=variable], collapse=" + "), "+ birth.reg~."), value.var="weights", fun.aggregate=list(sum, length, sum.var))
    } else {
      dat <- data.table(weights_sum=NaN, weights_length=NaN, weights_sum.var=NaN)
    }
    dat$variable <- "registration"
  }
  
  #Education
  if(variable == "education"){
    #message("Registration")
    
    groupstodo <- groups[groups %in% names(pr)]
    pr.education <- pr[!(is.na(education)) & !is.na(weights)]
    
    if(nrow(pr.education) > 0){
      dat <- dcast.data.table(pr.education, paste(paste(groupstodo[groupstodo!=variable], collapse=" + "), "+ education~."), value.var="weights", fun.aggregate=list(sum, length, sum.var))
    } else {
      dat <- data.table(weights_sum=NaN, weights_length=NaN, weights_sum.var=NaN)
    }
    dat$variable <- "education"
  }
  
  if(variable == "p20"){
    #message("Registration")
    
    groupstodo <- groups[groups %in% names(pr)]
    pr.p20 <- pr[!(is.na(p20)) & !is.na(weights)]
    
    if(nrow(pr.p20) > 0){
      dat <- dcast.data.table(pr.p20, paste(paste(groupstodo[groupstodo!=variable], collapse=" + "), "+ p20~."), value.var="weights", fun.aggregate=list(sum, length, sum.var))
    } else {
      dat <- data.table(weights_sum=NaN, weights_length=NaN, weights_sum.var=NaN)
    }
    dat$variable <- "p20"
  }
  
  if(variable == "ext"){
    #message("Registration")
    
    groupstodo <- groups[groups %in% names(pr)]
    pr.ext <- pr[!(is.na(ext)) & !is.na(weights)]
    
    if(nrow(pr.ext) > 0){
      dat <- dcast.data.table(pr.ext, paste(paste(groupstodo[groupstodo!=variable], collapse=" + "), "+ ext~."), value.var="weights", fun.aggregate=list(sum, length, sum.var))
    } else {
      dat <- data.table(weights_sum=NaN, weights_length=NaN, weights_sum.var=NaN)
    }
    dat$variable <- "ext"
  }
  
  if(variable == "sleeping"){
    #message("Registration")
    
    groupstodo <- groups[groups %in% names(pr)]
    pr.ext <- pr[!(is.na(ext)) & !is.na(weights)]
    
    if(nrow(pr.ext) > 0){
      dat <- dcast.data.table(pr.ext, paste(paste(groupstodo[groupstodo!=variable], collapse=" + "), "+ sleeping~."), value.var="weights", fun.aggregate=list(sum, length, sum.var))
    } else {
      dat <- data.table(weights_sum=NaN, weights_length=NaN, weights_sum.var=NaN)
    }
    dat$variable <- "sleeping"
  }
  
  # Stunting
  #{}
  
  #Mortality
  #{}
  
  #Secondary Education
  #{}
  
  dat$iso3 <- povcal_subset$iso3
  dat$povcal_year <- povcal_subset$RequestYear
  dat$survey_year <- povcal_subset$surveyyr
  last_filename <- tolower(substr(povcal_subset$filename,0,6))
  dataList[[dataIndex]] <- dat
  dataIndex <- dataIndex + 1

}
}
#setTxtProgressBar(pb, i)
#close(pb)
data.total.dhs <- rbindlist(dataList, fill=T)
data.total.dhs$datatype <- "dhs"

# save(data.total.dhs,file="../historical_dhsmf.RData")
# save(long.data.dhs.part1, file="../long_dhs_part1.RData")
# save(long.data.dhs.part2, file="../long_dhs_part2.RData")
setwd(wd3)
save(data.total.dhs,file="/historical_dhsmf_sleeping.RData")

####sleeping agg#### #not working
setnames(data.total.dhs, c("weights_sum", "weights_length", "weights_sum.var"), c("value", "sample", "var"))
data.total <- data.total.dhs[!is.nan(value)]
surveys <- unique(data.total[,c("iso3","survey_year","povcal_year","datatype")])
surveys$id <- seq(1,nrow(surveys)) 

var.cols <- names(data.total)[!(names(data.total) %in% c("iso3", "povcal_year", "survey_year", "datatype", "value", "sample", "variable", "var"))]
dums <- sapply(var.cols, function(x) unique(data.total[,..x]), USE.NAMES = F)
dat <- data.table(vars=var.cols, dums=dums)

#dummydata <- expand.grid(c(setNames(list(seq(1,nrow(surveys))), "id"), dums_dat(var.cols), setNames("registration", "variable")))
dummydata <- unique(data.total[,..var.cols])
dummydata <- cbind(dummydata[rep(seq_len(nrow(dummydata)), nrow(surveys)),], id=rep(1:nrow(surveys), each=nrow(dummydata)))
dummydata <- merge(surveys, dummydata, by="id")
dummydata$id <- NULL

data.total <- merge(dummydata, data.total, all.x=T)
rm(dummydata)
gc()
#data.total[is.na(value)]$value <- 0
#data.total[is.na(sample)]$sample <- 0

by.cols <- c("iso3", "povcal_year", var.cols)
agg.cols <- c(by.cols,"survey_year","datatype")
data.total <- data.total[, .(value=sum(value, na.rm=T), sample=sum(sample, na.rm=T), var=sum(var, na.rm=T)), by=agg.cols]

#Year weightings
data.total$diff <- abs(data.total$survey_year - data.total$povcal_year)
data.total[is.na(data.total$value)]$diff <- NA
data.total$diff.sign <- sign(data.total$survey_year - data.total$povcal_year)
pos.data.total <- data.total[diff.sign %in% c(0, 1)]
neg.data.total <- data.total[diff.sign %in% c(0,-1)]
neg.data.total <- neg.data.total[!(diff == 0)]
neg.data.total <- neg.data.total[!(do.call(paste0, neg.data.total[, ..by.cols]) %in% do.call(paste0, pos.data.total[diff == 0][, ..by.cols]))]
pos.data.total <- pos.data.total[pos.data.total[, .I[which.min(diff)], by = by.cols]$V1]
neg.data.total <- neg.data.total[neg.data.total[, .I[which.min(diff)], by = by.cols]$V1]
data.total <- rbind(pos.data.total, neg.data.total)
data.total[, year.weight := (sum(diff) - diff) / sum(diff), by = by.cols]
data.total$diff <- NULL
data.total$diff.sign <- NULL
data.total[year.weight == 0]$year.weight <- 1
data.total[is.nan(year.weight)]$year.weight <- 1

data.total[, value := value / sum(value, na.rm = T), by = .(iso3, povcal_year, survey_year, datatype)]

data.total.out <- data.total[, .(
  value = sum(value * year.weight, na.rm = T) / sum(year.weight, na.rm = T),
  sample = sum(sample, na.rm = T),
  var = sum(year.weight ^ 2 * var, na.rm = T) / sum(year.weight ^ 2, na.rm = T),
  survey_year = paste(survey_year, datatype, collapse = ";")
), by = by.cols]

pop <- as.data.table(WDI(indicator="SP.POP.TOTL", start=min(data.total.out$povcal_year), end=max(data.total.out$povcal_year), extra=T))
pop[iso2c=="KP"]$iso3c <- "PRK"
pop[iso2c=="MD"]$iso3c <- "MKD"
pop[iso2c=="SZ"]$iso3c <- "SWZ"
pop <- setnames(pop[,c("iso3c","year","SP.POP.TOTL")],c("iso3","povcal_year","ReqYearPopulation"))

#Population weightings
data.total.out <- merge(data.total.out, pop, by=c("iso3","povcal_year"), all.x=T)
data.total.out$value <- data.total.out$value * data.total.out$ReqYearPopulation
data.total.out$var <- data.total.out$var * data.total.out$ReqYearPopulation^2

#Rename variable results to plain English
data.total.out <- cbind(data.total.out[,!(..var.cols)], data.table(sapply(data.total.out[,..var.cols], as.character)))
dhs.sleeping <- data.total.out


# UHC <- UHC[UHC[iso3c!="",.I[YEAR==max(YEAR)],by=iso3c]$V1][,c(3,6,8)]
# sleeping <- data.total.dhs[data.total.dhs[,.I[survey_year==max(survey_year)],by=.(iso3,age,sleeping)]$V1]
# sleeping_age <- data.total.dhs[,.(avg=sum(USD_Disbursement_Defl)),by=.(Aid_t,LDC,Year)]
