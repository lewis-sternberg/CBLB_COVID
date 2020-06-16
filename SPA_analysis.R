####Function and setup####
list.of.packages <- c("Hmisc","plyr","foreign","data.table","varhandle","zoo","survey", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
setwd("C:/git/CBLB_COVID/")

country <- fread("data/SPAmeta.csv")
variables <- c("electricity","cleanliness","clean_hands")
num <- 0
vars <- data.table()

#load("SPA/HTFC7AFLSR.RData")

SPAs <- list.files("SPA",pattern = ".RData")
SPAs <- SPAs[grepl("sr.RData",SPAs)]
SPAs <- SPAs[endsWith(strtrim(SPAs,4),"fc")]

dataList <- list()
dataIndex <- 1
pb = txtProgressBar(max=length(SPAs),style=3)
for(i in 1:length(SPAs)){
  setTxtProgressBar(pb, i-1)
  SPA <- SPAs[i]
  load(paste0("SPA/",SPA))
  file <- strtrim(SPA,10)
  country_SPA <- country[Code==toupper(strtrim(SPA,2))]$Country
  
  assign("FC",data)
  setDT(FC)
  rm(data)
  
  keep <- c("v005","v003","v143","v2010a","v2010b","v2010c","v2010d","v120a","v166o","v166p","v166q","v168a1",
            "v168c","v168q","v168o","v168p","v168f", "v168g", "v168j","v168l", "v168r","v168s","v168t","v168u","v154","v121","facwt", "ftype",
            "q113", "q341","u005","u003"
            
  )
  FC <- subset(FC, select=(colnames(FC) %in% keep))
  missing_dat <- c()
  if(ncol(FC)>0){
    
    #Weight
    if("v005" %in% names(FC)){
      names(FC)[which(names(FC)=="v005")] <- "sample.weights"
      FC$weights <- FC$sample.weights/1000000
    } else if("facwt" %in% names(FC)){
      names(FC)[which(names(FC)=="facwt")] <- "sample.weights"
      FC$weights <- FC$sample.weights/1000000
    }else if("u005" %in% names(FC)){
      names(FC)[which(names(FC)=="u005")] <- "sample.weights"
      FC$weights <- FC$sample.weights/1000000
    }
    FC[is.na(weights)]$weights <- 0
    #rural.urban
    #Urban: 1
    #Rural: 2
    if("v003" %in% names(FC)){
      names(FC)[which(names(FC)=="v003")] <- "urban.rural"
      FC[urban.rural==1]$urban.rural <- 1
      FC[urban.rural==2]$urban.rural <- 0
    }else if("ftype" %in% names(FC)){
      names(FC)[which(names(FC)=="ftype")] <- "urban.rural"
      FC[urban.rural==1]$urban.rural <- 1
      FC[urban.rural==2]$urban.rural <- 0
    }else if("u003" %in% names(FC)){
      names(FC)[which(names(FC)=="u003")] <- "urban.rural"
      FC[urban.rural==1]$urban.rural <- 1
      FC[urban.rural==2]$urban.rural <- 0
    }
    #beds - overnight/in-patient
    if("v143" %in% names(FC)){
      names(FC)[which(names(FC)=="v143")] <- "beds"
      FC[beds==9998]$beds <- NA
      FC[beds==9999]$beds <- NA
    } else if("q113" %in% names(FC)){
      names(FC)[which(names(FC)=="q113")] <- "beds"
      FC[beds==998]$beds <- NA
    }
    
    #respiratory providers
    if("v2010a" %in% names(FC)){
      names(FC)[which(names(FC)=="v2010a")] <- "respir_providers"
    }
    
    #electricty
    if("v120a" %in% names (FC)){
      names(FC)[which(names(FC)=="v120a")] <- "electricity"
      # 0  Not connected
      # 1  Connected, always available
      # 2  Connected, sometimes interrupted
      # 3  Connected, DK if interrupted in last 7 days
      # 8  Don't know if connected
      # 9 missing 
      FC[electricity==8]$electricity <- NA
      FC[electricity==9]$electricity <- NA
      FC[electricity==0]$electricity <- 0
      FC[electricity==1]$electricity <- 1
      FC[electricity==2]$electricity <- 1
      FC[electricity==3]$electricity <- 1
    }else if("v121" %in% names (FC)){
      names(FC)[which(names(FC)=="v121")] <- "electricity"
      # 0  No source of electricity
      # 1  Central supply/central and solar always available
      # 2  Solar or other source always available
      # 3  Central supply/central and solar sometimes available, missing avail
      # 4  Solar or other source sometimes available, missing avail
      # 5  No other source, but has generator observed
      # 6  No other source, but has generator reported
      FC[electricity==9]$electricity <- NA
      FC[electricity==0]$electricity <- NA
      FC[electricity==1]$electricity <- 0
      FC[electricity==2]$electricity <- 1
      FC[electricity==3]$electricity <- 1
      FC[electricity==4]$electricity <- 0
      FC[electricity==5]$electricity <- 0
      FC[electricity==6]$electricity <- 0
    }else if("q341" %in% names (FC)){
      names(FC)[which(names(FC)=="q341")] <- "electricity"
      # 1  Always available
      # 2  Sometimes interrupted
      # 8  Don't know
      FC[electricity==8]$electricity <- NA
      FC[electricity==1]$electricity <- 1
      FC[electricity==2]$electricity <- 0
    }
    
    #oxygen, outpaitent
    # 0  Not available
    # 1  Observed, functioning
    # 2  Observed, missing functioning
    # 3  Observed, not/DK if functioning
    # 4  Reported functioning
    # 5  Reported, missing functioning
    # 6  Reported, not/DK if functioning
    # 9  Missing
    recode.O2 <- function(x){
      if(is.na(x)){return(NA)}
      else if(x==1 | x==4){return("1")}
      else if(x==2|x==3|x==5|x==6){return("0")}
      else if(x==9|x==0){return("NA")}
      else{return(NA)}
    }
    if("v166o" %in% names(FC)){
      names(FC)[which(names(FC)=="v166o")] <- "O2_concentrators"
      FC$O2_concentrators <- sapply(FC$O2_concentrators,recode.O2)
    }
    if("v166p" %in% names(FC)){
      names(FC)[which(names(FC)=="v166p")] <- "O2_cylinders"
      FC$O2_cylinders <- sapply(FC$O2_cylinders,recode.O2)
    }
    if("v166q" %in% names(FC)){
      names(FC)[which(names(FC)=="v166q")] <- "O2_distribution"
      FC$O2_distribution <- sapply(FC$O2_distribution,recode.O2)
    }
    #Outpaitent unit saftey
    recode.OPD <- function(x){
      if(is.na(x)){return(NA)}
      else if(x==1 | x==2){return("1")}
      else if(x==0 ){return("0")}
      else if(x==9){return("NA")}
      else{return(NA)}
    }
    
    
    if("v168a1" %in% names (FC)){
      names(FC)[which(names(FC)=="v168a1")] <- "water"
      FC$water <- sapply(FC$water,recode.OPD)
    }
    if("v168c" %in% names (FC)){
      names(FC)[which(names(FC)=="v168c")] <- "soap"
      FC$soap <- sapply(FC$soap,recode.OPD)
    }
    if("v168q" %in% names (FC)){
      names(FC)[which(names(FC)=="v168q")] <- "alcohol_hand_gel"
      FC$alcohol_hand_gel <- sapply(FC$alcohol_hand_gel,recode.OPD)
    }
    if("v168o" %in% names (FC)){
      names(FC)[which(names(FC)=="v168o")] <- "pedal_bin"
      FC$pedal_bin <- sapply(FC$pedal_bin,recode.OPD)
    }
    if("v168p" %in% names (FC)){
      names(FC)[which(names(FC)=="v168p")] <- "other_waste"
      FC$other_waste <- sapply(FC$other_waste,recode.OPD)
    }
    if("v168g" %in% names (FC)){
      names(FC)[which(names(FC)=="v168g")] <- "gloves"
      FC$gloves <- sapply(FC$gloves,recode.OPD)
    }
    if("v168j" %in% names (FC)){
      names(FC)[which(names(FC)=="v168j")] <- "disinfectant"
      FC$disinfectant <- sapply(FC$disinfectant,recode.OPD)
    }
    if("v168r" %in% names (FC)){
      names(FC)[which(names(FC)=="v168r")] <- "masks"
      FC$masks <- sapply(FC$masks,recode.OPD)
    }
    if("v168s" %in% names (FC)){
      names(FC)[which(names(FC)=="v168s")] <- "gowns"
      FC$gowns <- sapply(FC$gowns,recode.OPD)
    }
    if("v168t" %in% names (FC)){
      names(FC)[which(names(FC)=="v168t")] <- "eye_protection"
      FC$eye_protection <- sapply(FC$eye_protection,recode.OPD)
    }
    
    FC$clean_hands <- "missing"
    if("soap" %in% names(FC)&"water" %in% names(FC)){
      FC[is.na(water)|is.na(soap)]$clean_hands <- NA
      FC[water=="1"&soap=="1"]$clean_hands <- "1"
    }
    if("alcohol_hand_gel" %in% names(FC)){
      FC[is.na(alcohol_hand_gel)&clean_hands!="1"]$clean_hands <- NA
      FC[alcohol_hand_gel=="1"]$clean_hands <- "1"
    }
    if(!all(FC$clean_hands=="missing")){
      FC[clean_hands=="missing"]$clean_hands <- "0"
    }else{
      FC$clean_hands <- NULL
    }
    
    #general cleanliness score out of 8
    # General cleanliness: Floor swept, no obvious dirt
    # General cleanliness: Counters/tables/chairs wiped
    # General cleanliness: Needles/sharps outside of sharps container
    # General cleanliness: Sharps box overflowing/torn/pierced
    # General cleanliness: Bandages/ infectious waste lying uncovered
    # General cleanliness: Walls with significant damage
    # General cleanliness: Doors with significant damage
    # General cleanliness: Ceiling with water stains/ damage
    
    if("v154" %in% names(FC)){
      names(FC)[which(names(FC)=="v154")] <- "cleanliness"
      FC$full_cleanliness <- 0
      FC[cleanliness==8]$full_cleanliness <- 1
    }
    
    survey <- data.table(names(FC))
    survey$file <- file
    vars <- rbind(vars,survey)
    
    if("weights" %in% names(FC)){
      dsn <- svydesign(
        data=FC
        ,ids=~1
        ,weights=~weights
      )
      #electricity
      for(variable in variables){
        if(variable=="electricity"){
          if("electricity" %in% names(FC)&"urban.rural" %in% names(FC) &  !is.na(all(FC$urban.rural))){
            elec <- svytable(~electricity+urban.rural,dsn)
            elec.urban <- elec["1","1"]
            elec.rural <- elec["1","0"]
            non_elec.urban <- elec["0","1"]
            non_elec.rural <- elec["0","0"]
            
            elec.urban.numerator <- elec.urban
            elec.urban.denominator <- sum(elec.urban,non_elec.urban,na.rm=T)
            elec.urban.stat <- elec.urban.numerator/elec.urban.denominator
            
            elec.rural.numerator <- elec.rural
            elec.rural.denominator <- sum(elec.rural,non_elec.rural,na.rm=T)
            elec.rural.stat <- elec.rural.numerator/elec.rural.denominator
            dat <- data.frame(
              urban=c(rep("1",1),rep("0",1)),
              variable=c(rep("constant_electricity",2)),
              type=rep(c("statistic","numerator","denominator"),2),
              value =c(elec.urban.stat,elec.urban.numerator,elec.urban.denominator,
                       elec.rural.stat,elec.rural.numerator,elec.rural.denominator)
            )
            if(exists("dat")){
              dat$country <- country_SPA
              dat$file <- file
              dataList[[dataIndex]] <- dat
              dataIndex<- dataIndex + 1
            }
          }
          if("electricity" %in% names(FC)){
            elec.tbl <- svytable(~electricity,dsn)
            elec <- elec.tbl["1"]
            non_elec <- elec.tbl["0"]
            
            elec.numerator <- elec
            elec.denominator <- sum(elec,non_elec,na.rm=T)
            elec.stat <- elec.numerator/elec.denominator
            dat <- data.frame(
              variable=c("constant_electricity"),
              type=c("statistic","numerator","denominator"),
              value =c(elec.stat,elec.numerator,elec.denominator)
            )
            if(exists("dat")){
              dat$country <- country_SPA
              dat$file <- file
              dataList[[dataIndex]] <- dat
              dataIndex<- dataIndex + 1
            }
          }
        }
        #cleanliness
        if(variable=="cleanliness"){
          if("cleanliness" %in% names(FC)&"urban.rural" %in% names(FC)&!is.na(all(FC$urban.rural))){
            clean <- svytable(~full_cleanliness+urban.rural,dsn)
            clean.urban <- clean["1","1"]
            clean.rural <- clean["1","0"]
            non_clean.urban <- clean["0","1"]
            non_clean.rural <- clean["0","0"]
            
            clean.urban.numerator <- clean.urban
            clean.urban.denominator <- sum(clean.urban,non_clean.urban,na.rm=T)
            clean.urban.stat <- clean.urban.numerator/clean.urban.denominator
            
            clean.rural.numerator <- clean.rural
            clean.rural.denominator <- sum(clean.rural,non_clean.rural,na.rm=T)
            clean.rural.stat <- clean.rural.numerator/clean.rural.denominator
            dat <- data.frame(
              urban=c(rep("1",1),rep("0",1)),
              variable=c(rep("cleanliness",2)),
              type=rep(c("statistic","numerator","denominator"),2),
              value =c(clean.urban.stat,clean.urban.numerator,clean.urban.denominator,
                       clean.rural.stat,clean.rural.numerator,clean.rural.denominator)
            )
            
            if(exists("dat")){
              dat$country <- country_SPA
              dat$file <- file
              dataList[[dataIndex]] <- dat
              dataIndex<- dataIndex + 1
            }
          }
          if("cleanliness" %in% names(FC)){
            clean.tbl <- svytable(~full_cleanliness,dsn)
            clean <- clean.tbl["1"]
            non_clean <- clean.tbl["0"]
            
            clean.numerator <- clean
            clean.denominator <- sum(clean,non_clean,na.rm=T)
            clean.stat <- clean.numerator/clean.denominator
            dat <- data.frame(
              variable=c("cleanliness"),
              type=c("statistic","numerator","denominator"),
              value =c(clean.stat,clean.numerator,clean.denominator)
            )
            if(exists("dat")){
              dat$country <- country_SPA
              dat$file <- file
              dataList[[dataIndex]] <- dat
              dataIndex<- dataIndex + 1
            }
          }
        }
        #ability to clean hands - either soap and water or alcohol based hand gel
        if(variable=="clean_hands"){
          if("clean_hands" %in% names(FC)&"urban.rural" %in% names(FC)&!is.na(all(FC$urban.rural))){
            clean_hands <- svytable(~clean_hands+urban.rural,dsn)
            clean_hands.urban <- clean_hands["1","1"]
            clean_hands.rural <- clean_hands["1","0"]
            non_clean_hands.urban <- clean_hands["0","1"]
            non_clean_hands.rural <- clean_hands["0","0"]
            
            clean_hands.urban.numerator <- clean_hands.urban
            clean_hands.urban.denominator <- sum(clean_hands.urban,non_clean_hands.urban,na.rm=T)
            clean_hands.urban.stat <- clean_hands.urban.numerator/clean_hands.urban.denominator
            
            clean_hands.rural.numerator <- clean_hands.rural
            clean_hands.rural.denominator <- sum(clean_hands.rural,non_clean_hands.rural,na.rm=T)
            clean_hands.rural.stat <- clean_hands.rural.numerator/clean_hands.rural.denominator
            dat <- data.frame(
              urban=c(rep("1",1),rep("0",1)),
              variable=c(rep("ability_to_clean_hands",2)),
              type=rep(c("statistic","numerator","denominator"),2),
              value =c(clean_hands.urban.stat,clean_hands.urban.numerator,clean_hands.urban.denominator,
                       clean_hands.rural.stat,clean_hands.rural.numerator,clean_hands.rural.denominator)
            )
            
            if(exists("dat")){
              dat$country <- country_SPA
              dat$file <- file
              dataList[[dataIndex]] <- dat
              dataIndex<- dataIndex + 1
            }
          }
          if("clean_hands" %in% names(FC)){
            clean_hands.tbl <- svytable(~clean_hands,dsn)
            clean_hands <- clean_hands.tbl["1"]
            non_clean_hands <- clean_hands.tbl["0"]
            
            clean_hands.numerator <- clean_hands
            clean_hands.denominator <- sum(clean_hands,non_clean_hands,na.rm=T)
            clean_hands.stat <- clean_hands.numerator/clean_hands.denominator
            dat <- data.frame(
              variable=c("ability_to_clean_hands"),
              type=c("statistic","numerator","denominator"),
              value =c(clean_hands.stat,clean_hands.numerator,clean_hands.denominator)
            )
            if(exists("dat")){
              dat$country <- country_SPA
              dat$file <- file
              dataList[[dataIndex]] <- dat
              dataIndex<- dataIndex + 1
            }
          }
        }
      }
    }
  }
}
data.total <- rbindlist(dataList,fill=T)
save(data.total, file="Output/SPA_output.RData")
