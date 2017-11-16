## R scripts to harmonise family planning variable and estimate family planning indicators by marital status and age from DHS micro-data files
# 1. 'DHS_Translate.R' Translates relevant variables across surveys and stores harmonised variable names and codes as R data sets
# 2. 'DHS_Categorization.R' Computes marital status and contraceptive use variables
# 3. 'DHS_GenerateUnmet.R' Computes unmet need variable based on DHS code [http://dhsprogram.com/topics/unmet-need.cfm]
# 4. 'DHS_output_FP-Indicators.R' Outputs table of family planning indicators by marital status and age
## Author: United Nations Population Division (Ching Yee Lin, Philipp Ueffing, Stephen Kisambira and Aisha Dasgupta)
## Project: Making family planning count 
# [http://www.un.org/en/development/desa/population/projects/making-family-planning-count/index.shtml]
# [http://www.un.org/en/development/desa/population/theme/family-planning/index.shtml]
## DHS micro data sets need to be downloaded from the DHS program website [https://dhsprogram.com/]


#Compute output table

setwd("v:/FertilitySection/Alkema_Joint project on contraceptive use trends/1_All-women-estimates&projections/Data-tabulations/DHS")
#Translate new surveys
source("./RCode/DHS_Translate.R")

rm(list = ls())
#############LIBRARIES##############
library(foreign)
library(stats)
library(plyr)  
library(dplyr)
library(reshape2)
library(tidyr)
library(tools)
####################################
#####DIRECTORIES AND FILE LISTS#####
setwd("v:/FertilitySection/Alkema_Joint project on contraceptive use trends/1_All-women-estimates&projections/Data-tabulations/DHS")
dhs.master <- "V:/DHS/DHSMaster.csv"
#Load in DHS inventory list for standard surveys and individual record is available
dhs.list <- read.csv(dhs.master, header=TRUE, stringsAsFactors=FALSE, na.strings=c("..", "NA", "", " "))

dhs.list <- subset(dhs.list, !is.na(Survey.code) & !is.na(Individual.Recode) &
                     (Type!="MIS" & Type=="Standard DHS" | Type=="Interim DHS" | Type=="Continuous DHS") & !is.na(Recode))

#Exclude Brazil (Not nationally representative)
dhs.list <- subset(dhs.list, !Survey.code %in% c("br21"))

## List of special surveys need special unmet need calculation
specialSurveys <- c("br31", "kh61", "co60", "ci35", "ga41", "co60", "ci35", "ga41",
                    "gu34", "gu41" , "ht31" , "ia23" , "ia42", "ia52" , "jo42" , "kk42" ,
                    "ls60", "md21" , "mv50" , "mr42" , "ma43", "np51" , "ni22" , "tz3a" ,
                    "tz41", "tz60" , "tr31" , "tr4a" , "ug33", "ye21") 

#List of translated surveys
translated.list <- file_path_sans_ext(dir("./Translated_RDataFiles",pattern=".RData"))
#Retain the translated surveys that exist in dhs.list
working.list <- translated.list[translated.list %in% dhs.list$Individual.Recode | translated.list %in% dhs.list$Survey.code]
####################################

########FILES OUTPUT SETTING########
FileSetting <- function(){
  fileout <- "DHS_CPTYPE_by_MARSTAT_WIDE"
  filelong <- "DHS_CPTYPE_by_MARSTAT_LONG"
  fileMETHlong <- "DHS_CPMETHOD_by_MARSTAT_LONG"
  fileMETH <- "DHS_CPMETHOD_by_MARSTAT_WIDE"
  
  file.list <- c(fileout, filelong,fileMETHlong,fileMETH)

  ## Check existence of output file
  for (i in 1:length(file.list)){
    if(file.exists(paste0(file.list[i],".csv"))){
      #Remove completely if it is empty
      if(file.size(paste0(file.list[i],".csv"))==0){
        file.remove(paste0(file.list[i],".csv"))
      }else{
        #Move to Prior folder if it is not empty
        file.rename(paste0(file.list[i],".csv"), paste0("Prior/",file.list[i], "_", substr(file.info(paste0(file.list[i],".csv"))$ctime,1,10),".csv"))
      }
    }
    file.list[i]<-paste0(file.list[i],".csv")
  }
  
  ## Create output file 
  file.create(file.list)
  return(file.list)
}
####################################

#######Calculation Functions########
CrossTab <- function(VarWeight,VarMarital, VarMethod,df,Formula){
  AllWomen <- sum
  if(Formula==T){
    if(VarWeight==T){
      tab_All <- as.data.frame(addmargins(xtabs(weights~get(VarMarital)+get(VarMethod), ir.data),1,FUN=AllWomen))%>%
        dcast(get.VarMarital.~get.VarMethod.,value.var="Freq")%>%
        mutate(agegroup = "[Total]")
      tab_Age <- as.data.frame(addmargins(xtabs(weights~get(VarMarital)+agegroup+get(VarMethod),ir.data),1,FUN=AllWomen))%>%
        dcast(get.VarMarital.+agegroup~get.VarMethod.,value.var="Freq")
    }else{
      tab_All <- as.data.frame(addmargins(xtabs(~get(VarMarital)+get(VarMethod),ir.data),1,FUN=AllWomen)) %>%
        dcast(get.VarMarital.~get.VarMethod.,value.var="Freq")%>%
        mutate(agegroup="[Total]")
      tab_Age <- as.data.frame(addmargins(xtabs(~get(VarMarital)+agegroup+get(VarMethod),ir.data),1, FUN=AllWomen))%>%
        dcast(get.VarMarital.+agegroup~get.VarMethod.,value.var="Freq")
    }
  }else{
    if(VarWeight==T){
      tab_All <- as.data.frame(xtabs(weights~get(VarMarital)+get(VarMethod),ir.data))%>%
        dcast(get.VarMarital.~get.VarMethod.,value.var="Freq")%>%
        mutate(agegroup = "[Total]")
      tab_Age <- as.data.frame(xtabs(weights~get(VarMarital)+agegroup+get(VarMethod),ir.data))%>%
        dcast(get.VarMarital.+agegroup ~ get.VarMethod.,value.var="Freq")
    }else{
      tab_All <- as.data.frame(xtabs(~get(VarMarital)+get(VarMethod),ir.data))%>%
        dcast(get.VarMarital.~get.VarMethod.,value.var="Freq")%>%
        mutate(agegroup = "[Total]")
      tab_Age <- as.data.frame(xtabs(~get(VarMarital)+agegroup+get(VarMethod),ir.data))%>%
        dcast(get.VarMarital.+agegroup ~ get.VarMethod.,value.var="Freq")
    }
  }
  
  names(tab_All)[which(names(tab_All)=="get.VarMarital.")] <- "mstatus"
  names(tab_Age)[which(names(tab_Age)=="get.VarMarital.")] <- "mstatus"
  
  tab <- full_join(tab_All,tab_Age)
  
  return(tab)
}
CP_OUTPUT <- function(choice){
  #Remove observations when 
  ## 1. marital status is missing
  ### NA retained for married women in mstatusBinary
  ## 2. Method is NA/missing
  ## 3. Agegroup is NA
  ir.data <- filter(ir.data, (mstatus != 9 | is.na(mstatus)) ,method!="Unknown",!is.na(method),!is.na(agegroup))
  if(choice == "Both"){
    cp_MW <- CP_OUTPUT("MW")
    cp_UMW <- CP_OUTPUT("UMW")
    
    final <- rbind(cp_MW,cp_UMW)
    
    tTYPE <- ResetRecords(final)
    
    return(tTYPE)
    
  }else if(choice == "MW"){
    cp<-CrossTab(T,"mstatusBinary","method",df,T)
    samplesize <- CrossTab(F,"mstatusBinary","method",df,T)
    umn <- CrossTab(T,"mstatusBinary","unmettot",df,T)
  }else if(choice == "UMW"){
    cp<-CrossTab(T,"mstatus","method",df,F)
    samplesize<-CrossTab(F,"mstatus","method",df,F)
    umn <- CrossTab(T,"mstatus","unmettot",df,F)
  }
  
  samplesize$n_unweighted <- rowSums(samplesize[,c("Not_using_any_method","Using_modern_method","Using_traditional_method")])
  samplesize$nAny_unweighted <- rowSums(samplesize[,c("Using_modern_method","Using_traditional_method")])
  samplesize <- samplesize[,c("mstatus","agegroup","n_unweighted","nAny_unweighted")]
  
  cp <- full_join(cp,samplesize)
  final <- full_join(cp, umn, by= c("mstatus", "agegroup")) %>%
    mutate(cpModern = Using_modern_method / (Not_using_any_method + Using_modern_method + Using_traditional_method) * 100,
           cpTrad = Using_traditional_method / (Not_using_any_method + Using_modern_method + Using_traditional_method) *100,
           cpAny = (Using_modern_method + Using_traditional_method) / (Not_using_any_method + Using_modern_method + Using_traditional_method) *100,
           umn = Unmet_need / (Unmet_need + No_unmet_need) * 100,
           demandSatisfied_modern = ifelse(!is.na(umn),(Using_modern_method) / (Using_modern_method + Using_traditional_method + Unmet_need) * 100,NA),
           CountryName = SurveyInfo$CountryName.UN,
           LocID = SurveyInfo$LocID,
           CatalogID = SurveyInfo$CatalogID,
           Universe = SurveyInfo$Sample.Type.Female,
           DateToday = Sys.Date(),
           Phase = SurveyInfo$Phase,
           SampleType = SurveyInfo$Type,
           SurveyName = SurveyInfo$SurveyName,
           StartYear = SurveyInfo$StartYear,
           EndYear = SurveyInfo$EndYear,
           StartDate = SurveyInfo$StartDate,
           EndDate = SurveyInfo$EndDate,
           RefDate = SurveyInfo$RefDate
    ) 
  
  tTYPE <- select(final, CountryName : Universe, mstatus,agegroup,Not_using_any_method:agegroup, No_unmet_need:demandSatisfied_modern, DateToday : RefDate, n_unweighted,nAny_unweighted)
  return(tTYPE)
}
CP_METH <- function(choice){
  AllWomen <- sum
  if(choice == "Both"){
    cpMETH_MW<-CP_METH("MW")
    if(nrow(as.data.frame(xtabs(weights ~ mstatus + methodspecific_lab,ir.data)))>0){
      cpMETH_UMW<-CP_METH("UMW")
      cpMETH <- rbind(cpMETH_MW,cpMETH_UMW)
    }else{
      cpMETH <- cpMETH_MW
    }
    return(cpMETH)
  }else{
    if(choice=="MW"){
      cpMETH_All <- as.data.frame(addmargins(xtabs(weights ~ mstatusBinary + methodspecific_lab,ir.data),1,FUN=AllWomen)) %>%
        mutate(agegroup ="[Total]")
      cpMETH_Age <- as.data.frame(addmargins(xtabs(weights ~ mstatusBinary + methodspecific_lab+agegroup,ir.data),1,FUN=AllWomen))
    }else if (choice == "UMW"){
      cpMETH_All <- as.data.frame(xtabs(weights ~ mstatus + methodspecific_lab,ir.data)) %>%
        mutate(agegroup = "[Total]")
      cpMETH_Age <- as.data.frame(xtabs(weights ~ mstatus + methodspecific_lab + agegroup, ir.data))
    }
    if(nrow(cpMETH_All) >0 & nrow(cpMETH_Age)>0){
      cpMETH <- full_join(cpMETH_All,cpMETH_Age)
      if("mstatusBinary" %in% colnames(cpMETH)){
        colnames(cpMETH)[colnames(cpMETH)=="mstatusBinary"] <- "mstatus"
      }
      cpMETH <- cpMETH %>%
        arrange(mstatus,agegroup)%>%
        mutate(CountryName = SurveyInfo$CountryName.UN,
               LocID = SurveyInfo$LocID,
               CatalogID = SurveyInfo$CatalogID,
               Universe = SurveyInfo$Sample.Type.Female,
               SurveyName = SurveyInfo$SurveyName,
               StartYear = SurveyInfo$StartYear,
               EndYear = SurveyInfo$EndYear,
               StartDate = SurveyInfo$StartDate,
               EndDate = SurveyInfo$EndDate,
               RefDate = SurveyInfo$RefDate
               )
      return (cpMETH)
    }else{
      return (NULL)
    }
  }
  
}
####################################

######Output Cleaning Functions#####
ResetRecords <- function(t){
  mstat.list <- c("Formerly married","Never married")
  group.list <- c("AllWomen","Unmarried/Not-in-union")
  
  for(m in mstat.list){
    #Adjust for the new surveys that do not ask CP questions to unmarried women
    if(t$Using_modern_method[which(t$mstatus==m & t$agegroup == "[Total]")]==0 & 
       t$Using_traditional_method[which(t$mstatus==m & t$agegroup == "[Total]")]==0){
      t$cpAny[which(t$mstatus==m)]<-NA
      t$cpModern[which(t$mstatus==m)]<-NA
      t$cpTrad[which(t$mstatus==m)]<-NA
      t$umn[which(t$mstatus==m)]<-NA
      t$demandSatisfied_modern[which(t$mstatus==m)]<-NA
      
      for(g in group.list){
        t$cpAny[which(t$mstatus==g)]<-NA
        t$cpModern[which(t$mstatus==g)]<-NA
        t$cpTrad[which(t$mstatus==g)]<-NA
        t$umn[which(t$mstatus==g)]<-NA
        t$demandSatisfied_modern[which(t$mstatus==g)]<-NA
      }
    }
    if(t$Unmet_need[which(t$mstatus==m &t$agegroup=="[Total]")] ==0){
      t$umn[which(t$mstatus==m)]<-NA
      t$demandSatisfied_modern[which(t$mstatus==m)]<-NA
      
      for(g in group.list){
        t$umn[which(t$mstatus==g)]<-NA
        t$demandSatisfied_modern[which(t$mstatus==g)]<-NA
      }
    }
  }
  
  #Adjust for Ever married sample
  if(all(t$Universe=="Ever Married")){
    t <- subset(t, subset=t$mstatus!="Unmarried/Not-in-union")
    for(r in 13:17){
      t[(which(t$mstatus=="AllWomen")),r]<-NA
      if(r<12){
        if(all(t[(which(t$mstatus=="Never married")),r]==0)){
          t[(which(t$mstatus=="Never married")),r] <- NA
        }
      }
    }
  }
  
  #Adjust for DHS-I
  t$umn[which(t$Phase == "DHS-I")]<-NA
  t$No_unmet_need[which(t$Phase == "DHS-I")]<-NA
  t$Unmet_need[which(t$Phase == "DHS-I")]<-NA
  
  return(t)
}
Output <- function(tTYPE,tMETH){
  if (file.size(file.list[1]) == 0){
    # if the csv output file is empty append the computed values to the output file but output the column names first, that is,in the first row
    write.table(tTYPE, file = file.list[1], append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    # if the csv output file already has observations in it append the results to the output file without displaying column names each time data is outputted
    write.table(tTYPE, file = file.list[1], append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  }
  
  #Output for tMETH
  if (file.size(file.list[3]) == 0){
    # if the csv output file is empty append the computed values to the output file but output the column names first, that is,in the first row
    write.table(tMETH, file = file.list[3], quote = TRUE, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    # if the csv output file already has observations in it append the results to the output file without displaying column names each time data is outputted
    write.table(tMETH, file = file.list[3], quote = TRUE, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  }
}
Transform <- function(){
  df <- read.csv(file.list[1])
  
  t <- df %>% group_by(CountryName, LocID, CatalogID, Universe, agegroup, Phase, SampleType, SurveyName, StartYear, EndYear, StartDate, EndDate, RefDate, n_unweighted, nAny_unweighted, totalDemand,`WCUMA_nAny_totalAge_.10`,	`WCUMA_n.50`,	`WCUMA_totalDemand.5`) %>%
    summarise_at(vars(Not_using_any_method, Using_modern_method, Using_traditional_method, Unknown, No_unmet_need, Unmet_need), sum) %>%
    mutate(mstatus = "Total",
           DateToday = as.factor(Sys.Date()),
           cpModern = Using_modern_method / (Not_using_any_method + Using_modern_method + Using_traditional_method) * 100,
           cpTrad = Using_traditional_method / (Not_using_any_method + Using_modern_method + Using_traditional_method) *100,
           cpAny = (Using_modern_method + Using_traditional_method) / (Not_using_any_method + Using_modern_method + Using_traditional_method) *100,
           umn = Unmet_need / (Unmet_need + No_unmet_need) * 100,
           demandSatisfied_modern = ifelse(!is.na(umn),(Using_modern_method) / (Using_modern_method + Using_traditional_method + Unmet_need) * 100,NA)
    )
  
  tt <- rbind.data.frame(df, t) %>%
    select(-c(Not_using_any_method, Using_modern_method, Using_traditional_method, Unmet_need, No_unmet_need)) %>%
    filter(mstatus != "Marital Status, Missing") %>%
    gather(Indicator, Data.Value, cpModern:demandSatisfied_modern)
  
  write.table(tt, file.list[2] ,quote=TRUE,sep=",",row.names=F)
  
  #tMETH
  tLong <- read.csv(file = file.list[3])
  colList <- c("Pill","Daily pill","Monthly pill","IUD","Norplant/Implants","Condom","Female Condom","Female Sterilization","Male Sterilization","Patch","Ring","Injections","Injection (3 monthly)","Injection (monthly)",
               "Diaphragm/Foam/Jelly","Diaphragm","Diaphragm/Foam","Diaphragm/Jelly","Foam or Jelly","Foaming tablets","Vaginal methods","Lactational amenorrhea (LAM)","Prolonged breastfeeding","Emergency contraception",
               "Other modern method","Abstinence or periodic abstinence","Periodic abstinence","Cycle Beads/Standard days method","Abstinence","Mucus method","Temperature","Other Rhythm/Calendar/Periodic Abstinence",
               "Natural family planning, unspecified","Withdrawal","Other traditional/folkloric","Herbs/Plants","Gris-Gris/Amulet","Astrology","Strings","Massage","Douche","OTHER METHOD, UNSPECIFIED","Other specific method 1"
               ,"Other specific method 2","Other specific method 3","Other specific method 4","modernUser", "traditionalUser", "totalUser", "NotUsing")
  
  tWide <- tLong %>%
    filter(mstatus %in% c("Married/In-union", "Unmarried/Not-in-union", "AllWomen", "Formerly in-union", "Neverin-union", "Unmarried")) %>%  #filters out surveys from tLong which did not have method specific variable or other problems
    dcast(CatalogID + LocID + SurveyName + CountryName + StartYear +EndYear + mstatus + agegroup ~ methodspecific_lab, value.var = "Freq")
  
  for (j in colList[!colList %in% names(tWide)]) {
    tWide[j] <- NA
  }
  
  tWide <- tWide[c(names(tWide)[!names(tWide) %in% colList], colList)]
  
  tWide <- tWide %>%
    mutate(modernUser = rowSums(cbind(Pill, `Daily pill`, `Monthly pill`, IUD, `Norplant/Implants`, Condom, `Female Condom`, `Female Sterilization`,
                                      `Male Sterilization`,Patch, Ring, Injections, `Injection (3 monthly)`,`Injection (monthly)`,`Diaphragm/Foam/Jelly`,
                                      Diaphragm, `Diaphragm/Foam`, `Diaphragm/Jelly`, `Foam or Jelly`, `Foaming tablets`,`Vaginal methods`, `Lactational amenorrhea (LAM)`,
                                      `Prolonged breastfeeding`,`Emergency contraception`,`Other modern method`),na.rm=T),
           traditionalUser = rowSums(cbind(`Abstinence or periodic abstinence`,`Periodic abstinence`,`Cycle Beads/Standard days method`, Abstinence,
                                           `Mucus method`, Temperature,`Other Rhythm/Calendar/Periodic Abstinence`, `Natural family planning, unspecified`,
                                           Withdrawal,`Other traditional/folkloric`,`Herbs/Plants`,`Gris-Gris/Amulet`,Astrology,Strings,Massage,Douche),na.rm=T),
           totalUser = modernUser+traditionalUser,
           NotUsing = `Not using`,
           totalN = rowSums(cbind(modernUser, traditionalUser, NotUsing), na.rm = TRUE)
  )
  
  for (j in colList) {
    colTitle <- paste("CP", j, sep = "_")
    tWide[colTitle] <- tWide[j] / rowSums(tWide[, c("modernUser", "traditionalUser", "NotUsing")], na.rm = TRUE) * 100
  }
  
  write.table(tWide, file = file.list[4], quote = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  
}
####################################

#########RUNNING WITH FILES#########
source("./RCode/DHS_GenerateUnmet.R")
source("./RCode/DHS_Categorization.R")

file.list<-FileSetting()

for(i in 1:length(working.list)){
  SurveyID <- working.list[i]
  print (SurveyID)
  
  SurveyInfo <- subset(dhs.list,Survey.code == SurveyID)
  load(paste("./Translated_RDataFiles/",SurveyInfo$Survey.code,".RData",sep=""))
  
  if(SurveyID %in% specialSurveys){
    ir.data <- ir.data[,c(grep("v000|v001|v002|v003|v005|V007|v008|v011|v012|v013|v015|v016|v020|v021|v022|v023|v024|v025|v213|v215|v222|v225|v302|v3a08d|v302a|v312|v313|v375a|v376|v512|v525|v528|v529|v536|v502|V602|v605|b3.01|m6.1|m10.1|s313|s309b|s607c|s607d|method|methodSpecific|weights", 
                               names(ir.data),ignore.case=TRUE))]
  }else{
    ir.data <- ir.data[,c(grep("v000|v001|v002|v003|v005|V007|v008|v011|v012|v013|v015|v016|v020|v021|v022|v023|v024|v025|v213|v215|v222|v225|v302|v3a08d|v302a|v312|v313|v375a|v376|v512|v525|V527|v528|v529|v536|v501|v502|v602|v605|v613|v614|b3.01|m6.1|m10.1|s313|awfactt|awfactu|awfactr|awfacte|awfactw|method|methodSpecific|weights",
                               names(ir.data), ignore.case=TRUE))]
  }
  
  colnames(ir.data) <- tolower(colnames(ir.data))
  
  #Restrict Sexually Active Sample
  # ir.data$sexact <- NA

  #1 month
   # if(SurveyID=="bf21"){
   #   ir.data$sexact[which(ir.data$v527>=0 & ir.data$v527<300)]<-1
   #   ir.data$sexact[which(ir.data$v527 > 300)] <- 2
   # }else{
   #   ir.data$sexact[which(ir.data$v528>=0 & ir.data$v528<=30)] <- 1
   #   if(SurveyID %in% specialSurveys){
   #     ir.data$sexact[which(is.na(ir.data$sexact) & ir.data$v528==95)] <- 1
   #   }else{
   #     ir.data$sexact[which(ir.data$v528 > 30)] <- 2
   #     ir.data$sexact[which(ir.data$v536 == 0)] <- 3
   #   }
   # }

  # #3 months
  # ir.data$sexact[which((ir.data$v527>=0 & ir.data$v527<=190) |
  #                        (ir.data$v527>=200 & ir.data$v527<=212)|
  #                        (ir.data$v527>=300 & ir.data$v527<=303))] <- 1
  # 
  #  
  # if(all(ir.data$sexact==0) | all(is.na(ir.data$sexact))){
  #   next()
  # }else{
  #   ir.data <- subset(ir.data,subset=ir.data$sexact==1)
  # }
  
  
  #Calculate Unmet Estimates
  ## v213: Currently Pregnant
  ## v225: Wantedness of Current Pregnancy
  ## v312: Current CP Method
  ## m10_1: Wantedness of Last Child
  ## m6_1: PPA Duration
  ## v605: Wantedness of Future Child
  ## b3_01: Date of last Birth
  ## v008: Date of Interview
  
  UnmetVar.list <- c("v213","v225","v312","m10.1","m6.1","v605","b3.01","v008")
  
  if(all(toupper(UnmetVar.list) %in% toupper(names(ir.data)))){
    ir.data <- Unmet(ir.data)
  }else{
    ir.data$unmettot <- NA
    ir.data$unmet <- NA
    ir.data$sexact <-NA
  }
  
  #Categorize Marital Status, Age 
  ir.data <- MapVal(ir.data)
  
  #Calculate CP Estimates
  tTYPE <- CP_OUTPUT("Both")
  tMETH <- CP_METH("Both")
  
  tTYPE$totalDemand <- tTYPE$cpAny + tTYPE$umn
  
  #WMCUMA = Indicator of whether sample size of unmarried for 15-49 < 10
  ## <10 = Exclude
  ## >=10 = Publish
  tTYPE$`WCUMA_nAny_totalAge_<10` <- NA
  
  if(any(tTYPE$Universe == "Ever married" | !"Unmarried/Not-in-union" %in% tTYPE$mstatus)){
    if(any(tTYPE$nAny_unweighted[which(tTYPE$mstatus == "Formerly married" & tTYPE$AGE5YEAR_LAB == "[Total]")]<10)){
      tTYPE$`WCUMA_nAny_totalAge_<10`[which(tTYPE$mstatus=="Formerly married" | tTYPE$mstatus=="Never marred" | tTYPE$mstats == "AllWomen")] <- "Exculde"
    }
  }else if(any(tTYPE$nAny_unweighted[which(tTYPE$mstatus=="Unmarried/Not-in-union" & tTYPE$agegroup=="[Total]")] < 10)){
    tTYPE$`WCUMA_nAny_totalAge_<10`[which(tTYPE$mstatus == "Unmarried/Not-in-union" | tTYPE$mstatus == "Formerly in-union" | tTYPE$mstatus == "Neverin-union" | tTYPE$mstats == "AllWomen")] <- "Exclude"
  }
  tTYPE$`WCUMA_nAny_totalAge_<10`[which(is.na(tTYPE$`WCUMA_nAny_totalAge_<10`))] <- "Publish"
  
  #WCUMA_n<50 = Indicator of whether sample size of any age and marital group < 50
  ## <50 = Exclude
  ## >=50 = Publish
  tTYPE$`WCUMA_n<50` <- NA
  tTYPE$`WCUMA_n<50`[which(tTYPE$n_unweighted <50)] <- "Exclude"
  tTYPE$`WCUMA_n<50`[which(tTYPE$n_unweighted >=50)] <- "Publish"
  
  #WCUMA_totalDemand<5 = Indicator of whether total demand of any age and marital group < 5
  ## <5 = Exclude
  ## >=5 = Publish
  tTYPE$`WCUMA_totalDemand<5` <- NA
  tTYPE$`WCUMA_totalDemand<5`[which(tTYPE$totalDemand <5)] <- "Exclude"
  tTYPE$`WCUMA_totalDemand<5`[which(tTYPE$totalDemand >=5)] <- "Publish"
  
  Output(tTYPE,tMETH)
}
Transform()





