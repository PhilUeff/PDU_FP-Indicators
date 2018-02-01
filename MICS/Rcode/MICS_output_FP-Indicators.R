## R scripts to harmonise family planning variable and estimate family planning indicators by marital status and age from MICS micro-data files
# 1. 'MICS_Translate.R' Translates relevant variables across surveys and stores harmonised variable names and codes as R data sets
# 2. 'MICS_Categorization.R' Computes marital status and contraceptive use variables
# 3. 'MICS_GenerateUnmet.R' Computes unmet need variable based on DHS code [http://dhsprogram.com/topics/unmet-need.cfm]
# 4. 'MICS_output_FP-Indicators.R' Outputs table of family planning indicators by marital status and age
## Author: United Nations Population Division (Ching Yee Lin, Philipp Ueffing, Stephen Kisambira and Aisha Dasgupta)
## Project: Making family planning count 
# [http://www.un.org/en/development/desa/population/projects/making-family-planning-count/index.shtml]
# [http://www.un.org/en/development/desa/population/theme/family-planning/index.shtml]
## MICS micro data sets need to be downloaded from the MICS program website [http://mics.unicef.org/]



##Calculate Unmet need and CP estimates by marital status and 5-year age group

setwd("V:/FertilitySection/Alkema_Joint project on contraceptive use trends/1_All-women-estimates&projections/Data-tabulations/MICS/")
#Translate any new surveys
source("./RCode/MICS_Translate.R")
#############LIBRARIES##############
rm(list = ls())

library(plyr) #MapValues
library(dplyr) #Output Tables Formatting
library(reshape2) 
library(tidyr)
library(tools) #File Extension Cleaning
####################################
#####DIRECTORIES AND FILE LISTS#####
setwd("V:/FertilitySection/Alkema_Joint project on contraceptive use trends/1_All-women-estimates&projections/Data-tabulations/MICS")

mics.main.list <- read.csv("V:/MICS/MICSMaster.csv", header=TRUE, stringsAsFactors = FALSE, na.strings = c("..", "NA", "", " "))

##Master List of all the information for surveys
# subset the inventory list to keep only the information for the survey(s) you want to run
mics.list <- subset(mics.main.list, Phase=="MICS2" | Phase=="MICS3" | Phase=="MICS4" | Phase=="MICS5") 
mics.list <- subset(x=mics.list, subset = !is.na(Individual.Recode))
# Exclude problematic countries and countries without survey.code:
problemSurveys <- c("mcwm3")   ###Former Yugoslav Republic of macedonia 2006, Problem with CP Variable
mics.list <-  subset(mics.list, !Individual.Recode %in% problemSurveys)

##List of translated surveys
translated.list <- file_path_sans_ext(dir("Translated_RDataFiles",pattern=".RData$"))
#working list only retained non-problematic surveys, and exists in master file (Loop through these to calculate)
working.list <- translated.list[translated.list %in% mics.list$Individual.Recode]
####################################
########FILES OUTPUT SETTING########
##Set Names and move existing files to prior folder
FileSetting <- function(){
  filename <- "MICS_CPTYPE_by_MARSTAT_WIDE" # Modern and traditional CP
  filename1 <- "MICS_CPTYPE_by_MARSTAT_LONG"
  filename2 <- "MICS_CPMETHOD_by_MARSTAT_LONG" # Method specific CP
  filename3 <- "MICS_CPMETHOD_by_MARSTAT_WIDE"
  
  file.list <- c(filename,filename1,filename2,filename3)
  
  for (f in 1:length(file.list)){
    if(file.exists(paste0(file.list[f],".csv"))){
      #Remove completely if it is empty
      if(file.size(paste0(file.list[f],".csv"))==0){
        file.remove(paste0(file.list[f],".csv"))
      }else{
        # Move to Prior folder if it is not empty
        ## Added date to prevent overwrite of older files
        ## Will overwrite files produced on the same day
        file.rename(paste0(file.list[f],".csv"), paste0("Prior/",file.list[f], "_", substr(file.info(paste0(file.list[f],".csv"))$ctime,1,10),".csv"))
      }
    }
    file.list[f]<-paste0(file.list[f],".csv")
  }
  
  # create output file
  file.create(file.list)
  
  return(file.list)
}
####################################

#######Calculation Functions########
##Cross Tabulations to create output tables (Call Within CP_OUTPUT)
CrossTab <- function(VarWeight,VarMarital, VarMethod,df,Formula,VarList){
  AllWomen <- sum

  if(Formula == T){
    if(VarWeight==T){
      ##CP and Unmet Table
      tab_All <- as.data.frame(addmargins(xtabs(wmweight ~ get(VarMarital) + get(VarMethod), df, exclude=NULL), 1, FUN = AllWomen)) %>%
        dcast(get.VarMarital. ~ get.VarMethod., value.var = "Freq") %>%
        mutate(AGE5YEAR_LAB="[Total]")
      
      tab_Age <- as.data.frame(addmargins(xtabs(wmweight ~ get(VarMarital) + get(VarMethod) + AGE5YEAR_LAB,df,exclude=NULL),1,FUN=AllWomen))%>%
        dcast(get.VarMarital. + AGE5YEAR_LAB ~ get.VarMethod., value.var = "Freq")

    }else{
      tab_All <- as.data.frame(addmargins(xtabs(~get(VarMarital)+get(VarMethod),df,exclude=NULL),1,FUN=AllWomen)) %>%
        dcast(get.VarMarital.~get.VarMethod., value.var = "Freq") %>%
        mutate(AGE5YEAR_LAB = "[Total]")
      tab_Age <- as.data.frame(addmargins(xtabs(~get(VarMarital)+get(VarMethod) + AGE5YEAR_LAB,df,exclude=NULL),1,FUN=AllWomen)) %>%
        dcast(get.VarMarital.+AGE5YEAR_LAB~get.VarMethod., value.var = "Freq")
    }
  }else if(Formula==F){
    if(VarWeight==T){
      tab_All <- as.data.frame(xtabs(wmweight ~ get(VarMarital) + get(VarMethod), df,exclude=NULL)) %>%
        dcast(get.VarMarital. ~ get.VarMethod., value.var = "Freq") %>%
        mutate(AGE5YEAR_LAB="[Total]")
      
      tab_Age <- as.data.frame(xtabs(wmweight ~ get(VarMarital) + get(VarMethod) + AGE5YEAR_LAB,df,exclude=NULL))%>%
        dcast(get.VarMarital. + AGE5YEAR_LAB ~ get.VarMethod., value.var = "Freq")
    }else {
      tab_All <- as.data.frame(xtabs(~get(VarMarital)+get(VarMethod),df,exclude=NULL)) %>%
        dcast(get.VarMarital.~get.VarMethod., value.var = "Freq") %>%
        mutate(AGE5YEAR_LAB = "[Total]")
      tab_Age <- as.data.frame(xtabs(~get(VarMarital)+get(VarMethod) + AGE5YEAR_LAB,df,exclude=NULL)) %>%
        dcast(get.VarMarital.+AGE5YEAR_LAB~get.VarMethod., value.var = "Freq")
    }
  }
  
  names(tab_All)[which(names(tab_All)=="get.VarMarital.")] <- "MSTAT_LAB"
  names(tab_Age)[which(names(tab_Age)=="get.VarMarital.")] <- "MSTAT_LAB"
  
  tab <- full_join(tab_All,tab_Age)
  if("NA" %in% names(tab)){
    tab<-within(tab, rm("NA"))
  }
  if(any(is.na(tab))){
    #Only retain complete records (of the crosstab)
    tab <- tab[!is.na(tab$MSTAT_LAB)&!is.na(tab$AGE5YEAR_LAB),]
  }
  
  #Set (method users/unmet need estimate) as 0 if it does not exists in crosstab
  if(any(VarList !=F)){
    for(var in 1:length(VarList)){
      if(VarList[var]%in%colnames(tab) == F){
        tab[,VarList[var]] <- 0 
      }
    }
  }
  return(tab)
}
##Generate output table
CP_OUTPUT <- function(df,choice,VarMethods){
  cpVarList <- c("Using_modern","Using_traditional","Not_using","Using_any_nomethod")
  umnVarList <- c("No_Unmet_Need","Unmet_Need","MissingData")
  if(choice == "Both"){
    if(!"AGE5YEAR_LAB"%in%names(df)){
      df$AGE5YEAR_LAB <- "[Total]"
    }
    #Only MSTAT - Married/Unmarried/AllWomen
    cp_MW <- CP_OUTPUT(df,"General",VarMethods)
    #Only MSTATNF - Formerly/Never married
    cp_UMW <- CP_OUTPUT(df,"Unmarried",VarMethods)
    
    if(!is.null(cp_UMW)){
      final <- rbind(cp_MW,cp_UMW)
    }else{
      final <- cp_MW
    }
    
    #Set ever married sample to NA for formerly,never and unmarried 
    final <- ResetRecords(final)
    
    tTYPE <- select(final, country:Endyear,Universe, MSTAT_LAB, AGE5YEAR_LAB, 
                    Not_using, Using_any, Using_modern, Using_traditional, Unmet_Need, 
                    cpModern:`Demand satisfied by modern`, survey:surveyShort,n_unweighted,nAny_unweighted)
    return(tTYPE)
    
  }else{
    if(choice == "General"){
      if(all(is.na(df$MSTAT_LAB))==T){
        #No variable on marital status - Calculate for All Women
        df$MSTAT_LAB <- "AllWomen"
        
        cp<-CrossTab(T,"MSTAT_LAB",VarMethods,df,F,cpVarList)
        samplesize <- CrossTab(F,"MSTAT_LAB",VarMethods,df,F,cpVarList)
      }else{
        cp<-CrossTab(T,"MSTAT_LAB",VarMethods,df,T,cpVarList)
        samplesize <- CrossTab(F,"MSTAT_LAB",VarMethods,df,T,cpVarList)
      }
      if(all(is.na(df$UnmetSim))==F & all(df$UnmetSim=="MissingData")==F){
        umn<-CrossTab(T,"MSTAT_LAB","UnmetSim",df,T,umnVarList)
      }
    }else if (choice == "Unmarried"){
      if(all(is.na(df$MSTATNF_LAB))==F & all(is.na(df[which(!is.na(df$MSTATNF_LAB)),VarMethods]))==F){
        cp<-CrossTab(T,"MSTATNF_LAB",VarMethods,df,F,cpVarList)
        samplesize<-CrossTab(F,"MSTATNF_LAB",VarMethods,df,F,cpVarList)
        
        if(all(df$UnmetSim=="MissingData")==F){
          umn<-CrossTab(T,"MSTATNF_LAB","UnmetSim",df,F,umnVarList)
        }
      }else{
        return (NULL)
      }
    }
    
    #Calculate sample size by marital/agegroup 
    ##For decision of exclusion for WCUMA
    samplesize$n_unweighted <- NA
    samplesize$nAny_unweighted <- NA
    
    samplesize$n_unweighted <- rowSums(samplesize[,c("Using_modern","Using_traditional","Not_using")])
    samplesize$nAny_unweighted <- rowSums(samplesize[,c("Using_modern","Using_traditional")])
    
    samplesize <- samplesize[,c("MSTAT_LAB","AGE5YEAR_LAB","n_unweighted","nAny_unweighted")]
    
    
    if(exists("umn")){
      final <- join(cp,samplesize)
      final <- full_join(final,umn,
                         by=c("MSTAT_LAB","AGE5YEAR_LAB"))
    }else{
      final <- join(cp,samplesize) %>%
        arrange(MSTAT_LAB,AGE5YEAR_LAB)
      final$Unmet_Need <- NA
      final$No_Unmet_Need <- 0
      final$MissingData <- 0
    }
    
    if(VarMethods == "FPANYspecial_LAB"){
      if(any(df$FPANYspecial_LAB[which(df$MSTAT_LAB=="Unmarried/Not-in-union")]=="Using_any") == F){
        final$Using_any <- NULL
        final$Using_any <- 0
      }
      final <- final %>%
        mutate(Using_modern = indicator,
               Using_traditional = indicator,
               cpModern = indicator,
               cpTraditional = indicator,
               cpAny = (Using_any) / (Not_using + Using_any) * 100,
               Unmet = (Unmet_Need) / (Unmet_Need + No_Unmet_Need)*100,
               `Demand satisfied by modern` = NA,
               country = surveyInfo$CountryName.UN,
               iso = surveyInfo$LocID,
               catalogID = surveyInfo$CatalogID,
               Startyear = surveyInfo$StartYear,
               Endyear = surveyInfo$EndYear,
               survey = surveyInfo$SurveyName,
               surveyShort = surveyInfo$ShortName,
               Universe = surveyInfo$Sample.Type.Female
               )
    }else{
      final <- final %>%
        mutate(Using_any = Using_modern + Using_traditional + Using_any_nomethod,
               cpModern = Using_modern / (Not_using + Using_modern + Using_traditional + Using_any_nomethod)*100,
               cpTraditional = Using_traditional / (Not_using + Using_modern + Using_traditional + Using_any_nomethod) *100,
               cpAny = (Using_modern + Using_traditional) / (Not_using + Using_modern + Using_traditional + Using_any_nomethod) *100,
               Unmet = Unmet_Need/(Unmet_Need + No_Unmet_Need)*100,
               `Demand satisfied by modern` = ifelse(!is.na(Unmet),((Using_modern)/(Using_any_nomethod+Using_modern+Using_traditional+Unmet_Need)) *100,NA),
               country = surveyInfo$CountryName.UN,
               iso = surveyInfo$LocID,
               catalogID = surveyInfo$CatalogID,
               Startyear = surveyInfo$StartYear,
               Endyear = surveyInfo$EndYear,
               survey = surveyInfo$SurveyName,
               surveyShort = surveyInfo$ShortName,
               Universe = surveyInfo$Sample.Type.Female
        )
    }
    
    #Only include Married when survey's Universe is Ever Married
    if(final$Universe == "Ever married" && !"Never married"%in%df$MSTATNF_LAB){
      final <- subset(final,subset=final$MSTAT_LAB!="Unmarried/Not-in-union")
    }
    tTYPE <- select(final, country:Endyear,Universe, MSTAT_LAB, AGE5YEAR_LAB, n_unweighted, nAny_unweighted, Not_using, Using_any, Using_modern, Using_traditional,Unmet_Need, cpModern:`Demand satisfied by modern`, survey:surveyShort)
  }
  return (final)
}


CP_METH <- function (df,choice){
  AllWomen <- sum
  if(choice=="Both"){
    if(!"AGE5YEAR_LAB"%in%names(df)){
      df$AGE5YEAR_LAB <- "[Total]"
    }
    cpMeth_general <- CP_METH(df,"General")
    cpMeth_NF <- CP_METH(df,"Unmarried")
    if(!is.null(cpMeth_NF)){
      cpMeth<-rbind(cpMeth_general,cpMeth_NF)
    }else{
      cpMeth<-cpMeth_general
    }
    return(cpMeth)
  }
  else{
    if(choice == "General"){
      if(all(is.na(df$MSTAT_LAB))){
        df$MSTAT_LAB<-"AllWomen"
        cpMethAll <- as.data.frame(xtabs(wmweight ~ MSTAT_LAB + FPMETHOD_LAB, df)) %>%
          mutate(AGE5YEAR_LAB = "[Total]")
        cpMethAge <- as.data.frame(xtabs(wmweight ~ MSTAT_LAB + FPMETHOD_LAB + AGE5YEAR_LAB, df))
        
        if("Specific_unmet"%in%names(df)){
          umnAll <- as.data.frame(xtabs(wmweight~MSTAT_LAB + Specific_unmet, df))%>%
            mutate(AGE5YEAR_LAB = "[Total]")
          
          umnAge <- as.data.frame(xtabs(wmweight~MSTAT_LAB + Specific_unmet + AGE5YEAR_LAB,df)) 
        }
      }else{
        cpMethAll <- as.data.frame(addmargins(xtabs(wmweight ~ MSTAT_LAB + FPMETHOD_LAB, df), 1, FUN = AllWomen)) %>%
          mutate(AGE5YEAR_LAB = "[Total]")
        
        cpMethAge <- as.data.frame(addmargins(xtabs(wmweight ~ MSTAT_LAB + FPMETHOD_LAB + AGE5YEAR_LAB, df), 1, FUN = AllWomen))
        
        if("Specific_unmet"%in%names(df)){
          umnAll <- as.data.frame(addmargins(xtabs(wmweight~MSTAT_LAB + Specific_unmet, df),1,FUN=AllWomen))%>%
            mutate(AGE5YEAR_LAB = "[Total]")
          
          umnAge <- as.data.frame(addmargins(xtabs(wmweight~MSTAT_LAB + Specific_unmet + AGE5YEAR_LAB,df),1,FUN=AllWomen) )
        }
      }
    }else if(choice == "Unmarried"){
      if(all(is.na(df$MSTATNF_LAB))==F){
        cpMethAll <- as.data.frame(xtabs(wmweight ~ MSTATNF_LAB + FPMETHOD_LAB, df)) %>%
          mutate(AGE5YEAR_LAB = "[Total]")
        
        cpMethAge <- as.data.frame(xtabs(wmweight ~ MSTATNF_LAB + FPMETHOD_LAB + AGE5YEAR_LAB, df))
        
        if("Specific_unmet"%in%names(df)){
          umnAll <- as.data.frame(xtabs(wmweight~MSTATNF_LAB + Specific_unmet, df))%>%
            mutate(AGE5YEAR_LAB = "[Total]")
          
          umnAge <- as.data.frame(xtabs(wmweight~MSTATNF_LAB + Specific_unmet + AGE5YEAR_LAB,df)) 
        }
        
      }else{
        return (NULL)
      }
    }
    
    cpMeth <- full_join(cpMethAge, cpMethAll)
    
    if(exists("umnAll")){
      umn <- full_join(umnAll,umnAge)
      names(umn)[which(names(umn)=="Specific_unmet")] <- "FPMETHOD_LAB"
      cpMeth <- full_join(cpMeth,umn)
    }
    
    if("MSTATNF_LAB"%in%colnames(cpMeth)){
      colnames(cpMeth)[colnames(cpMeth)=="MSTATNF_LAB"]<-"MSTAT_LAB"
    }
    cpMeth <- cpMeth %>% 
      arrange(MSTAT_LAB,AGE5YEAR_LAB) %>%
      mutate(country = surveyInfo$CountryName.UN,
             iso = surveyInfo$LocID,
             catalogID = surveyInfo$CatalogID,
             Startyear = surveyInfo$StartYear,
             Endyear = surveyInfo$EndYear,
             survey = surveyInfo$SurveyName,
             surveyShort = surveyInfo$ShortName
      )
  }
  return(cpMeth)
}
####################################

######Output Cleaning Functions#####
##Set records to NA for missing data
ResetRecords<- function(final){
  if(surveyID != "thwm3"){
    mstatus.list <- c("Never married","Formerly in union")
    mstat.list<-c("Never married","Formerly married")
    
    for(m in 1:length(mstatus.list)){
      if(all(is.na(df$MSTATNF_LAB))==F & all(is.na(df[which(!is.na(df$MSTATNF_LAB)),VarMethods]))==F){
        if(any(df$MARSTAT_LAB==mstatus.list[m]) & !is.na(any(df$MARSTAT_LAB == mstatus.list[m]))){
          if(all(is.na(df$FPNOW[which(df$MARSTAT_LAB==mstatus.list[m])])) & mstat.list[m] %in% final$MSTAT_LAB){
            if(final$Using_any[which(final$MSTAT_LAB == mstat.list[m] & final$AGE5YEAR_LAB == "[Total]")] ==0){
              #Subcategories of Unmarried has 0 users - Set unmarried, allwomen and that category CPs as NA
              final$cpAny[which(final$MSTAT_LAB == mstat.list[m] | final$MSTAT_LAB == "AllWomen"| final$MSTAT_LAB == "Unmarried/Not-in-union")] <- NA
              final$cpModern[which(final$MSTAT_LAB == mstat.list[m] | final$MSTAT_LAB == "AllWomen"| final$MSTAT_LAB == "Unmarried/Not-in-union")] <- NA
              final$cpTraditional[which(final$MSTAT_LAB == mstat.list[m] | final$MSTAT_LAB == "AllWomen"| final$MSTAT_LAB == "Unmarried/Not-in-union")] <- NA
              final$`Demand satisfied by modern`[which(final$MSTAT_LAB == mstat.list[m] | final$MSTAT_LAB == "AllWomen"| final$MSTAT_LAB == "Unmarried/Not-in-union")] <- NA
              final$Unmet[which(final$MSTAT_LAB == mstat.list[m] | final$MSTAT_LAB == "AllWomen"| final$MSTAT_LAB == "Unmarried/Not-in-union")] <- NA
            }
          }
          if(all(df$UnmetSim[which(df$MARSTAT_LAB==mstatus.list[m])]  == "MissingData")){
            final$Unmet[which(final$MSTAT_LAB == mstat.list[m] | final$MSTAT_LAB == "AllWomen"| final$MSTAT_LAB == "Unmarried/Not-in-union")] <- NA
          }
        }

      }
    }
  }
  
  #Set Unmet and Demand Satisfied of Unmarried and Allwomen when no unmet estimate can be calculated 
  if(all(df$UnmetSim[which(df$MSTAT_LAB=="Unmarried/Not-in-union")] == "MissingData")){
    final$Unmet[which(final$MSTAT_LAB=="Unmarried/Not-in-union")] <- NA
    final$Unmet[which(final$MSTAT_LAB=="AllWomen")] <- NA
    final$`Demand satisfied by modern`[which(final$MSTAT_LAB == "Unmarried/Not-in-union")] <- NA
    final$`Demand satisfied by modern`[which(final$MSTAT_LAB=="AllWomen")] <- NA
  }
  if(all(df$UnmetSim[which(df$MSTAT_LAB=="Married/In-union")]=="MissingData")){
    final$Unmet[which(final$MSTAT_LAB=="Married/In-union")] <- NA
    final$Unmet[which(final$MSTAT_LAB=="AllWomen")] <- NA
    final$`Demand satisfied by modern`[which(final$MSTAT_LAB=="Married/In-union")] <- NA
    final$`Demand satisfied by modern`[which(final$MSTAT_LAB=="AllWomen")] <- NA
  }
  
  return (final)
}
Output <- function(tTYPE,tMETH){
  #Write table for tTYPE
  if (file.size(file.list[1])== 0){
    # if the csv output file is empty append the computed values to the output file but output the column names first, that is,in the first row
    write.csv(tTYPE, file = file.list[1], quote = TRUE, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    # if the csv output file already has observations in it append the results to the output file without displaying column names each time data is outputted
    write.table(tTYPE, file = file.list[1], quote = TRUE, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  }
  
  #Output Table for tMETH
  if (file.size(file.list[3]) == 0){
    # if the csv output file is empty append the computed values to the output file but output the column names first, that is,in the first row
    write.table(tMETH, file = file.list[3], quote = TRUE, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    # if the csv output file already has observations in it append the results to the output file without displaying column names each time data is outputted
    write.table(tMETH, file = file.list[3], quote = TRUE, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  }
}
TransformFormat <- function(){
  #TRANSFORM TYPE TABLE FROM WIDE FORMAT TO LONG FORMAT AND CALCULATE CP
  long <- read.csv(file = file.list[1]) %>%
    gather(Indicator, prevalence, cpModern:`Demand.satisfied.by.modern`)  #cp in long format
  
  write.csv(long, file.list[2])
  
  #TRANSFORM METHOD TABLE FROM LONG INTO WIDE FORMAT AND CALCULATE CP
  tLong <- read.csv(file = file.list[3])
  
  colList <- c("IMP", "MST", "IUD_IUS", "FST", "INJ", "INJ1", "INJ2", "INJ3", "PILL", "PAT", "RING", "CONM", "DIA", "CONF", "LAM", "EC", "NSP",
               "FOA", "OTHMOD", "RHY", "WD", "BF","SDM","OTHTRAD", "OTH","FPAny_butNotInFPMETHOD","UnmetNeed_for_Limiting","UnmetNeed_for_Spacing",
               "No_Unmet_Need","modernUser", "traditionalUser", "totalUser", "NotUsing")
  
  ## Cast into wide format
  tWide <- tLong %>%
    filter(MSTAT_LAB %in% c("Married/In-union", "Unmarried/Not-in-union", "AllWomen", "Formerly in-union", "Neverin-union", "Unmarried")) %>%  #filters out surveys from tLong which did not have method specific variable or other problems
    dcast(catalogID + iso + survey + surveyShort + country + Startyear + Endyear + MSTAT_LAB + AGE5YEAR_LAB ~ FPMETHOD_LAB, value.var = "Freq")
  
  # Assign NA values for methods that were not present in any MICS surveys included in run
  for (j in colList[!colList %in% names(tWide)]) {
    tWide[j] <- NA
  }
  
  # Order columns
  tWide <- tWide[c(names(tWide)[!names(tWide) %in% colList], colList)]
  
  # Sum individual methods to modern and traditional users
  tWide <- tWide %>%
    mutate(modernUser = rowSums(cbind(IMP, MST, IUD_IUS, FST, INJ, INJ1, INJ2, INJ3, PILL, PAT, RING, CONM, DIA, CONF, LAM, EC, FOA, OTHMOD), na.rm = TRUE), 
           traditionalUser = rowSums(cbind(RHY, WD, BF, NSP,SDM ,OTHTRAD, OTH), na.rm = TRUE), # category FPAny_butNotInFPMETHOD not categorised yet,
           totalUser = modernUser + traditionalUser,
           totalN = rowSums(cbind(modernUser, traditionalUser, NotUsing, FPAny_butNotInFPMETHOD), na.rm = TRUE))
  
  # Add CP percentages for selection of methods
  for (j in colList) {
    colTitle <- paste("CP", j, sep = "_")
    if(j %in% c("UnmetNeed_for_Limiting","UnmetNeed_for_Spacing","No_Unmet_Need")){
      tWide[colTitle] <- tWide[j] / rowSums(tWide[,c("UnmetNeed_for_Limiting","UnmetNeed_for_Spacing","No_Unmet_Need")],na.rm=T) *100
    }
    tWide[colTitle] <- tWide[j] / rowSums(tWide[, c("modernUser", "traditionalUser", "NotUsing", "FPAny_butNotInFPMETHOD")], na.rm = TRUE) * 100
  }
  
  # Write out table as csv
  write.table(tWide, file = file.list[4], quote = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
}
####################################

#########RUNNING WITH FILES#########
choice <- "Both"

#Load Categorize & unmet calculation functions from other scripts
source("./RCode/MICS_Categorization.R")
source("./RCode/MICS_GenerateUnmet.R")

file.list<-FileSetting()

for(i in 1:length(working.list)){
  surveyID <- working.list[i]
  print(surveyID)
  
  surveyInfo <- subset(mics.list, Individual.Recode == surveyID)
  
  load(paste("Translated_RDataFiles/", paste0(surveyID, ".RData"),sep="/"))
  
  ##Restrict Sexually Active Sample
  # if("TIMESINCESEX_Unit"%in%names(df)){
  #   #Sexually Active defined as 1 month
  #   df$Sex<-0
  #   df$Sex[which((df$TIMESINCESEX_Unit==1 & as.numeric(df$TIMESINCESEX_Numeric)<=90) |
  #                  (df$TIMESINCESEX_Unit==2 & as.numeric(df$TIMESINCESEX_Numeric)<=12) |
  #                  (df$TIMESINCESEX_Unit==3 & as.numeric(df$TIMESINCESEX_Numeric)<=3))] <- 1
  # }else{
  #   next()
  # }
  # 
  # if(all(df$Sex==0)){
  #   next()
  # }else{
  #   df <- subset(df,subset=df$Sex==1)
  # }
  
  #Only calculate women age 15-49
  if("[50-54]"%in%df$AGE5YEAR_LAB){
    df[which(df$AGE5YEAR_LAB == "[50-54]"),] <- NA
  }

  #Check for Contraceptive Variable 
  fpVarList<-c("FPNOWUSFST", "FPNOWUSMST", "FPNOWUSIUD", "FPNOWUSIUS", "FPNOWUSINJ", "FPNOWUSIMP", "FPNOWUSPILL", "FPNOWUSCONM", "FPNOWUSCONF",
               "FPNOWUSDIA", "FPNOWUSFOA", "FPNOWUSLAM", "FPNOWUSEC", "FPNOWUSPAT", "FPNOWUSRING", "FPNOWUSRHY", "FPNOWUSWD", "FPNOWUSBF", "FPNOWUSOTH",
               "FPNOWUSNSP", "FPNOWUSOTHMOD", "FPNOWUSOTHTRAD","FPNOWUSSDM")
  
  ##FPNOW does not exists and specificMethods missing -> output FP variables Missing
  if ((is.null(df$FPNOW) | all(is.na(df$FPNOW))) & all(!fpVarList %in% names(df))){
    indicator <- "FP variables missing"
    tTYPE <- paste(surveyID, surveyInfo$CountryName.UN, surveyInfo$StartYear, "FP variables missing", sep = "; ")
  }else{
    if (all(!fpVarList %in% names(df)) & !"FPMETHNOW"%in%names(df)){
      indicator <- "Method specific variables missing"
      ###Can compute CPANY 
      df$FPANYspecial <- mapvalues(df$FPNOW, from = c(1 , 2, 9, NA), to = c(1, 2, 2, 2), warn_missing = F)
      df$FPANYspecial_LAB <- mapvalues(df$FPANYspecial, from = c(1 , 2), to = c("Using_any", "Not_using"), warn_missing = F)
      
      df<-CalcUnmet(df)
      df<-MaritalStatusCategories(df,choice)
      
      VarMethods <- "FPANYspecial_LAB"
      tTYPE <- CP_OUTPUT(df,choice,VarMethods)
      
    }else{
      df<-MaritalStatusCategories(df,choice)
      df<-CalcUnmet(df)
      
      df<-MethodCategories(df)
      
      VarMethods <- "FPTYPE_LAB"
      tTYPE <-CP_OUTPUT(df,choice,VarMethods)
      cpMeth<-CP_METH(df,choice)
    }

    tTYPE$totalDemand <- rowSums(tTYPE[,c("cpAny","Unmet")],na.rm=T)
    
    #WCUMA_nAny_totalAge_<10 = Indicator of whether sample size of unmarried for 15-49 < 10
    ## Missing FP NOW Variable, WMCUMA indicator remains empty
    ## <10 = Exclude
    ## >=10 = Publish
    tTYPE$`WCUMA_nAny_totalAge_<10` <- NA
    
    if(any(tTYPE$Universe == "Ever married" | !"Unmarried/Not-in-union" %in% tTYPE$MSTAT_LAB)){
      if(any(tTYPE$nAny_unweighted[which(tTYPE$MSTAT_LAB == "Formerly married" & tTYPE$AGE5YEAR_LAB == "[Total]")]<10)){
        tTYPE$`WCUMA_nAny_totalAge_<10`[which(tTYPE$MSTAT_LAB=="Formerly married" | tTYPE$MSTAT_LAB=="Never marred" |tTYPE$MSTAT_LAB=="AllWomen")] <- "Exclude"
      }
    }else if(any(tTYPE$nAny_unweighted[which(tTYPE$MSTAT_LAB=="Unmarried/Not-in-union" & tTYPE$AGE5YEAR_LAB=="[Total]")] < 10)){
      tTYPE$`WCUMA_nAny_totalAge_<10`[which(tTYPE$MSTAT_LAB == "Unmarried/Not-in-union" | tTYPE$MSTAT_LAB == "Formerly married" | tTYPE$MSTAT_LAB == "Never married" | tTYPE$MSTAT_LAB=="AllWomen")] <- "Exclude"
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
  }
  
  if(exists('cpMeth')){
    tMETH <- cpMeth
  }else{
    tMETH <- paste(surveyID, surveyInfo$CountryName.UN,surveyInfo$StartYear,indicator, sep="; ")
  }
  
  Output(tTYPE,tMETH)
  rm(cpMeth)
}
TransformFormat()


