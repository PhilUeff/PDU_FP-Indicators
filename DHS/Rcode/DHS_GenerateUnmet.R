## R scripts to harmonise family planning variable and estimate family planning indicators by marital status and age from DHS micro-data files
# 1. 'DHS_Translate.R' Translates relevant variables across surveys and stores harmonised variable names and codes as R data sets. Based on IPUMS-DHS (https://www.idhsdata.org/idhs/).
# 2. 'DHS_Categorization.R' Computes marital status and contraceptive use variables
# 3. 'DHS_GenerateUnmet.R' Computes unmet need variable based on DHS code [http://dhsprogram.com/topics/unmet-need.cfm]
# 4. 'DHS_output_FP-Indicators.R' Outputs table of family planning indicators by marital status and age
## Author: United Nations Population Division (Ching Yee Lin, Philipp Ueffing, Stephen Kisambira and Aisha Dasgupta)
## Project: Making family planning count 
# [http://www.un.org/en/development/desa/population/projects/making-family-planning-count/index.shtml]
# [http://www.un.org/en/development/desa/population/theme/family-planning/index.shtml]
## DHS micro data sets need to be downloaded from the DHS program website [https://dhsprogram.com/]



#Calculation of Unmet Need based on DHS general code and DHS code for 24 special surveys

specialSurveys <- c("br31", "kh61", "co60", "ci35", "ga41",
                    "gu34", "gu41" , "ht31" , "ia23" , "ia42", "ia52" , "jo42" , "kk42" ,
                    "ls60", "md21" , "mv50" , "mr42" , "ma43", "np51" , "ni22" , "tz3a" ,
                    "tz41", "tz60" , "tr31" , "tr4a" , "ug33", "ye21") 

Unmet <- function(ir.data){
  ir.data$unmet <- NA
  
  if(SurveyID %in% specialSurveys){
    #Temporary Index for replacement vector ir.data$wantedlast
    index1 <- (is.na(ir.data$unmet) & ir.data$v502!=1 & (ir.data$v020==1 | substr(ir.data$v000,3,3)=="2" | 
                                                           ir.data$v000=="MA4" | ir.data$v000=="TR2" | 
                                                           (ir.data$v000=="CI3" & ir.data$v007==94) | ir.data$v000=="HT3" | 
                                                           ir.data$v000=="IA5" | ir.data$v000=="NP5"))
    
    ir.data$unmet[index1] <- mapvalues(ir.data$unmet[index1], from=NA, to=98)
    rm(index1) #Remove temp index
  }else{
    #Set unmet need to NA for unmarried women if survey only included ever-married women or only collected necessary ir.data for married women
    ir.data$unmet[which(ir.data$v502 != 1 & ir.data$v020 == 1)] <- 98
  }
  
  #########################  GROUP 1: CONTRACEPTIVE USERS #########################
  # Using to limit if wants no more, sterilized or declared infecud
  ## v312 = Current contraceptive method
  ### 0 - Not using, 6 - Female Sterilization, 7 - Male Sterilization, 13 - Lactational Amenorrhea
  ## v605 = Desire for more children
  ### 1 - Wants within 2 years, 5 - Wants no more, 6 - Sterilized, 7 - Declared Infecud
  ir.data$unmet[which(is.na(ir.data$unmet) & ir.data$v312 != 0 & (ir.data$v605 >= 5 & ir.data$v605<=7))] <- 4
  
  #Using to space = All other contraceptive users
  ir.data$unmet[which(is.na(ir.data$unmet) & (ir.data$v312 != 0 | is.na(ir.data$v312)))] <- 3
  
  #########################  GROUP 2: PREGNANT or POSTPARTUM AMENORRHEIC (PPA) WOMEN #########################
  # Generate Time since Last Birth
  ## b3.01 = Date of last birth (CMC)
  ## v008 = Date of Interview (CMC)
  ## DHS April Update: tsinceb now calculated using v222 which is based on century day codes in DHS7
  ir.data$tsinceb <- NA
  ir.data$tsinceb[which(ir.data$v000 < 7)] <- ir.data$v008[which(ir.data$v000 < 7)] - ir.data$b3.01[which(ir.data$v000 < 7)]
  ir.data$tsinceb[which(ir.data$v000 >= 7)] <- ir.data$v222[which(ir.data$v000 >= 7)]
  # ir.data <- ir.data %>% mutate(
  #    tsinceb = ifelse(
  #     v000>=7,
  #      v008-b3.01,
  #      v222
  #    )
  #  )
  
  #Generate Time since Last Period in Months
  ir.data$tsincep <- NA
  ir.data$tsincep[which(ir.data$v215 >= 100 & ir.data$v215 <= 190)] <- (ir.data$v215[which(ir.data$v215 >= 100 & ir.data$v215 <= 190)] - 100) / 30
  ir.data$tsincep[which(ir.data$v215 >= 200 & ir.data$v215 <= 290)] <- (ir.data$v215[which(ir.data$v215 >= 200 & ir.data$v215 <= 290)] - 200) / 4.3
  ir.data$tsincep[which(ir.data$v215 >= 300 & ir.data$v215 <= 390)] <- (ir.data$v215[which(ir.data$v215 >= 300 & ir.data$v215 <= 390)] - 300)
  ir.data$tsincep[which(ir.data$v215 >= 400 & ir.data$v215 <= 490)] <- (ir.data$v215[which(ir.data$v215 >= 400 & ir.data$v215 <= 490)] - 400) * 12
  
  #Initialize Pregnant/Postpartum Amenorrheic women
  ## v213 = Currently Pregnant
  ### 0 - No/Unsure, 1 - Yes
  ## m6.1 = Duration of PPA after Birth of last child in month
  ### 96 = Period Not Returned, 98 Don't know
  ir.data$pregPPA <- NA
  ir.data$pregPPA[which(ir.data$v213 == 1 | ir.data$m6.1 == 96)] <- 1
  
  #For women with 
  ##missing ir.data or "Period not Returned" as date of last menstrual
  ir.data$pregPPA[(is.na(ir.data$m6.1) | ir.data$m6.1==97 | ir.data$m6.1==99) & (ir.data$tsincep > ir.data$tsinceb)
                  & ir.data$tsinceb < 60 & !is.na(ir.data$tsincep) & !is.na(ir.data$tsinceb)] <- 1
  ##"Before Last Birth" as time since last period in the last 5 years
  ir.data$pregPPA[(is.na(ir.data$m6.1) | ir.data$m6.1==97 | ir.data$m6.1==99) & ir.data$v215==995
                  & ir.data$tsinceb < 60 & !is.na(ir.data$tsinceb)] <- 1
  
  #Select only women who are pregnant or PPA <24Months
  ir.data$pregPPA24 <- NA
  ir.data$pregPPA24[(ir.data$v213==1 | (ir.data$pregPPA==1 & ir.data$tsinceb<24))] <- 1
  
  #Classification of wantedness of current / last birth
  ir.data$wantedlast <- NA
  ir.data$wantedlast <- ir.data$v225
  
  ####Special Survey####
  if (SurveyID == "ni22") {
    ir.data$wantedlast[which(ir.data$v000 == 4)] <- 1
  }
  
  #Last Birth 
  ##Temporary index for replacement vector ir.data$wantedlast
  indexWL <- (is.na(ir.data$wantedlast) | ir.data$wantedlast==9) & (ir.data$v213!=1 | is.na(ir.data$v213))
  ir.data$wantedlast[indexWL] <- ir.data$m10.1[indexWL]
  rm(indexWL)
  
  ####Special Survey####
  if (SurveyID %in% c("ci35", "md21", "ni22")) {
    ir.data$wantedlast <- mapvalues(ir.data$wantedlast, from = c(4,8), to = c(2,2))
  }
  
  #No Unmet Need
  ##If wanted current/last birth at the time
  ir.data$unmet[which(is.na(ir.data$unmet) & (ir.data$pregPPA24==1 & ir.data$wantedlast==1))] <- 7
  
  #Unmet Need for Spacing
  ##If wanted current pregnant/ last birth later
  ir.data$unmet[which(is.na(ir.data$unmet) & (ir.data$pregPPA24==1 & ir.data$wantedlast==2))] <- 1
  
  #Unmet Need for Limiting
  ##If wanted current/last birth not at all
  ir.data$unmet[which(is.na(ir.data$unmet) & (ir.data$pregPPA24==1 & ir.data$wantedlast==3))] <- 2
  
  #Missing ir.data on watedness of current/last birth
  ir.data$unmet[which(is.na(ir.data$unmet) & ir.data$pregPPA24==1 & (is.na(ir.data$wantedlast) | ir.data$wantedlast==9))] <- 99
  
  #Determine Sexually Active women in Last 30 Days
  ##No Unmet Need for unmarried women who are not sexually active
  ir.data$sexact <- NA
  
  ####special Survey####
  #Sexually Active Defined as 1 month
  if(SurveyID=="bf21"){
    ir.data$sexact[which(ir.data$v527>=0 & ir.data$v527<300)]<-1
    ir.data$sexact[which(ir.data$v527 > 300)] <- 2
  }else{
    ir.data$sexact[which(ir.data$v528>=0 & ir.data$v528<=30)] <- 1
    if(SurveyID %in% specialSurveys){
      ir.data$sexact[which(is.na(ir.data$sexact) & ir.data$v528==95)] <- 1
    }else{
      ir.data$sexact[which(ir.data$v528 > 30)] <- 2
      ir.data$sexact[which(ir.data$v536 == 0)] <- 3
    }
  }

  #Sexually Active Defined as 3months
  # ir.data$sexact[which((ir.data$v527>=0 & ir.data$v527<=190) |
  #                        (ir.data$v527>=200 & ir.data$v527<=212)|
  #                        (ir.data$v527>=300 & ir.data$v527<=303))] <- 1

  #No Unmet need
  ir.data$unmet[which(is.na(ir.data$unmet) & (ir.data$v502 != 1 | is.na(ir.data$v502)) & (ir.data$sexact != 1 | is.na(ir.data$sexact)) )] <- 97
  
  ########################  GROUP 3: DETERMINE FECUNDITY (Boxes refer to Figure 2 flowchart in report)   ########################
  
  ########### Box 1 - Applicable only to Currently Married Women ###########
  # Married 5+Years ago, No children in Past 5 years, Never used Contraception
  # Excluding pregnant and PPA <24 Months
  ir.data$infec <- NA
  
  if(SurveyID == "kh61"){
    ir.data$infec[which(ir.data$v502==1 & ir.data$v512>=5 & !is.na(ir.data$v512) & 
                          (ir.data$tsinceb>59 | is.na(ir.data$tsinceb)) & ir.data$s313==0  & 
                          (ir.data$pregPPA24!=1 | is.na(ir.data$pregPPA24)) & (ir.data$v007==2010 | ir.data$v007==2011))] <- 1 
  }else if (SurveyID == "tz60"){
    ir.data$infec[which(ir.data$v502==1 & ir.data$v512>=5 & !is.na(ir.data$v512) & 
                          (ir.data$tsinceb>59 | is.na(ir.data$tsinceb)) & ir.data$s309b==0 & 
                          (ir.data$pregPPA24!=1 | is.na(ir.data$pregPPA24)) & (ir.data$v007==2009 | ir.data$v007==2010))] <- 1 
  }else {
    # DHS Update April 2017: checks for v000 now look for "7"
    ir.data$infec[which(!substr(ir.data$v000,3,3) %in% c("6","7") & ir.data$v502==1 & ir.data$v512>=5 & !is.na(ir.data$v512) & 
                          (ir.data$tsinceb>59 | is.na(ir.data$tsinceb)) & ir.data$v302==0 & 
                          (ir.data$pregPPA24!=1 | is.na(ir.data$pregPPA24)))] <- 1
    
    # Pakistan round 6 DHS does still have v302 as name and not v302a (Not specified in DHS code because differently coded)
    if(SurveyID == "pk61") {
      ir.data$v302a <- ir.data$v302
    }
    
    ir.data$infec[which(substr(ir.data$v000,3,3) %in% c("6","7") & 
                          ir.data$v502 == 1 & ir.data$v512>=5 & !is.na(ir.data$v512) & 
                          (ir.data$tsinceb>59 | is.na(ir.data$tsinceb)) & ir.data$v302a==0 &
                          (ir.data$pregPPA24!=1 | is.na(ir.data$pregPPA24)))] <- 1
    
  }
  
  ########### Box 2 ###########
  # Declare Infecud on Future desires for children
  ir.data$infec[which(ir.data$v605==7)] <- 1
  
  ########### Box 3 ###########
  # Menopausal/Hysterectomy as the reason of not using contraception
  
  ####Special Surveys####
  if(SurveyID %in% c("br31","gu34","gu41")){
    ir.data$infec[which(is.na(ir.data$infec) & (ir.data$v375a==23 | ir.data$v375a==28))] <- 1 
  }else if(SurveyID == "ci35"){
    ir.data$infec[which(is.na(ir.data$infec) & ir.data$v007==94 & ir.data$v376==23)] <- 1
  }else if(SurveyID == "ga41"){
    ir.data$infec[which(is.na(ir.data$infec) & ir.data$s607d==1)] <- 1
  }else if(SurveyID == "ht31"){
    ir.data$infec[which(is.na(ir.data$infec) & ir.data$v376==23)] <- 1
  }else if(SurveyID == "jo42"){
    ir.data$infec[which(is.na(ir.data$infec) & (ir.data$v376==23 | ir.data$v376==24))] <- 1
  }else if(SurveyID %in% c("kk42","tz3a")){
    ir.data$infec[which(is.na(ir.data$infec) & (ir.data$v007==99 & ir.data$s607d==1))] <- 1
  }else if(SurveyID == "mv50"){
    ir.data$infec[which(is.na(ir.data$infec) & ir.data$v376==23)] <- 1
  }else if(SurveyID == "mr42"){
    ir.data$infec[which(is.na(ir.data$infec) & ir.data$s607c==1)] <- 1
  }else if(SurveyID == "tr4a"){
    ir.data$infec[which(is.na(ir.data$infec) & ir.data$v375a==23)] <- 1
  }else{
    ##DHS IV+ Surveys
    ir.data$infec[which(substr(ir.data$v000,3,3) %in% c("4","5","6","7") & ir.data$v3a08d==1)] <- 1
    
    ##DHS III Surveys
    ir.data$infec[which(substr(ir.data$v000,3,3) %in% c("3","T") & ir.data$v375a==23)]<-1
    
    ##DHS II Surveys
    ###Reason not using contraception does not exists in DHSII
    ##Use Reason Not Intending to use In Future
    ir.data$infec[which(substr(ir.data$v000,3,3)=="2" & ir.data$v376 == 14)]<-1
  }
  
  ########### Box 4 ########### 
  #Time since Last Period >=6 Months and not PPA
  ir.data$infec[which(ir.data$tsincep>=6 & !is.na(ir.data$tsincep) & 
                        (is.na(ir.data$pregPPA) | ir.data$pregPPA!=1))] <- 1   
  
  ########### Box 5 ########### 
  #Menopausal/Hysterectomy on Time Since Last Period
  if (SurveyID %in% c("tr41", "ug33", "ye21")){
    ir.data$infec[which(is.na(ir.data$infec) & ir.data$v215==993)] <- 1
  }else{
    ir.data$infec[which(ir.data$v215==994)] <- 1
  }
  
  #Never Menstruated on Time Since Last birth, unless had a birth in the last 5 years
  ir.data$infec[which(ir.data$v215==996 & (ir.data$tsinceb>59 | is.na(ir.data$tsinceb)))] <- 1
  
  ########### Box 6 ###########
  # Time Since Last Birth >=60 Months and last period was before last birth
  ir.data$infec[which(ir.data$v215==995 & (ir.data$tsinceb>=60 & !is.na(ir.data$tsinceb)))] <- 1
  
  #Never had Birth, but last period reported as before last birth
  ##Assume code should have been 994/996
  ir.data$infec[which(ir.data$v215==995 & is.na(ir.data$tsinceb))] <- 1
  
  #Exclude Pregnant & PPA<24Months
  ir.data$infec[which(ir.data$pregPPA24==1)]<-NA
  ir.data$unmet[which(is.na(ir.data$unmet)& ir.data$infec==1)] <- 9
  
  ######################## GROUP 4: **FECUND WOMEN ########################
  # Wants within 2 years.
  if(SurveyID == "ia23"){
    ir.data$unmet[which(is.na(ir.data$unmet) & ir.data$v602==6)] <-7
  }else if(SurveyID == "ia42"){
    ir.data$unmet[which(is.na(ir.data$unmet) & ir.data$v605==9)] <-7
  }else{
    ir.data$unmet[which(is.na(ir.data$unmet) & ir.data$v605==1)]<-7
  }
  # wants in 2+years, wants undecided timing, or unsure if wants
  if(SurveyID == "ls60"){
    ir.data$v605[which(is.na(ir.data$v605))] <- 4
  }
  ir.data$unmet[which(is.na(ir.data$unmet) & ir.data$v605>=2 & ir.data$v605<=4 )] <- 1
  # wants no more
  ir.data$unmet[which(is.na(ir.data$unmet) & ir.data$v605==5)] <-2
  ir.data$unmet[which(is.na(ir.data$unmet))] <-99
  
  ####Special Surveys####
  if(SurveyID == "tr4a"){
    ir.data <- subset(ir.data, ir.data$v001 %%2 == ir.data$v002 %%2)
  }
  
  #Total Unmet Need
  ir.data$specific_unmet <- ir.data$unmet
  ir.data$specific_unmet[which(!ir.data$specific_unmet%in%c(1,2))] <- 0
  
  ir.data$unmettot <- NA
  ir.data$unmettot[which(ir.data$unmet==1 | ir.data$unmet==2)]<-1
  ir.data$unmettot[which(!ir.data$unmet %in% c(1,2))] <- 0
  
  # Kenya 2014 is not included in DHS list of special surveys, but has long and short questionnaire.
  # Only short questionnaire included questions on fertility preferences. Therefore limit universe.
  # We use desire for children variable and exclude missings from universe
  
  if(SurveyID=="ke70") {
    for(i in c("unmet","unmettot","specific_unmet")){
      ir.data[,i][which(is.na(ir.data$v605))]<-NA
    }
  }
  
  return (ir.data)
}