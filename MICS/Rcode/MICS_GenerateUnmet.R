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



# Unmet need computation
CalcUnmet<- function(df){
  UnmetVars <- c("TIMEMENSTRUATE_Unit","TIMEMENSTRUATE_Numeric","PGDESIRE_NOW","PGDESIRE_LATER","FUTCHLD_NPREG"
                 ,"FPLCHDESIRE_NOW","FPLCHDESIRE_LATER","PREGNANT","FPNOW")
  df$UnmetSim <- NA
  
  if(all(UnmetVars%in% names(df))==T){
    #Unmet: 1 = unmetneed for spacing
    #       2 = unmet need for limiting
    #       3 = using to space
    #       4 = using to limit
    #       7 = no unmet need
    #       9 = infecund/menopausal
    #       97= not sexually active
    #       98= unmarried sample/ no data
    #       99= unmet need missing
    df$Unmet <- NA
    
    
    #Group 1 - on Contraceptive Use
    ##Criterias: i) Using some kind of methods
    ##              a) FPNow: 1 = Current using
    ##                        2 = Not using
    ##          ii) Want no more children
    df$UsingCP<-NA
    df$UsingCP[which(df$FPNOW==1)] <- 1
    
    df$Unmet[which(df$UsingCP==1 )] <- 3
    df$Unmet[which(df$UsingCP==1 & (df$FPNOWUSFST==1 | df$FPNOWUSMST==1 | df$FUTCHLD_NPREG==2 |df$FUTCHLD_NPREG==3 | df$FUTCHLD_Numeric==94 ))] <-4
    
    ###############CHECK MONTHSINCEMENSTRUATE############
    #Postpartum amenorrheic
    ##In this group only if: i) Not Pregnant
    ##                      ii) Period did not return since last birth in last 5years
    df$MonthSinceLastBirth <-NA
    df$MonthSinceLastBirth <- as.numeric(df$INTDATECMC) - as.numeric(df$KIDDOBCMC)
    
    df$TimeLastPeriod_Num <- NA
    df$TimeLastPeriod_Num[which( as.numeric(df$TIMEMENSTRUATE_Numeric)>=0 & as.numeric(df$TIMEMENSTRUATE_Numeric<=90))] <- 1
    
    df$MonthSinceMenstruate<-NA
    df$MonthSinceMenstruate[which(df$TIMEMENSTRUATE_Unit==1 & df$TimeLastPeriod_Num==1)]<-
      as.numeric(df$TIMEMENSTRUATE_Numeric[which(df$TIMEMENSTRUATE_Unit==1 & df$TimeLastPeriod_Num==1)])/30
    df$MonthSinceMenstruate[which(df$TIMEMENSTRUATE_Unit==2 & df$TimeLastPeriod_Num==1)]<-
      as.numeric(df$TIMEMENSTRUATE_Numeric[which(df$TIMEMENSTRUATE_Unit==2 & df$TimeLastPeriod_Num==1)])/4.3
    df$MonthSinceMenstruate[which(df$TIMEMENSTRUATE_Unit==3 & df$TimeLastPeriod_Num==1)]<-
      as.numeric(df$TIMEMENSTRUATE_Numeric[which(df$TIMEMENSTRUATE_Unit==3 & df$TimeLastPeriod_Num==1)])
    df$MonthSinceMenstruate[which(df$TIMEMENSTRUATE_Unit==4 & df$TimeLastPeriod_Num==1)]<-
      as.numeric(df$TIMEMENSTRUATE_Numeric[which(df$TIMEMENSTRUATE_Unit==4 & df$TimeLastPeriod_Num==1)])*12
    
    df$MontheSinceMenstruate <- as.numeric(df$MonthSinceMenstruate)
    
    df$PPA <- 0
    df$PPA[which(df$PREGNANT==1 | df$MENSTRUAL_LASTBIRTH==0)] <- 1
    
    
    ###Fix cases when MENSTRUAL_LASTBIRTH is missing
    df$PPA[which((df$MENSTRUAL_LASTBIRTH==7) & !is.na(df$MonthSinceLastBirth) & !is.na(df$MonthSinceMenstruate)
                 & df$MonthSinceLastBirth<60 & (df$MonthSinceMenstruate>df$MonthSinceLastBirth))] <- 1
    
    df$PPA[which((df$MENSTRUAL_LASTBIRTH==7)
                 & !is.na(df$MonthSinceLastBirth) & 
                   (df$TIMEMENSTRUATE_Numeric==95 | df$TIMEMENSTRUATE_Unit==995)& df$MonthSinceLastBirth<60)]<- 1
    
    
    df$PPA24<-0
    df$PPA24[which(df$PREGNANT==1 | (df$PPA==1 & df$MonthSinceLastBirth<24))]<-1
    
    
    ##Criterias: i) Wantedness on Current/Last birth
    ##              a) FPLCHDESIRE & PGDESIRE: 2 = Later
    ##                                         3 = No more
    ##                                         8 = Missing
    ###WantChild 1:Then
    ###          2:Later
    ###          3:Not at all
    ###          8:Missing
    df$wantchild<-NA
    ####For Current Pregnancy
    df$wantchild<-ifelse(df$PREGNANT==1,df$PGDESIRE_NOW,df$wantchild)
    df$wantchild<-ifelse((df$PREGNANT==1&df$PGDESIRE_NOW==0&df$PGDESIRE_LATER==3)
                         ,3,df$wantchild)
    df$wantchild<-ifelse((df$PREGNANT==1&df$PGDESIRE_NOW==0&df$PGDESIRE_LATER==2)
                         ,0,df$wantchild)
    df$wantchild<-ifelse((df$PREGNANT==1&df$PGDESIRE_NOW==0&df$PGDESIRE_LATER==8)
                         ,8,df$wantchild)
    
    ####For Future Desire
    df$wantchild<-ifelse(df$PREGNANT!=1,df$FPLCHDESIRE_NOW,df$wantchild)
    df$wantchild<-ifelse((df$PREGNANT!=1&df$FPLCHDESIRE_NOW==0&df$FPLCHDESIRE_LATER==3)
                         ,3,df$wantchild)
    df$wantchild<-ifelse((df$PREGNANT!=1&df$FPLCHDESIRE_NOW==0&df$FPLCHDESIRE_LATER==2)
                         ,0,df$wantchild)
    df$wantchild<-ifelse((df$PREGNANT!=1&df$FPLCHDESIRE_NOW==0&df$FPLCHDESIRE_LATER==8)
                         ,8,df$wantchild)
    df$wantchild<-ifelse(is.na(df$wantchild),8,df$wantchild)
    
    df$Unmet[which(is.na(df$Unmet) & df$PPA24==1 & df$wantchild==1)]<-7
    df$Unmet[which(is.na(df$Unmet) & df$PPA24==1 & df$wantchild==0)]<-1
    df$Unmet[which(is.na(df$Unmet) & df$PPA24==1 & df$wantchild==3)]<-2
    df$Unmet[which(is.na(df$Unmet) & df$PPA24==1 & df$wantchild==8)] <- 99
    
    ##Sexual Activity
    if("TIMESINCESEX_Unit"%in%names(df)){
      #Sexually Active defined as 1 month
      df$Sex<-0
      df$Sex[which((df$TIMESINCESEX_Unit==1 & as.numeric(df$TIMESINCESEX_Numeric)<=30) |
                     (df$TIMESINCESEX_Unit==2 & as.numeric(df$TIMESINCESEX_Numeric)<=4) |
                     (df$TIMESINCESEX_Unit==3 & as.numeric(df$TIMESINCESEX_Numeric)<=1))] <- 1
      
      #Sexually Active defined as 3 months
      # df$Sex<-0
      # df$Sex[which((df$TIMESINCESEX_Unit==1 & as.numeric(df$TIMESINCESEX_Numeric)<=90) |
      #                (df$TIMESINCESEX_Unit==2 & as.numeric(df$TIMESINCESEX_Numeric)<=12) |
      #                (df$TIMESINCESEX_Unit==3 & as.numeric(df$TIMESINCESEX_Numeric)<=3))] <- 1

      ###Identify not sexually active women
      df$Unmet[which(is.na(df$Unmet) & (df$MSTAT!=1|is.na(df$MSTAT)) & (df$Sex!=1))] <- 97
      
      #Label Sexual Activtiy categories
      df$SexAct <- factor(df$Sex, 
                          levels=c(1,0),
                          labels=c("Sexually_Active","Sexually_Inactive"))
    }else{
      df$Unmet[which(is.na(df$Unmet)&(df$MSTAT!=1|is.na(df$MSTAT)))]<-97
    }
    
    #Group 3 - Determine Fecundity
    df$infecud <- NA
    ###Set available observations for infecud (only if unmet is still unidentified)
    df$infecud[which(is.na(df$Unmet)&!df$PREGNANT==1&!df$PPA24==1)]<-0
    
    df$nonPPA <- NA
    df$nonPPA[which(is.na(df$Unmet)&!df$PREGNANT==1&!df$PPA24==1)]<-1

    if("MAR1STCMC" %in% names(df)){
      ##3.1 - married 5+ years ago, no children in past 5 years, never used contraception, excluding pregnant and PPA <24 months
      df$TimeMarried <- as.numeric(df$INTDATECMC) - as.numeric(df$MAR1STCMC)
      
      if(!is.null(df$FPEVUSE)){
        df$infecud[which(df$nonPPA==1 & df$MSTAT==1 & df$TimeMarried>=60 & (is.na(df$MonthSinceLastBirth)|df$MonthSinceLastBirth>=60) & df$FPEVUSE!=10)]<-1
      }else{
        ###FPEVUSE does not exist in data set (Many cases)
        df$infecud[which(df$nonPPA==1 & df$MSTAT==1 & df$TimeMarried>=60& (is.na(df$MonthSinceLastBirth)|df$MonthSinceLastBirth>=60) & (df$NOPREG2YRS==1 | df$TOOOLD==1))] <- 1
      }
    }
    
    ##3.2 - Future Desire of Children
    df$infecud[which(df$nonPPA==1  & (as.numeric(df$FUTCHLD_Numeric) == 94 | df$FUTCHLD_NPREG == 3 | df$FUTCHLD_Unit==994))] <-1
    
    ##3.3
    ###SPSS Code uses Reason on not able to get pregnant - Menopausal/Hysterectomy as the variable###
    df$infecud[which(df$nonPPA==1  & (df$MENOPAUSAL == 1 | df$HYSTERECTOMY == 1))] <- 1
    df$infecud[which(df$nonPPA==1 & df$NEVERMENSTRUATE==1 & (is.na(df$MonthSinceLastBirth) | df$MonthSinceLastBirth >=60))] <- 1
    
    ##3.4
    ###Time Since Last Period > 6months & Not postpartum amenorrheic
    df$infecud[which(df$nonPPA==1 & (!is.na(df$MonthSinceMenstruate)) & df$MontheSinceMenstruate>=6 & df$PPA!=1)] <- 1
    
    
    ##3.5 df$TIMEMENSTRUATE_Unit == 996 | | df$TIMEMENSTRUATE_Numeric == 96
    ###Time Since last period = menopausal/hysterectomy or = never menstruated
    df$infecud[which(df$nonPPA==1 & (df$TIMEMENSTRUATE_Unit == 994 | df$TIMEMENSTRUATE_Numeric == 94))]<-1
    df$infecud[which(df$nonPPA==1 & (df$TIMEMENSTRUATE_Unit == 996 | df$TIMEMENSTRUATE_Numeric == 96) & (df$MonthSinceLastBirth >=60 | is.na(df$MonthSinceLastBirth)))] <- 1
    
    ##3.6
    ###Time since last period = before birth & Last Birth > 5years
    df$infecud[which(df$nonPPA==1 & (df$MonthSinceLastBirth >=60|is.na(df$MonthSinceLastBirth)) &
                       (df$TIMEMENSTRUATE_Unit == 995 | df$TIMEMENSTRUATE_Numeric == 95))]<- 1
    df$infecud[which(df$nonPPA==1 & is.na(df$MonthSinceLastBirth) &
                       (df$TIMEMENSTRUATE_Unit == 995 | df$TIMEMENSTRUATE_Numeric == 95))]<- 1
    
    df$Unmet[which(df$infecud==1 & df$nonPPA==1)] <- 9
    
    #Group 4
    ##4.1 Want child within 2years or very soon - No unmet need(unmet=7)
    df$Unmet[which(is.na(df$Unmet) & df$nonPPA==1 
                   & df$FUTCHLD_NPREG==1 & df$FUTCHLD_Unit==1 & as.numeric(df$FUTCHLD_Numeric)<24)] <- 7
    df$Unmet[which(is.na(df$Unmet) & df$nonPPA==1
                   & df$FUTCHLD_NPREG==1 & df$FUTCHLD_Unit==2 & as.numeric(df$FUTCHLD_Numeric)<2)]<-7
    df$Unmet[which(is.na(df$Unmet) & df$nonPPA==1
                   & df$FUTCHLD_NPREG==1 & as.numeric(df$FUTCHLD_Numeric)==93)] <- 7
    
    
    ##4.2 Want child in 2+years, after marriage, undecided or unsure if wants
    df$Unmet[which(is.na(df$Unmet) & df$nonPPA==1
                   & df$FUTCHLD_NPREG==1 & as.numeric(df$FUTCHLD_Numeric)<=90)] <- 1
    df$Unmet[which(is.na(df$Unmet) & df$nonPPA==1
                   & df$FUTCHLD_NPREG==1 & df$FUTCHLD_Numeric==95)]<-1
    df$Unmet[which(is.na(df$Unmet) & df$nonPPA==1
                   & df$FUTCHLD_NPREG==1 & (df$FUTCHLD_Numeric==98 | df$FUTCHLD_Unit==998))]<-1
    
    df$Unmet[which(is.na(df$Unmet) & df$nonPPA==1
                   & df$FUTCHLD_NPREG==8)] <- 1
    df$Unmet[which(is.na(df$Unmet) & df$nonPPA==1
                   &df$FUTCHLD_NPREG==2)] <-2
    
    df$Unmet[which(is.na(df$Unmet))]<-99
    
    #Label Unmet Needs categories
    df$UnmetNeed <- factor(df$Unmet, 
                           levels=c(1,2,3,4,7,9,97,98,99), 
                           labels=c("UnmetNeed_for_Spacing", "UnmetNeed_for_Limiting", "Using_for_Spacing",
                                    "Using_to_Limit","No_UnmetNeed","Infecund/Menopausal","Not_Sexually_Active", "Unmarried_Sample/NoData","UnmetNeed_Missing"))
    #Label to specifc group of unmet need
    df$Specific_unmet<- df$Unmet
    df$Specific_unmet[which(!df$Specific_unmet%in%c(1,2) & (df$UnmetSim!=9 | is.na(df$UnmetSim)))] <- 0
    
    df$Specific_unmet <- factor(df$Specific_unmet,
                                levels = c(0,1,2),
                                labels = c("No_Unmet_Need", "UnmetNeed_for_Spacing","UnmetNeed_for_Limiting"))
    
  }else{
    df$UnmetSim <- 9
  }
  #Simplify Unmetneed Categories to Yes or No
  df$UnmetSim[which(df$Unmet==1 | df$Unmet==2)] <- 1
  df$UnmetSim[which(!df$Unmet %in% c(1,2) & (df$UnmetSim!=9 | is.na(df$UnmetSim)))] <- 0
  
  #Remove unmet need estimates when sexual activity is not present in dataset
  if(!"TIMESINCESEX_Unit"%in%names(df)){
    df$UnmetSim[which(df$MSTAT_LAB=="Unmarried" | df$MSTATNF_LAB=="Formerly-Married" | df$MSTATNF_LAB == "Never-Married")] <- 9
  }
  
  #Label Unmet Needs Simplied categories
  df$UnmetSim <- factor(df$UnmetSim,
                        levels=c(0,1,9),
                        labels=c("No_Unmet_Need","Unmet_Need","MissingData"))
  
  return(df)
}