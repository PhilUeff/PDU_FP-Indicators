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


VariableNames_Path <- "V:/MICS/Translation tables"
nameInventory <- read.csv(paste(VariableNames_Path,"tt_variableNames.csv",sep="/"), stringsAsFactors = F, header = TRUE)

library(plyr)

# Compute marital status categories
MaritalStatusCategories<-function(df,choice){
  if(choice == "Both"){
    df<-MaritalStatusCategories(df,"General") #Categorization for Married and Unmarried categories
    df<-MaritalStatusCategories(df,"Unmarried")
  }else if(choice == "General"){
    married <- c(11, 20:22)
    unmarried <- c(10,30:35,40)
    missing_notstated <- c(98,99)
    
    #Another variable of marital status seems to be used in report (Current marital status)
    ##State of Palestine - PHL26 (Translated to CurrentMStatus) match with reported marital status table
    if(surveyID == "pswm4"){
      df["MSTAT"]<-NA
      df$MSTAT[which(df$CURRENTMSTATUS %in% married)]<-1
      df$MSTAT[which(df$CURRENTMSTATUS %in% unmarried)]<-2
      df$MSTAT[which(is.na(df$CURRENTMSTATUS) & df$EVERMARRIED %in% unmarried)]<-2
      df$MSTAT[which(df$CURRENTMSTATUS %in% missing_notstated)]<-NA
    }else{
      ##Categorize for Married/Unmarried women
      df["MSTAT"]<-NA
      df$MSTAT[which(df$MARSTAT %in% married)]<-1
      df$MSTAT[which(df$MARSTAT %in% unmarried)]<-2
      df$MSTAT[which(is.na(df$MSTAT) & df$EVERMARRIED %in% unmarried)]<-2
      df$MSTAT[which(df$MARSTAT %in% missing_notstated)]<-NA
    }
    
    df$MSTAT_LAB <- mapvalues(df$MSTAT, from = c(1,2), to=c("Married/In-union","Unmarried/Not-in-union"), warn_missing = F)
  }else if(choice == "Unmarried"){ #Categorization for Formerly Married and Never Married categories
    formerly <- c(30:35)
    never <- c(10)
    
    df["MSTATNF"]<-NA
    df$MSTATNF[which(df$MARSTAT %in% formerly)]<-1
    df$MSTATNF[which(df$MARSTAT %in% never)]<-2
    
    if(!any(1%in%df$MSTATNF) && !is.null(df$EVERMARRIED)){
      df$MSTATNF[which(df$EVERMARRIED %in% formerly)] <-1
      df$MSTATNF[which(df$EVERMARRIED %in% never)] <- 2
    }
    
    df$MSTATNF_LAB <- mapvalues(df$MSTATNF, from = c(1,2), to=c("Formerly married","Never married"), warn_missing = F)
    
    if("Formerly-Married" %in% df$MSTATNF_LAB && !"Never-Married" %in% df$MSTATNF_LAB){
      df$MSTAT_LAB[which(df$MSTAT_LAB=="Unmarried")]<- NA
    }
  }
  return (df)
}

# Compute contraceptive method categories
MethodCategories <- function(df){
  # Women who are coded as NA also need to be classified as 2 (Non-users) so that they are later included in the denominator for CP
  df$FPANY <- NA
  df$FPANY <- mapvalues(df$FPNOW, from = c(1, 2, 9, NA), to = c(1, 2, 2, 2), warn_missing = F)

  #Determine effectiveness of methods presented
  ## Variable of single most effective method used, based on Trussel and WHO
  df$FPMETHOD <- NULL
  df$FPMETHOD <- NA
  ###Specific methods are under FPMETHNOW for bswm2
  if("FPMETHNOW"%in%names(df)){
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 9)] <- 102 
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 8)] <- 106
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 2)] <- 101
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 7)] <- 105
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 2)] <- 101
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 3)] <- 120
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 1)] <- 110
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 6)] <- 103
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 11)] <- 210
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 4)] <- 130
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 5)] <- 104
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 13)] <- 220
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 14)] <- 300
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPMETHNOW == 10)] <- 303
  }
  ###Specific methods are separated into different variables
  else{
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSIMP == 1)] <- 102 # Implant
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSMST == 1)] <- 106 # Male Sterilisation
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSIUS == 1)] <- 101 # IUS
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSFST == 1)] <- 105 # Female Sterilsaiton
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSIUD == 1)] <- 101 # IUD ## Some IUS might have been classified as IUD in MICS as differentiation not made. Trussel: IUS > FST > IUD 
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSINJ == 1)] <- 120 # Injection ## Not in Trussel
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSINJ1 == 1)] <- 121 ## Not in Trussel
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSINJ2 == 1)] <- 122 ## Not in Trussel
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSINJ3 == 1)] <- 123 ## Not in Trussel
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSPILL == 1)] <- 110 # Pill
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSPAT == 1)] <- 107 # Patch
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSRING == 1)] <- 108 # Ring
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSCONM == 1)] <- 103 # Male condom
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSRHY == 1)] <- 210 # Rhythm
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSSDM == 1)] <- 212 # Standard Days Method
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSDIA == 1)] <- 130 # Diaphragm
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSCONF == 1)] <- 104 # Female condom
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSLAM == 1)] <- 140 # LAM ## Not in Trussel

    
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSWD == 1)] <- 220 # Withdrawal
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSFOA == 1)] <- 135 # Foam ## Spermicides in Trussel
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSEC == 1)] <- 150 # Emergency contraception ## Not in Trussel
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSBF == 1)] <- 141 # Breasfeeding ## Not in Trussel
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSOTH == 1)] <- 300 # Other
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSNSP == 1)] <- 301 # Not specified
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSOTHMOD == 1)] <- 302 # Other modern
    df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPNOWUSOTHTRAD == 1)] <- 303 # Other traditional
  }
  
  # Set Variable of method type (modern/traditional/no use)
  df["FPTYPE"] <- NA
  
  modern <- c(100:108, 110:112, 120:123, 130:136, 140, 150, 160, 302)
  traditional <- c(141, 200, 210:217, 220, 230:236, 303, 212)
  other <- c(300, 301) # Not specified put into 'other'
  
  df$FPTYPE[which(df$FPMETHOD %in% modern)] <- 1
  df$FPTYPE[which(df$FPMETHOD %in% traditional)] <- 2
  df$FPTYPE[which(df$FPMETHOD %in% other)] <- 2 # other methods classified as traditional
  
  
  # Recode non-user in for denominator of calculation
  df$FPTYPE[which(is.na(df$FPTYPE) & df$FPANY == 2)] <- 3
  df$FPTYPE[which(is.na(df$FPTYPE) & df$FPANY == 1)] <- 4 #Using, but no method
  # If FPNOW not available (equal to "Not in translation table" then set all NAs to "Not using") define non-users through FPNOWUSXXXXs
  if (is.null(df$FPNOW) | all(is.na(df$FPNOW))){
    df$FPTYPE[which(is.na(df$FPMETHOD))] <- 3
    df$FPANY <- mapvalues(df$FPTYPE, from = c(1, 2, 3, NA), to = c(1, 1, 2, NA), warn_missing = F)
  }
  
  #Recategorize Non-user -> for cases when FPNOW is missing
  df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPANY == 2)] <- 999 # Categorise NAs as Non-users
  df$FPMETHOD[which(is.na(df$FPMETHOD) & df$FPANY == 1)] <- 998 # Identify women who have said that using contraception, but not specified method (Being excluded from calculation)
  df$FPMETHOD_LAB <- mapvalues(df$FPMETHOD,
                               from = c(102, 106, 101, 105, 101, 120, 121, 122, 123, 110, 107, 108, 103, 130, 104, 140, 150, 
                                        210, 220, 135, 141, 300, 301, 302, 303, 999, 998, 212),
                               to = c("IMP", "MST", "IUD_IUS", "FST", "IUD_IUS", "INJ", "INJ1", "INJ2", "INJ3", "PILL", "PAT", "RING", "CONM", "DIA", "CONF", "LAM", "EC",
                                      "RHY", "WD", "FOA", "BF", "OTH", "NSP", "OTHMOD", "OTHTRAD", "NotUsing", "FPAny_butNotInFPMETHOD","SDM"),
                               warn_missing = F)

  # Compute variable with descriptive names
  df$FPANY_LAB <- mapvalues(df$FPANY, from = c(1, 2, NA), to = c("Using_any", "Not_using", NA), warn_missing = F) # There shouldn't be any 'NA' values #### PHIL #### Problem?
  df$FPTYPE_LAB <- mapvalues(df$FPTYPE, from = c(1, 2, 3,4), to = c("Using_modern", "Using_traditional", "Not_using","Using_any_nomethod"), warn_missing = F)
  
  return (df)
}

# NOT USED
MethodAllocation <- function(df){
  ##Distribute FPAny_butNotInFPMETHOD among all the methods
  if(any(df$FPMETHOD == 998) & !is.na(any(df$FPMETHOD==998))){
    #Get Frequencies of all method
    FPMethod_ByAgeMSTAT<-as.data.frame(xtabs(~FPMETHOD+AGE5YEAR_LAB+MSTAT_LAB,df))
    FPMethod_AllMethod<-subset(FPMethod_ByAgeMSTAT,subset=!FPMethod_ByAgeMSTAT$FPMETHOD%in%c("998","999"))
    FPMethod_NoMethod <- subset(FPMethod_ByAgeMSTAT,subset=FPMethod_ByAgeMSTAT$FPMETHOD%in%c("998"))
    
    #Sum up all the available specific methods
    FPMethod_Total<-aggregate(x=FPMethod_AllMethod["Freq"],by=list(AGE5YEAR_LAB=FPMethod_AllMethod$AGE5YEAR_LAB,MSTAT_LAB=FPMethod_AllMethod$MSTAT_LAB),FUN=sum)
    FPMethod_Total$Total <- FPMethod_Total$Freq
    FPMethod_Total <- FPMethod_Total[,c("AGE5YEAR_LAB","MSTAT_LAB","Total")]
    
    #Get Frequency of FPAny_butNoMethod
    FPMethod_NoMethod$NoMethod.freq <- FPMethod_NoMethod$Freq
    FPMethod_NoMethod <- FPMethod_NoMethod[,c(1,2,3,5)]
    
    FPMethod_AllMethod<-merge(FPMethod_AllMethod,FPMethod_Total,by=c("AGE5YEAR_LAB","MSTAT_LAB"))
    FPMethod_All <- merge(FPMethod_AllMethod,FPMethod_NoMethod,by=c("AGE5YEAR_LAB","MSTAT_LAB"))
    
    #Get the number of observations needed to be allocate (Round by 0 in order to select sample (What to do with the remaining FPAny_NoMethod???))
    FPMethod_All$Rate<-NA
    FPMethod_All$Rate <- round((FPMethod_All$Freq / FPMethod_All$Total)*FPMethod_All$NoMethod.freq,0)
    FPMethod_All <- FPMethod_All[!is.na(FPMethod_All$Rate),]
    #Only works if there is only 1 FPANY_NoMethod -> can directly set to the method with highest rate
    if(all(FPMethod_All$Rate ==0)){
      FPMethod_All$Rate <- round((FPMethod_All$Freq / FPMethod_All$Total)*FPMethod_All$NoMethod.freq,1)
    }
    
    #Subset possible candidates for allocation
    FPMethod_Allocate <- subset(FPMethod_All, FPMethod_All$Rate > 0)
    FPMethod_Allocate <-  data.frame(lapply(FPMethod_Allocate, as.character), stringsAsFactors=FALSE)
    FPMethod_Allocate$Rate <- as.numeric(FPMethod_Allocate$Rate)
    
    #Match Marital status and Age, the method with the highest rate is replaced when there is only 1 FPAny_NoMethod
    if(length(which(df$FPMETHOD == 998))==1){
      FPMethod_NoMethod <- subset(FPMethod_NoMethod, subset=FPMethod_NoMethod$NoMethod.freq!=0)
      
      #Retain only the matching marital status and age, who has the highest rate - candidate of FPANY_NoMethod
      FPMethod_Allocate <- subset(FPMethod_Allocate, subset=FPMethod_Allocate$MSTAT_LAB == FPMethod_NoMethod$MSTAT_LAB &
                                    FPMethod_Allocate$AGE5YEAR_LAB == FPMethod_NoMethod$AGE5YEAR_LAB & FPMethod_Allocate$Rate == max(FPMethod_Allocate$Rate))
      
      df$FPMETHOD[which(df$MSTAT_LAB == FPMethod_NoMethod$MSTAT_LAB &
                          df$AGE5YEAR_LAB == FPMethod_NoMethod$AGE5YEAR_LAB &
                          df$FPMETHOD == 998)] <- as.numeric(FPMethod_Allocate[,"FPMETHOD.x"])
    }else{
      # Allocate to df based on matching marital status and age group
      ## Order of allocation? Highest rate first? (Affect if df does not have same # of observations correspond to FPAny_NoMethod's marital status and age)
      
      ###Order by Highest Rate
      # FPMethod_Allocate <- FPMethod_Allocate[with(FPMethod_Allocate,order(-Rate)),]
      # rownames(FPMethod_Allocate) <- 1:nrow(FPMethod_Allocate)
      
      for(s in 1:nrow(FPMethod_Allocate)){
        if(length(which(df$MSTAT_LAB == FPMethod_Allocate[s,"MSTAT_LAB"] &
                        df$AGE5YEAR_LAB == FPMethod_Allocate[s,"AGE5YEAR_LAB"] &
                        df$FPMETHOD == 998)) < FPMethod_Allocate[s,"Rate"]){
          #If there are less candidates from df compared to amount needed to be allocated
          ##Set all matching marital status and age observations to current loop's method
          df[row.names(subset(df,df$MSTAT_LAB == FPMethod_Allocate[s,"MSTAT_LAB"] &
                                df$AGE5YEAR_LAB == FPMethod_Allocate[s,"AGE5YEAR_LAB"] &
                                df$FPMETHOD == 998)),"FPMETHOD"] <- FPMethod_Allocate[s,"FPMETHOD.x"]
        }else{
          #Random sample the number of allocation needed from df
          ##Set the random sample FPMETHOD to current loop's method
          df[row.names(sample_n(subset(df,df$MSTAT_LAB == FPMethod_Allocate[s,"MSTAT_LAB"] &
                                         df$AGE5YEAR_LAB == FPMethod_Allocate[s,"AGE5YEAR_LAB"] &
                                         df$FPMETHOD == 998),as.numeric(FPMethod_Allocate[s,"Rate"]))),"FPMETHOD"] <- FPMethod_Allocate[s,"FPMETHOD.x"]
        }
      }
    }
  } #End of Allocation
  
  #Remaining FPAny_NoMethod puts into Other -> In order to retain all the woman in the denominator
  if(any(df$FPMETHOD == 998) & !is.na(any(df$FPMETHOD==998))){
    df$FPMETHOD[which(df$FPMETHOD == 998)] <- 300
  }
  
  return (df)
}