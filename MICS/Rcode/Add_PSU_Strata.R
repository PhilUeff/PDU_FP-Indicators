#Add in PSU and Strata variables 
##1. Find in household (HH) file
##2. Find in HL file
##3. Match with excel table created that recorded stratified methods from the report

rm(list=ls())
library(tools)
library(haven)
library(stringr)

PathMICS <- "V:/FertilitySection/Alkema_Joint project on contraceptive use trends/1_All-women-estimates&projections/Data-tabulations/MICS/Translated_RDataFiles"
RawSurveys.dir <- "V:/MICS/Microdata/" 

mics.list <- dir(PathMICS, pattern = ".RData$")

#Table includes all PSU and Strata information obtained from the report
summary <- read.csv("V:/FertilitySection/Alkema_Joint project on contraceptive use trends/1_All-women-estimates&projections/Data-tabulations/MICS/Standard-Errors/PSU_Strata_Summary.csv",stringsAsFactors=FALSE)

OtherFile_Vars <- function(SurveyCode, wmdf){
  files.type <- c("hh","hl")
  
  for (f in files.type){
    tempfile <- gsub("wm",f,SurveyCode) #Obtain survey code - from loaded women file
    tempdata <- read_spss(paste(RawSurveys.dir,tempfile,".sav",sep="")) #Read in household/houselisting data
    tempdf <- as.data.frame(tempdata, stringAsFactors = T)
    colnames(tempdf)<-tolower(colnames(tempdf))
    
    #"hid" = concatenated id in the women file - use directly to match with hh/hl file if exists
    #concatenation of "hh1" and "hh2" = "hid" - becomes merging id between wm and hh/hl file
    if("hid" %in% names(wmdf)){
      wmdf$id <- wmdf$hid
      tempdf$id <- tempdf$hid
    }else if ("hh1" %in% names(wmdf)){
      wmdf$id <- paste0(str_pad(wmdf$hh1,6,pad="0"),str_pad(wmdf$hh2,3,pad="0"))
      tempdf$id <- paste0(str_pad(tempdf$hh1,6,pad="0"),str_pad(tempdf$hh2,3,pad="0"))
    }
    
    #Find variations of possible strata variable 
    ##If exists copy such variable to the women file and standardized the name as "stata"
    if("stratum"%in%names(tempdf)){
      tempdf$strata<-tempdf$stratum
    }else if("estrato"%in% names(tempdf)){
      tempdf$strata <- tempdf$estrato
    }else if("strat"%in% names(tempdf)){
      tempdf$strata <- tempdf$strat
    }
    
    if("strata"%in% names(tempdf)){
      #When strata is found - look for psu if it's not in the wm file but is in the hh/hl file
      if(!"psu"%in% names(wmdf) && "psu" %in% names(tempdf)){
        formerge <- tempdf[,c("id","psu","strata")] 
      }else{
        formerge <- tempdf[,c("id","strata")] 
      }
      #Merge wm and subsetted hh/hl file containing only psu and strata variable 
      final <- merge(wmdf,formerge, by ="id",all.x=T)
      return(final) #Return dataframe with strata (&) psu added
    }else if(f=="hl"){
      return (wmdf) #Case when "strata" cannot be found in hh/hl file - return original women's file
    }
  }
}
SPSSCode_Vars <- function(SurveyCode, wmdf){
  #Extract Strata by sampling methods
  SPSS_Method <- summary[which(summary$SurveyID == SurveyCode),"Strata.Method"]
  if(!identical(SPSS_Method,character(0))){
    if(SPSS_Method!="" && SPSS_Method!="Match to HH File" && SPSS_Method!="Self-weighted"){
      #Possible variables included in the summary table as strata
      Region <- c("hh7","hi7","wmreg","region","hh7a")
      Urban_Rural <- c("hh6","hi6","area","hh6a","resid")
      Governorates <- c("hh7","hi7")
      District <- c("hh7a","hi6a")
      Interior_Coast <- "district"
      CityTown <- "citytown"
      Parish <- "hh1a"
      State_Division <- "hisd"
      Place_of_Residence <- "hi6"
      States <- "hi7"
      
      # Case when strata consists of 3 variables 
      if(grepl(",",SPSS_Method)){
        MethodA<-gsub( ",.*$", "", SPSS_Method)
        MethodB<-gsub( " and .*$", "", SPSS_Method )
        MethodB<-gsub(".*, ","",MethodB)
        MethodC <- gsub( ".*and ", "", SPSS_Method )
        for(a in get(MethodA)){
          if(a %in% names(wmdf)){
            MethodA_Var <- a
            break
          }
        }
        for(b in get(MethodB)){
          if(b %in% names(wmdf)){
            MethodB_Var <- b
            break
          }
        }
        for(c in get(MethodC)){
          if(c %in% names(wmdf)){
            MethodC_Var <- c
            break
          }
        }
        wmdf$strata <- wmdf[,MethodA_Var]*100 + wmdf[,MethodB_Var]*10 + wmdf[,MethodC_Var]
      }
      #Case when strata consists of 2 variables
      else if(grepl(" and ",SPSS_Method)){
        MethodA<-gsub( " and .*$", "", SPSS_Method )
        MethodB<-gsub( ".*and ", "", SPSS_Method )
        for(a in get(MethodA)){
          if(a %in% names(wmdf)){
            MethodA_Var <- a
            break
          }
        }
        for(b in get(MethodB)){
          if(b %in% names(wmdf)){
            MethodB_Var <- b
            break
          }
        }
        wmdf$strata <- wmdf[,MethodA_Var]*10 + wmdf[,MethodB_Var]
      }
      #Case when strata is defined by 1 variable
      else {
        for(s in get(SPSS_Method)){
          if(s %in% names(wmdf)){
            wmdf$strata <- wmdf[,s]
            break
          }
        }
      }
      #Survey = self-weighted = no stratification is done, strata sets to be 1
    }else if(SPSS_Method == "Self-weighted"){
      wmdf$strata<- 1
    }
  }
  return (wmdf)
}

for(i in 1:length(mics.list)){

  #Reads in Women data
  load(paste(PathMICS, mics.list[i],sep="/"))

  SurveyCode <- file_path_sans_ext(mics.list[i])
  print (SurveyCode)
  
  #Only add strata var if all of the possible variations are not in wm file
  if(!"stratum" %in% (names(df)) && !"strata" %in% (names(df)) && !"estrato" %in% (names(df))){
    #Find in hh file
    hhhl_df<-OtherFile_Vars(SurveyCode,df)
    if(!"strata"%in%(names(hhhl_df))){
      #Find based on summary table
        spss_df <- SPSSCode_Vars(SurveyCode,df)
        if(!"strata"%in% (names(spss_df))&&(all(df$wmweight==1 | is.null(df$wmweight)))){
          df$strata <- 1
        }
      }
  }else if("stratum" %in% names(df)){
    df$strata <- df$stratum
  }else if("estrato" %in% names(df)){
    df$strata <- df$estrato
  }
  
  if(exists("spss_df")){
    df<-spss_df
  }else if (exists("hhhl_df")){
    df<- hhhl_df
  }
  
  if(!"psu"%in% names(df)){
    PSUVars <- c("hh1","hi1","wiclno","af13","ttclno")
    for (p in PSUVars){
      if(p %in% names(df)){
        df$psu <- df[,p]
        break
      }
    }
  }
  save(df,file = paste(PathMICS,paste0(SurveyCode, ".RData"),sep="/"))
  
  rm(spss_df,hhhl_df)
}
