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



#Translating variables
#Variable names must contain all tt_"variables"

################ DIRECTORIES ##############################
# Directory where MICS data are stored:
mics.dir <- "V:\\MICS\\Microdata\\"

# Set working directory (where the translationtables are)
setwd("V:/MICS/Translation tables")
# Store the Translated surveys
translatedPath <- ("V:/FertilitySection/Alkema_Joint project on contraceptive use trends/1_All-women-estimates&projections/Data-tabulations/MICS/Translated_RDataFiles")

###############	LIBRARIES	###############################
library(foreign) #Read in spss
library(plyr) #Mapvalues
library(tools)
library(haven)

######################################################################################################################
##########################   LOAD INVENTORY LIST OF MICS SURVEYS  																          		  
######################################################################################################################

# Read in the MICS inventory list for standard surveys into mics.list dataframe:
mics.main.list <- read.csv("V:\\MICS\\MICSMaster.csv", header=TRUE, stringsAsFactors = FALSE, na.strings = c("..", "NA", "", " "))

# subset the inventory list to keep only the information for the survey(s) you want to run
mics.list <- subset(mics.main.list, Phase=="MICS2" | Phase=="MICS3" | Phase=="MICS4" | Phase=="MICS5") 
mics.list <- subset(x=mics.list, subset = !is.na(Individual.Recode))

# exclude problematic surveys
mics.list <- subset(x=mics.list, subset = !Individual.Recode%in%c("bswm2"))


######################################################################################################################
##########################   LOAD VARIABLE NAME TRANSLATION TABLE  																          		  
######################################################################################################################

# Inventory of harmonised and original variables names (header row needs to identify Individual.Recode)
nameInventory <- read.csv("tt_variableNames.csv", stringsAsFactors = F, header = TRUE)

# Translation tables of variable values will be loaded individually within a local function at later stage

#######################################################################################################################
###############		 	FUNCTION TO OUTPUT CONTRACEPTIVE PREVALENCE BY MARITAL STATUS
#######################################################################################################################
##	Function to calculate the desired data 
Translation <- function(surveyID,surveyInfo){
  # Load dataset
  dat <- read_spss(paste(mics.dir, surveyInfo$Individual.Recode, ".sav", sep = ""))
  df <- as.data.frame(dat, stringsAsFactors = TRUE)
  dat_varLabs <- attr(dat, "variable.labels")
  dat_valLabs <- attr(dat, "label.table")
  
  # Set column names to lower cases only 
  colnames(df) <- tolower(colnames(df))
  
  ###------------------------ APPLY TRANSLATION TABLES ---------------------------------###
  
  ## list of variables to compute through translation tables
  #varList <- c("INTRESULT", "AGE5YEAR", "MARSTAT", "FPNOW", "FPNOWUSFST") # select variable that you want to translate
  varList <- nameInventory$Name # easier if you want to translate each variable available for harmonisation
  
  ## computation of harmonised variables
  for (varNew in varList) {
    print(varNew)
    varOriginal <- tolower(nameInventory[nameInventory$Name == varNew, surveyID])
  
#    if(varOriginal == "cm11x$"){
#      varOriginal <- "cm11x."
#    }
    
    if (varOriginal != "" & !is.na(varOriginal)) {    # Only run if the variable is present in the dataset (if cell in translation table is not "" )
      transTable <- read.csv(paste0("tt_", tolower(varNew), ".csv"), header = TRUE)

      # MICS data have lots of trailing and leading spaces that need to be trimmed for matching
      df[,varOriginal] <- gsub("^\\s+|\\s+$", "", df[ ,varOriginal])
      
      # MICS data have cells with only spaces which should be NAs. After trimming set "" to NA
      df[, varOriginal][which(df[,varOriginal] == "")] <- NA
      
      # Get original and harmonised values from translation table
      original <- as.character(gsub("=.*$", "", transTable[ , surveyID])) # Get what is written before equal sign
      original <- gsub("^\\s+|\\s+$", "", original)   # Trim leading or trailing spaces
      original[which(original == "")] <- NA  # Treat empty cells as NAs
      harmonised <- as.character(transTable[ , "Code"])
      harmonised_LAB <- as.character(transTable[ , "Label"]) #Labels
      
      if("TRUE" %in% transTable[,"Hybrid"]){
        df[varNew]<-ifelse(df[,varOriginal]%in%original, 
                               (mapvalues(df[,varOriginal], from = original, to = harmonised, warn_missing=F)),
                               (df[,varOriginal]))
        df[paste0(varNew, "_LAB")]<-ifelse(df[,varNew]%in%harmonised, mapvalues(df[,varNew], from = harmonised, to = harmonised_LAB,warn_missing=F),df[,varNew])
      }else{
        # Harmonised variable with numeric categories
        df[varNew] <- mapvalues(df[ , varOriginal], from = original, to = harmonised, warn_missing = F)
        # Harmonised variable with text categories, with added "_LAB" description to end of haromised variable name
        df[paste0(varNew, "_LAB")] <- mapvalues(df[ , varNew], from = harmonised, to = harmonised_LAB, warn_missing = F)
      }

      df[,varNew][which(is.na(df[varOriginal]))] <- NA #overwrite missclassification of NAs introduced by mapvalues function
      df[, paste0(varNew, "_LAB")][which(is.na(df[varNew]))] <- NA #overwrite missclassification of NAs introduced by mapvalues function
    }
  }
 
  ###------------------------ EXCLUDE CASES OF INCOMPLETE INTERVIEW ---------------------------------###
  # If variable on interview result was included then only keep completed interviews.
  if (nameInventory[nameInventory$Name == "INTRESULT", surveyID] != "" & !is.na(nameInventory[nameInventory$Name == "INTRESULT", surveyID]))  {
    df <- subset(df, INTRESULT == 1)
  }

  return(df)
}

#Find all RData format file in TranslatePath = The translated table outputed
Translated.list <- dir(translatedPath,pattern=".RData$")
Translated.list<-file_path_sans_ext(Translated.list)

#Subset all surveys code from Translted survey code = retain untranslated surveys 
UnTranslated <- subset(mics.list, !Individual.Recode %in% Translated.list)

if(nrow(UnTranslated)>0){
  for(i in 1:nrow(UnTranslated)){

    surveyID <- UnTranslated[i,"Individual.Recode"]
    print(surveyID)
  
    # subset mics.list to show info for individual dataset
    surveyInfo <- subset(mics.list, Individual.Recode == surveyID)
    
    df<- Translation(surveyID,surveyInfo)

    if(!is.null(df$wmweight)){
      df$wmweight<-as.numeric(df$wmweight)
    }else{
      df$wmweight <- 1
    }
  
    save(df,file=paste(translatedPath, paste0(surveyID,".RData"),sep="/"))
    
  }
}else{
  print ("All surveys are Translated")
}

