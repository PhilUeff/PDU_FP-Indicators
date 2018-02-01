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



#Translate available DHS-ir surveys
#Only on method variable (v312)

rm(list = ls())

library(foreign)
library(plyr) 
library(tools)

setwd("V:/FertilitySection/Alkema_Joint project on contraceptive use trends/1_All-women-estimates&projections/Data-tabulations/DHS")

## Read in variable translation table
transTable <- read.csv("v:/DHS/Translation/DHS_VariableTranslation_v312.csv")

## Classify modern and tradition contraceptive methods (See translation table for specific method harmonised code)
modern <- c(100:108, 110:112, 120:122, 130:136, 140, 150, 160)
traditional <- c(141, 200, 210:217, 220, 230:236)
other <- c(300:304)
notusing <- c(999)
exclude <- c(997, 998)

dhs.dir <- "V:/DHS/Microdata"

## Directory where Populatiohn Division's DHS master file is stored
dhs.master <- "V:/DHS/DHSMaster.csv"

#===================LOAD THE INVENTORY LIST FOR SURVEYS THAT FOLLOW THE DHS STANDARD FOR FORMAT AND VARIABLE NAMES========================
## Read in the DHS inventory list for standard surveys into dhs.list dataframe:
dhs.list <- read.csv(dhs.master, header=TRUE, stringsAsFactors=FALSE, na.strings=c("..", "NA", "", " "))

## Subset the inventory list to keep only the Standard DHS surveys for which the 'Individual.Recode' ir.data are available
## Exclude pecial surveys (MIS)	
dhs.list <- subset(dhs.list, !is.na(Survey.code) & !is.na(Individual.Recode) &
                     (Type!="MIS" & Type=="Standard DHS" | Type=="Interim DHS" | Type=="Continuous DHS") & !is.na(Recode))

# Exclude Brazil 1991 as not nationally representative
dhs.list <- subset(dhs.list, !Survey.code %in% c("br21"))

#Directory where the translated files are stored
TranslatedPath <- "/Translated_RDataFiles/"
Translated <- file_path_sans_ext(dir("./Translated_RDataFiles/",pattern = ".RData$"))
#List of surveys that are in master file, not in translated directory
Untranslated <- subset(dhs.list, subset=!dhs.list[,"Survey.code"]%in%Translated)

# Only DHS that are in translation table
#dhs.list <- subset(dhs.list, Individual.Recode %in% names(transTable))

# Variables needed for calcuation of contraceptive prevalence and unmet need (To speed up working with RData files)
VarToKeep<-c("v000", "v020", "v007", "v502", "v605", "v312", "v215", "v213", "m6.1", "v225", "v527", "v528", "v005", "v013", "b3_01",
             "v536", "v512", "v302", "v375a", "v376", "s607d", "s607c", "v3a08d", "v602", "v001", "v002", "v222", "m10_1", "v008")

#==========================================================================================================================================

ContraceptiveVar <- function(ir.data){
  
  ## Classify modern and tradition contraceptive methods (See translation table for specific method harmonised code)
  modern <- c(100:108, 110:112, 120:122, 130:136, 140, 150, 160)
  traditional <- c(141, 200, 210:217, 220, 230:236)
  other <- c(300:304)
  notusing <- c(999)
  exclude <- c(997, 998)
  
  
  colnames(ir.data)<-tolower(colnames(ir.data))
  
  # Harmonized variables
  original <- as.numeric(gsub("=.*$", "", transTable[ , dosurvey$Individual.Recode]))
  harmony <- transTable[ ,"harmonised"]
  harmonised_LAB <- as.character(transTable[ , "label"])
  harmonised_LAB <- sub("^\\s+", "",harmonised_LAB) #Trim off trailing spaces for subcategories of methods in translation table
  
  ir.data$methodSpecific <- mapvalues(ir.data$v312, from = original, to = harmony)
  ir.data$methodSpecific[which(is.na(ir.data$v312))] <- NA
  
  # Apply classification
  ir.data$method <- NA
  ir.data$method[which(ir.data$methodSpecific %in% notusing)] <- 0
  ir.data$method[which(ir.data$methodSpecific %in% modern)] <- 1
  ir.data$method[which(ir.data$methodSpecific %in% traditional)] <- 2
  ir.data$method[which(ir.data$methodSpecific %in% other)] <- 2         
  ir.data$method[which(ir.data$methodSpecific %in% exclude)] <- 9
  
  ir.data$methodSpecific_LAB <- mapvalues(ir.data$methodSpecific,from=harmony, to=harmonised_LAB)
  
  return (ir.data)
}

if(nrow(Untranslated) > 0){
  for (i in 1:nrow(Untranslated)){
    
    SurveyID <- Untranslated[i,"Survey.code"]
    
    print (SurveyID)
    
    dosurvey <- subset(Untranslated, Survey.code == SurveyID)
    ir.data <- read.dta (paste(dhs.dir,"/", toupper(dosurvey$Individual.Recode), "FL.dta", sep=""),
                         convert.factors = F, convert.underscore = T)
    
    #Only keep variables needed
    ir.data<-ir.data[,names(ir.data)%in%VarToKeep]
    
    ir.data <- ContraceptiveVar(ir.data)
    
    #Recode weighting variable
    ir.data$weights <- ir.data$v005/1000000
    
    save(ir.data, file = paste0("Translated_RDataFiles/",SurveyID,".RData"))
  }
  
}else{
  print("All surveys are translated")
  
}


