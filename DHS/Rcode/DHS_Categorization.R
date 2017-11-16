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



library(plyr) 

# Compute marital status categories
Define <- function(ir.data){
  ir.data$mstatus <- NA
  #Drop Currently Married 
  ##(mstatusBinary contains Married and Unmarried)
  ##mstatus only contains subcategories of Unmarried
  ir.data$mstatus <- mapvalues(ir.data$v502, from =c(NA,0,1,2,9), to =c(NA,0,NA,2,9))
  
  #Binary Variable for marital status of either "Married/in-union' or 'Unmarried/Not-in-union
  ir.data$mstatusBinary <- NA
  ir.data$mstatusBinary[which(ir.data$v502==1)] <- 1
  ir.data$mstatusBinary[which(ir.data$v502==9)] <- 9
  
  #Remove Unmarried categories if formerly-married exists but not never-in-unin
  ir.data$mstatusBinary[which(ir.data$v502!=1 & ir.data$v502!=9)] <-2
  
  
  ir.data$agegroup <- ir.data$v013
  
  return(ir.data)
}

# Compute variable labels
MapVal <- function(ir.data){
  ir.data <- Define(ir.data)
  #Contraceptive Use
  ir.data$method <- factor(ir.data$method, levels = c(0,1,2,9),
                           labels = c("Not_using_any_method", "Using_modern_method", "Using_traditional_method", "Unknown"))
  
  #Unmet Need
  ir.data$unmetneed <- ir.data$unmet
  
  ir.data$unmetneed <- factor(ir.data$unmetneed, levels = c(1,2,3,4,7,9,97,98,99), 
                              labels = c("unmet_need_for_spacing", "unmet_need_for_limiting", "using_for_spacing",
                                         "using_for_limiting", "no_unmet_need", "infecund_or_menopausal", "not_sexually_active",
                                         "unmarried_EM_sample_or_no_data", "unmet need missing"))
  
  ir.data$sexact <- factor(ir.data$sexact, levels = c(1,2,3),
                           labels = c("Sexually_active", "Sexually_inactive", "Never_had_sex"))
  
  ir.data$unmettot <- factor(ir.data$unmettot,
                             levels = c(0,1),
                             labels = c("No_unmet_need", "Unmet_need" ))
  
  #Marital Status
  ir.data$mstatus <- factor(ir.data$mstatus, levels = c(0,2,9),
                            labels = c("Never married", "Formerly married", "Marital Status, Missing"))
  
  ir.data$mstatusBinary <- factor(ir.data$mstatusBinary, levels = c(1,2,9),
                                  labels = c("Married/In-union", "Unmarried/Not-in-union", "Marital Status, Missing"))
  
  #Age
  ir.data$agegroup <- factor(ir.data$agegroup, levels = c(1,2,3,4,5,6,7), 
                             labels = c("[15-19]", "[20-24]", "[25-29]", "[30-34]", "[35-39]", "[40-44]","[45-49]"))
  return (ir.data)
}

# Compute contraceptive method categories
Categorization_SE <- function(ir.data){
  ir.data<-Define(ir.data)
  
   #For SE
  ir.data$anymethod <- ifelse(ir.data$method %in% c(1,2), 1, 0) 
  
  ir.data$tradmethod <- ifelse(ir.data$method == 2, 1, 0)
  
  ir.data$modernmethod <- ifelse(ir.data$method == 1, 1, 0)
  
  ir.data$unmettot <- ir.data$unmettot
  
  ir.data$maritalstatus <- ifelse(ir.data$mstatusBinary == 1, 1, 0)
  
  return (ir.data)
}