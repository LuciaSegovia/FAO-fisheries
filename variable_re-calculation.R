#
# This script combines food components in standardised FCTs
# 
# More details can be found in the documentation. 
#
# Note1: Most of the optional steps are commented out, to be run
# remove the "#" adjust for your dataset structure and needs and run
#
# Note2: If your dataset is not in standard format check documentation
# 
#
##Run this to clean the environment
rm(list = ls())


# Loading libraries
## Note: if it is the first time: install.packages() first
library(dplyr) # For data cleaning (wrangling)
library(stringr) # For string manipulation (data cleaning)
library(measurements) # For unit conversion
library(gt) # For generating tables
library(NutritionTools) # Nutrition functions
source(here::here("functions.R")) # Loading nutrition functions (change to package when ready)


# Loading the data
# For reproducing, our updating the FAO-fisheries load data by
#Uncommenting and running the following
#source(here::here("merging_all.R"))

data.df <- readRDS(here::here("data", "FAO-fish-standardised_v1.0.0.RDS"))
#data.df <- read.csv(here::here("Output", "template-name_FCT_FAO_Tags.csv"))
  
# 0) Check that we have all FCTs merged ----
data.df %>% 
  group_by(source_fct) %>% count()


# 3.6.2 ) Combining Tagnames to generate variables ----

# Note: Fat and Fibre are needed for recalculating:
# Carbohydrates by difference, Energy & SOP. 

##├  ) Fat - standardised ----

#Checking variable names:
names(data.df)

data.df <- nutri_combiner(data.df, 
                          "FATg","FAT_g", "FATCEg", "FAT_g_standardised")

# Adding Fat component names
var1 <- "FATg"
var2 <- "FAT_g"
var3 <- "FATCEg"
#data.df$comments <- NA #Uncomment if not found

# New variable (where to be stored) (check with documentation)
new_var <- "FAT_g_standardised"

# Checking that the changes are performed
names(data.df)
data.df[, c(var1, var2, var3, new_var, "comments")] #checking the new variable w/ other
dim(data.df) # same rows, one more column


##├ ) Fibre - standardised  ---- 

# No other fibre fractions available (See "QC.R")
fao_fish_fct$FIBTGg_std <- fao_fish_fct$FIBTGg 

data.df %>% filter()

##├ ) Ash - standardised  ---- 
# NOTE: Combining Tagnames (after back-calculating ASHDFg)

subset(fao_fish_fct, !is.na(ASHg)) %>% count(source_fct)
subset(fao_fish_fct, !is.na(ASHg_bydiff)) %>% count(source_fct)

fao_fish_fct$ASHg_std <- ifelse(is.na(fao_fish_fct$ASHg), fao_fish_fct$ASHg_bydiff, fao_fish_fct$ASHg )
fao_fish_fct$comment <- ifelse(!is.na(fao_fish_fct$ASHg_bydiff), "Ash values were calculated by difference, see documentation", NA)

subset(fao_fish_fct, is.na(ASHg_std)) %>% count(source_fct)

##├ ) Vitamin B6 - standardised  ----

#This loop combine all the Tagnames for VITB6

# Adding Fat component names
var1 <- "VITB6Amg"
var2 <- "VITB6Cmg"
var3 <- "VITB6_mg"
#data.df$comments <- NA #Uncomment if not found

# New variable (where to be stored) (check with documentation)
new_var <- "VITB6_mg_standardised"

test <- combiner(data.df, "VITB6Amg", 
                 "VITB6Cmg", "VITB6_mg", "VITB6_mg_standardised")

# Checking that the changes are performed
names(test)
test[, c(var1, var2, var3, new_var, "comments")] #checking the new variable w/ other
dim(data.df) # same rows, one more column



for(i in 1:nrow(fao_fish_fct)){
  print(i)
  if (!is.na(fao_fish_fct$VITB6Amg[i])) {
    print(!is.na(fao_fish_fct$VITB6Amg[i]))
    fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6Amg[i]
  }  
  if (is.na(fao_fish_fct$VITB6Amg[i])) { 
    fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6Cmg[i]
  } 
  if (is.na(fao_fish_fct$VITB6Amg[i]) & is.na(fao_fish_fct$VITB6Cmg[i])) {
    fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6_mg[i]
  }
  if (is.na(fao_fish_fct$VITB6Amg[i]) & is.na(fao_fish_fct$VITB6Cmg[i]) & is.na(fao_fish_fct$VITB6_mg[i])) {
    fao_fish_fct$VITB6_mg_standardised[i] <- NA
  }
  print(fao_fish_fct$VITB6_mg_standardised[i])
}

##├ )  Niacin - standardised  ----

fao_fish_fct <- fao_fish_fct %>% 
  nia_conversion_creator()


# Re-calculating variables ----

##├ ) Carbohydrates - standardised ----


fao_fish_fct  <- fao_fish_fct %>% CHOAVLDFg_std_creator()

##├├ Plot: Missing values for carbohydrates by difference in each FCT ----
fao_fish_fct[,c("CHOAVLDFg_std",  "source_fct")] %>%  #selecting variables
  naniar::gg_miss_fct(., fct = source_fct) #making the plot

##├├ Hist: Carbohydrates by difference ----
# before assumning zero of the negative values
hist(fao_fish_fct$CHOAVLDFg_std)

#No. negative value
sum(fao_fish_fct$CHOAVLDFg_std < 0)

#Checking negative values 
n1 <- which(fao_fish_fct$CHOAVLDFg_std< 0)

fao_fish_fct$comment[n1] <- ifelse(is.na(fao_fish_fct$comment[n1]),
                                   "CHOAVLDFg_std assumed zero", paste(fao_fish_fct$comment, "| CHOAVLDFg_std assumed zero") )
fao_fish_fct$CHOAVLDFg_std[fao_fish_fct$CHOAVLDFg_std< 0] <- 0


##├ ) Energy - standardised ----

# Energy in kcal
fao_fish_fct$ENERCkcal_std <- ENERCKcal_standardised(
  fao_fish_fct$PROCNTg,fao_fish_fct$FAT_g_standardised,
  fao_fish_fct$CHOAVLDFg_std, fao_fish_fct$FIBTGg_std,
  fao_fish_fct$ALCg)

# Energy in kJ
fao_fish_fct$ENERCkJ_std <-  ENERCKj_standardised(
  fao_fish_fct$PROCNTg, fao_fish_fct$FAT_g_standardised,
  fao_fish_fct$CHOAVLDFg_std, fao_fish_fct$FIBTGg_std,
  fao_fish_fct$ALCg)

##├ ) Sum of proximate  ----

fao_fish_fct  <- fao_fish_fct %>% SOP_std_creator() 

#├ ) Retinol - recalculated ---- 
# Back-calculating 

fao_fish_fct <- RETOLmcg_calculator(fao_fish_fct)


#├ ) Beta - Carotene eq. - standardised ---- 

fao_fish_fct <- fao_fish_fct %>% 
  CARTBEQmcg_std_creator() %>% # Calculate CARTBEQmcg & store it in CARTBEQmcg_std
  CARTBEQmcg_std_imputer_with_CARTBEQmcg() %>% # New imputer of CARTBEQmcg into CARTBEQmcg_std when they are NAs.
  CARTBEQmcg_std_back_calculator_VITA_RAEmcg() %>% # This requires values created in RETOLmcg_Recalculator
  CARTBEQmcg_std_to_zero()   # changing negative values to zero # This handles better and adds comments

#├ )  Vitamin A - standardised ----  

fao_fish_fct  <- fao_fish_fct %>%
VITA_RAEmcg_std_creator() %>%  # This function recalculate VITA_RAEmcg_std (standardised)
  VITAmcg_std_creator()   # This function recalculate VITAmcg_std (standardised)
  


#├ ) Thiamine - standardised  ----

fao_fish_fct  <- fao_fish_fct %>%
  THIAmg_std_creator() 

saveRDS(fao_fish_fct, here::here("data", "FAO-fish-harmonised_v1.0.0.RDS"))



