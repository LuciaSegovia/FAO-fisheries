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
##Run this to clean the environment, aside from fct_cover
variables_list <- ls()
kept_variables <- "fct_cover"
remove_list <- variables_list[!variables_list %in% kept_variables]
rm(list = remove_list)


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

if(!file.exists("data/FAO-fish-standardised-updated_v1.1.0.RDS")){
  stop("Dependency file missing. Please run merging_all.R, ensuring that the final line is not commented out.")
}

data.df <- readRDS(here::here("data", "FAO-fish-standardised-updated_v1.1.0.RDS"))
#data.df <- read.csv(here::here("output", "template-name_FCT_FAO_Tags.csv"))
  
# 0) Check that we have all FCTs merged ----
data.df %>% 
  group_by(source_fct) %>% count()


# 3.6.2 ) Combining Tagnames to generate variables ----

# Note: Fat and Fibre are needed for recalculating:
# Carbohydrates by difference, Energy & SOP. 

##├  ) Fat - standardised ----

#Checking variable names:
names(data.df)

data.df <- NutritionTools::nutri_combiner(data.df, 
                          "FATg","FAT_g", "FATCEg", new_var = "FAT_g", fill_missing = TRUE)

# Adding Fat component names
variables_Fat <- c("FATg", "FAT_g", "FATCEg")
#data.df$comments <- NA #Uncomment if not found

#Only pulls ones which are found in the df
variables_Fat <- variables_Fat[variables_Fat %in% colnames(data.df)]

# New variable (where to be stored) (check with documentation)
new_fat_var <- "FAT_g_combined"

# Checking that the changes are performed
names(data.df)
data.df[, c(variables_Fat, new_fat_var, "comments")] #checking the new variable w/ other
dim(data.df) # same rows, one more column


##├ ) Fibre - standardised  ---- 

# No other fibre fractions available (See "QC.R")
data.df$FIBTGg_std <- data.df$FIBTGg 

data.df %>% filter()

##├ ) Ash - standardised  ---- 
# NOTE: Combining Tagnames (after back-calculating ASHDFg)
# 
# subset(fao_fish_fct, !is.na(ASHg)) %>% count(source_fct)
# subset(fao_fish_fct, !is.na(ASHg_bydiff)) %>% count(source_fct)
# 
# fao_fish_fct$ASHg_std <- ifelse(is.na(fao_fish_fct$ASHg), fao_fish_fct$ASHg_bydiff, fao_fish_fct$ASHg )
# fao_fish_fct$comment <- ifelse(!is.na(fao_fish_fct$ASHg_bydiff), "Ash values were calculated by difference, see documentation", NA)
# 
# subset(fao_fish_fct, is.na(ASHg_std)) %>% count(source_fct)

##├ ) Vitamin B6 - standardised  ----

#This loop combine all the Tagnames for VITB6

# Adding component names

variables_vitB6 <- c("VITB6Amg", "VITB6Cmg", "VITB6_mg")


#Only pulls ones which are found in the df
variables_vitB6 <- variables_vitB6[variables_vitB6 %in% colnames(data.df)]

#data.df$comments <- NA #Uncomment if not found

# New variable (where to be stored) (check with documentation)
new_Vitb6_var <- "VITB6_mg_combined"

data.df <- NutritionTools::nutri_combiner(data.df, 
                                          "VITB6Amg", 
                                          "VITB6Cmg", "VITB6_mg", new_var = "VITB6_mg", fill_missing = TRUE)

# Checking that the changes are performed
names(data.df)
data.df[, c(variables_vitB6, new_Vitb6_var, "comments")] #checking the new variable w/ other
dim(data.df) # same rows, one more column



# for(i in 1:nrow(fao_fish_fct)){
#   print(i)
#   if (!is.na(fao_fish_fct$VITB6Amg[i])) {
#     print(!is.na(fao_fish_fct$VITB6Amg[i]))
#     fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6Amg[i]
#   }  
#   if (is.na(fao_fish_fct$VITB6Amg[i])) { 
#     fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6Cmg[i]
#   } 
#   if (is.na(fao_fish_fct$VITB6Amg[i]) & is.na(fao_fish_fct$VITB6Cmg[i])) {
#     fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6_mg[i]
#   }
#   if (is.na(fao_fish_fct$VITB6Amg[i]) & is.na(fao_fish_fct$VITB6Cmg[i]) & is.na(fao_fish_fct$VITB6_mg[i])) {
#     fao_fish_fct$VITB6_mg_standardised[i] <- NA
#   }
#   print(fao_fish_fct$VITB6_mg_standardised[i])
# }


##├ )  Niacin - standardised  ----

data.df <- data.df %>% 
  NIAmg_calc_combiner()


# Re-calculating variables ----

##├ ) Carbohydrates - standardised ----
# CHOAVLDFg_std

# data.df  <- data.df %>%
#   CHOAVLDFg_std_creator()

data.df$WATERg <- as.numeric(data.df$WATERg)

data.df <- data.df %>% CHOAVLDFg_calculator(., FIBTGg_combined_column = "FIBTGg_std")

##├├ Plot: Missing values for carbohydrates by difference in each FCT ----
data.df[,c("CHOAVLDFg_calculated",  "source_fct")] %>%  #selecting variables
  naniar::gg_miss_fct(., fct = source_fct) #making the plot

##├├ Hist: Carbohydrates by difference ----
# before assuming zero of the negative values
hist(data.df$CHOAVLDFg_calculated)

#No. negative value
sum(data.df$CHOAVLDFg_calculated < 0)

# Checking negative outside range (-5g)
sum(data.df$CHOAVLDFg_calculated < -5)
unique(data.df$fdc_id[data.df$CHOAVLDFg_calculated < -5])
data.df$source_fct[data.df$CHOAVLDFg_calculated < -5]
data.df$CHOAVLDFg_calculated[data.df$CHOAVLDFg_calculated < -5]
# QUALITY ASSURANCE
# Marking FOOD ENTRIES  ----
# Due to CHOAVLDFg_calculated < -5
n1 <- which(data.df$CHOAVLDFg_calculated < -5)
comment <- "Food entry to be excluded: CHOAVLDFg_calculated below - 5."

data.df$comments[n1] <- ifelse(is.na(data.df$comments[n1]), comment,
                               paste(comment, "; ", data.df$comments))


# Checking the items
#View(unique(subset(data.df,  fdc_id %in% unique(data.df$fdc_id[data.df$CHOAVLDFg_standardised < -5]))))

#Perc. of negative values
sum(data.df$CHOAVLDFg_calculated < 0)/nrow(data.df)*100

#Checking negative values 
n1 <- which(data.df$CHOAVLDFg_calculated< 0)

data.df$comments[n1] <- ifelse(is.na(data.df$comments[n1]),
                                   "CHOAVLDFg_calculated assumed zero", paste(data.df$comments, "; CHOAVLDFg_calculated assumed zero") )
data.df$CHOAVLDFg_calculated[data.df$CHOAVLDFg_calculated < 0] <- 0


##├ ) Energy - standardised ----

# Energy in kcal
data.df$ENERCkcal_std <- ENERCKcal_standardised(
  data.df$PROCNTg,data.df$FAT_g_combined,
  data.df$CHOAVLDFg_calculated, data.df$FIBTGg_std,
  data.df$ALCg)

# Energy in kJ
data.df$ENERCkJ_std <-  ENERCKj_standardised(
  data.df$PROCNTg, data.df$FAT_g_combined,
  data.df$CHOAVLDFg_calculated, data.df$FIBTGg_std,
  data.df$ALCg)

##├ ) Sum of proximate  ----

data.df  <- data.df %>% SOPg_calculator(FIBTGg_combined_column = "FIBTGg_std") 

# Back-calculating ----
#├ ) Retinol - recalculated ---- 

data.df <- RETOLmcg_calculator(data.df)

#├ ) Beta - Carotene eq. - standardised ---- 

data.df <- data.df %>% 
  CARTBEQmcg_std_creator() %>% # Calculate CARTBEQmcg & store it in CARTBEQmcg_std
  CARTBEQmcg_std_imputer_with_CARTBEQmcg() %>% # New imputer of CARTBEQmcg into CARTBEQmcg_std when they are NAs.
  CARTBEQmcg_backcalculator() %>% # This requires values created in RETOLmcg_Recalculator
  CARTBEQmcg_std_to_zero()   # changing negative values to zero # This handles better and adds comments


#├ )  Vitamin A - standardised ----  

data.df  <- data.df %>% 
VITA_RAEmcg_std_creator() %>%  # This function recalculate VITA_RAEmcg_std (standardised)
  VITAmcg_std_creator()   # This function recalculate VITAmcg_std (standardised)
  


#├ ) Thiamine - standardised  ----

if("THIAHCLmg" %in% colnames(data.df)){ #Only allows this step if THIAHCL, which only comes from certain FCT's, is in the dataframe
  
  data.df  <- data.df %>%
    THIAmg_combiner() 
}


# Saving the results of the harmnonised dataset

saveRDS(data.df, here::here("data", "FAO-fish-harmonised_v1.1.0.RDS"))





