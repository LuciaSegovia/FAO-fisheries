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
#rm(list = ls())


# Loading libraries
## Note: if it is the first time: install.packages() first
library(dplyr) # For data cleaning (wrangling)
library(stringr) # For string manipulation (data cleaning)
library(measurements) # For unit conversion
library(gt) # For generating tables
source(here::here("functions.R")) # Loading nutrition functions (change to package when ready)


# Loading the data
# For reproducing, our updating the FAO-fisheries load data by
#Uncommenting and running the following
#source(here::here("merging_all.R"))
#data.df <- fao_fish_fct

data.df <- read.csv(here::here("Output", "template-name_FCT_FAO_Tags.csv"))

# 0) Check that we have all FCTs merged ----
data.df %>% 
  group_by(source_fct) %>% count()


# 3.4.2) Combining Tagnames  ----

# Note: Fat and Fibre are needed for recalculating:
# Carbohydrates by difference, Energy & SOP. 

##├  ) Fat - standardised ----


#This loop combine all the Tagnames for FAT_g_standardised
#Checking variable names:
names(data.df)

# Adding Fat component names
var1 <- "FATg"
var2 <- "FAT_g"
var3 <- "FATCEg"
#data.df$comments <- NA #Uncomment if not found

# New variable (where to be stored) (check with documentation)
new_var <- "FAT_g_standardised"
text <- paste0(new_var, " equals to ") #metadata in comments variable

# Loop that prioritise in the other of the variables defined above (1->3)
for(i in 1:nrow(data.df)){
  print(i)
  if (!is.na(data.df[i, var1])) {
    print(!is.na(data.df[i, var1]))
    data.df[i, new_var] <- data.df[i, var1]
    data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]), 
                                     paste0(data.df[i, "comments"], ";", text, var1), 
                                     paste0(text, var1))
    
    
  }  else if (is.na(data.df[i, var1]) & !is.na(data.df[i, var2])) { 
    data.df[i, new_var] <- data.df[i, var2]
    data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]), 
                                     paste0(data.df[i, "comments"], ";", text, var2), 
                                     paste0(text, var2))
    
  } 
  if (is.na(data.df[i, var1]) & is.na(data.df[i, var2]) & !is.na(data.df[i, var3])) {
    data.df[i, new_var] <- data.df[i, var3]
    data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]), 
                                     paste0(data.df[i, "comments"], ";", text, var3), 
                                     paste0(text, var3))
    
  }
  if (is.na(data.df[i, var1]) & is.na(data.df[i, var2]) & is.na(data.df[i, var3])) {
    data.df[i, new_var] <- NA
  }
  print(data.df[i, new_var])
}

# Checking that the changes are performed
names(data.df)
data.df[, c(var1, var2, var3, new_var, "comments")] #checking the new variable w/ other
dim(data.df) # same rows, one more column



##├ ) Fibre - standardised  ---- 

# No other fibre fractions available (See "QC.R")
fao_fish_fct$FIBTGg_std <- fao_fish_fct$FIBTGg 

##├ ) Ash - standardised  ---- 
##├ ) Vitamin B6 - standardised  ----

#This loop combine all the Tagnames for VITB6

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



# 3.4.3 ) Back calculation ----

##├ ) Refuse


##├ )  ASHg by difference (ASHDFg) ---- 

##├ ) Retinol - back-calculated ---- 

##├ ) Beta-Carotene eq. back-calculated ---- 

data.df <- data.df %>% 
  ASHDFg_calculator() %>%  # back-calculation & generation of new variable
  RETOLmcg_calculator()  %>%  # back-calculation & imputation in RETOLmcg
  CARTBEQmcg_backcalculator() # back-calculation & imputation in CARTBEQmcg_std
  
