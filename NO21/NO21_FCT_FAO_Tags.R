#                                                                             # 
#                                                                             # 
#                           FAO - Fisheries                                   # 
#                       Processing Norwegian FCDB                             #     
#                                                                             #
#                                                                             #
#                                                                             #
###############################################################################


##Run this to clean the environment
#rm(list = ls())


# Loading libraries
## Note: if it is the first time: install.packages() first
library(dplyr) # For data cleaning (wrangling)
library(stringr) # For string manipulation (data cleaning)
library(measurements) # For unit conversion
source(here::here("functions.R")) # Loading nutrition functions (change to package when ready)

# 0. Obtaining the raw (FCT) file(s) and other data needed (only run first time)
# # Check licensing conditions & record the data source (see README)
#
# Here's the link to the online file
# f <- "https://www.matportalen.no/verktoy/the_norwegian_food_composition_table/article43472.ece/BINARY/The%20Norwegian%20Food%20Composition%20Table%202022%20(xlsx)"
# 
# download.file(f,  # the location where is downloaded from
#              destfile = here::here('NO21',
#                                    "NorwegianFCT.xlsx"), # the location where is stored in your terminal
#             method="wininet", # use "curl" for OS X / Linux, "wininet" for Windows
#             mode="wb")

# cleaning scientific names
#source(here::here("NO21", "NO21_scientific-names.R"))

# 1. Data Import ----

#├  FAO data - ICS codes,  ISSCAAP groups & product type info  ----

#loading the data
ics_code <- readRDS(here::here("data", "ics-code.RDS"))


#├  Norwegian FCDB data (NO21) ----

#reading excel
readxl::excel_sheets(here::here( "NO21",
                                 "NorwegianFCT.xlsx"))

#Importing the data
no21 <- readxl::read_excel(here::here("NO21",
                                      "NorwegianFCT.xlsx"),
                           sheet = "Foods", skip = 2) %>% 
  janitor::clean_names() 

# Checking variable names
names(no21)

# Checking variables removing ref.
grep("ref", names(no21), value = TRUE, invert = TRUE)


#├  NO21 - Scientific names ----

# Loading the data
sci_no21 <- readRDS(here::here("NO21", 
                               "scientific-name_NO21.RDS"))
                              
#  2.	Cleaning and standardising FCT   ----

#├  2.1	Formatting FCT   ----

## ├ 2.1.1 Food groups ----

# Getting the food groups
# foodgroup <- no21 %>% filter(is.na(edible_part) & grepl("^\\d{1,2}$", food_id)) %>% .[,1:2]


no21 %>% filter(is.na(edible_part) & !is.na(food_id)) %>% .[,1:2] %>% View()

# String with the locations of the empty rows (separating the food groups and subgroups)
rows <- which(is.na(no21$food_item))
which(!is.na(no21$edible_part))

n <- 3

no21$food_item[n+1]
no21$food_id[n+1]

no21$food_id[1:20]

# Creating the variable for food groups and subgroups
no21$foodgroup <- NA
no21$food_subgroup <- NA
no21$food_subgroup2 <- NA

# Loop to allocate the food groups

j <- 0
foodgroups <- NA
f <- 0
food_subgroups <- NA

for(i in 1:length(rows)){

if(grepl("^\\d{1,2}$", no21$food_id[rows[i]+1])){
  
  j <- j+1
  foodgroups[j] <- no21$food_item[rows[i]+1]
  no21$foodgroup[rows[i]:rows[i+1]] <- no21$food_item[rows[i]+1]

  
}

if(grepl("^\\d{1,2}\\.\\d{1,2}$", no21$food_id[rows[i]+1])){
  
  f <- f+1
  food_subgroups[f] <- no21$food_item[rows[i]+1]
  no21$food_subgroup[rows[i]:rows[i+1]]  <- no21$food_item[rows[i]+1]
  no21$foodgroup[rows[i]:rows[i+1]]  <- foodgroups[[j]]
  
}
  
  if(grepl("^\\d{1,2}\\.\\d{1,2}\\.", no21$food_id[rows[i]+1])){
    
    no21$food_subgroup2[rows[i]:rows[i+1]]  <- no21$food_item[rows[i]+1]
    no21$foodgroup[rows[i]:rows[i+1]]  <- foodgroups[[j]]
    no21$food_subgroup[rows[i]:rows[i+1]]  <- food_subgroups[[f]]
    
  }
  
  
}

no21 %>% filter(is.na(food_subgroup) & 
                  is.na(foodgroup)) %>% View()

no21[which(is.na(no21$food_subgroup)),] %>% View()

unique(no21$food_subgroup)
unique(no21$foodgroup)


data.df <- no21

names(data.df)

#Checking values with food entry id.
data.df %>% filter(!is.na(food_id)) 
data.df %>% filter(is.na(kilojoules))

# Removing empty rows

data.df  <- data.df %>% filter(!is.na(food_id) & !is.na(food_item))

sum(is.na(data.df$foodgroup))

data.df %>% filter(is.na(foodgroup))

## ├ 2.1.2 Scientific name ----

names(sci_no21)

data.df <- data.df %>% 
  left_join(., sci_no21 %>% select(fdc_id, Scientific_name),
                      by = c("food_id" = "fdc_id"))

# Checking the data
data.df %>% filter(foodgroup %in% foodgroups[4]) %>% View()

#├  2.2 Renaming variables ----

data.df <- data.df %>%  rename(
  fdc_id = "food_id",
  food_desc = "food_item",
  scientific_name = "Scientific_name", 
  Edible_factor_in_FCT = "edible_part", 
  ENERCkJ = "kilojoules", 
  ENERCkcal = "kilocalories", 
  WATERg = "water", 
  FAT_g = "fat", 
  FASATg = "sat_fa",
  FAMSg = "mu_fa",
  FAPUg = "pu_fa",
  FATRNg = "trans_fa", 
  CHOLEmg = "cholesterol",
  CHOAVLg = "carbo", #if imputed it can be CHOAVLDFg
  SUGARg = "sugar", 
  STARCHg = "starch",
  FIBTGg = "dietary_fibre", 
  PROCNTg = "protein",
  ALCg = "alcohol", 
  VITA_RAEmcg = "vitamin_a", 
  RETOLmcg = "retinol",
  CARTBmcg = "beta_carotene",
  VITDmcg = "vitamin_d", # not well-defined in the documentation
  VITEmg = "vitamin_e", # expressed in alpha-tocopherol eq.
  THIAmg ="thiamin",
  RIBFmg = "riboflavin",
  NIAmg = "niacin", 
  VITB6_mg = "vitamin_b6",
  FOLmcg = "folate", 
  VITB12mcg = "vitamin_b12", 
  VITCmg = "vitamin_c", 
  CAmg = "calcium",
  FEmg = "iron",
  NAmg = "sodium", 
  Kmg = "potassium", 
  MGmg = "magnesium",
  Pmg = "phosphorus", 
  SEmcg = "selenium", 
  CUmg = "copper", 
  IDmcg = "iodine",
  ZNmg = "zinc") %>% 
  relocate(c(foodgroup, food_subgroup), .after = food_desc) #relocating food group variable

# Renaming Fatty Acids (FA)

grep("^c\\d", names(data.df), value = TRUE) # Checking FA names
data.df[1, grep("^c\\d", names(data.df))] # Checking FA units
# Generating new FA Tagnames (pattern)
fatty <- grep("^c\\d", names(data.df), value = TRUE) # creating a vector w/ FA names
fatty <- gsub("c", "F", fatty) # Changing "c" to "F (No of C)
fatty <- sub("_", "D", fatty) # Changing first "_" to "D" 
fatty <- sub("_[[:alpha:]]{2,}", "", fatty) # Removing short name for FA
fatty <- sub("n_", "N", fatty) # Changing "n_" to "N"
fatty <- paste0(fatty, "g") # adding the unit "g"

names(data.df)[grep("^c\\d", names(data.df))] <- fatty # Actual renaming

# Checking new names 
names(data.df)


#The Norwegian FCDB - remove # if willing to have the whole dataset.
#no21_fct <- no21 %>% filter(!is.na(ENERCkcal)) %>% slice (-1)

## ├ 2.1.2 Metadata ----

# Extracting metadata into a new datase, two dataset:
# 1) Nutrient values (NO21)
# 2) References for each fish and NV (NO21_biblio) 

dim(data.df)

# Getting the location of the first component with a reference value
n <- which(grepl("ref", names(data.df)))[1]-1
names(data.df)[n:ncol(data.df)] # Checking

# Generating a new dataset to store the references
NO_FCT_Sources <- data.df

# Loop to change ref.names for its component name
for(i in n:ncol(data.df)){
  colname <- colnames(NO_FCT_Sources)[i]
  if(grepl("ref", colname)){
    new_colname <- colnames(NO_FCT_Sources)[i-1]
    colnames(NO_FCT_Sources)[i-1] <- paste0(colname, "_TO_DELETE")
    colnames(NO_FCT_Sources)[i] <- new_colname
  }
}

names(NO_FCT_Sources)

# Removing NVs from the metadata (reference dataset)
NO_FCT_Sources <- NO_FCT_Sources[, !grepl("_TO_DELETE", names(NO_FCT_Sources))]

# Saving the NO21 biblio (ref metadata)
NO_FCT_Sources %>% filter(!is.na(fdc_id) & !is.na(WATERg)) %>% # (optional) removing empty rows due to food groups
  write.csv(., file = here::here("NO21", "NO21-biblio.csv"),
            row.names = FALSE)

# Removing metadata (ref columns) from the NO21
data.df <- data.df[, !grepl("ref", names(data.df))]

# Adding info on where to fine the metadata to the table
data.df$nutrient_data_source <-  "Information on the source of each value can be found in NO21-biblio.csv "

# Adding the FCT code for identification when combined w/ other FCTs
data.df$source_fct <- "NO21"

#├  2.3 Standardising of values ----

str(data.df)

# Check that there are no more characters
# Currently, only works for presence of character strings, [], and * 
#NOTE: we are using only NVs variables as character are found in non-numeric variables (i.e., food_desc)

variables <- grep("^[[:upper:]]+",  names(data.df), value = TRUE)  # Specify the NVs columns
data.df[, variables][grepl("[:alpha:]|\\[|\\*", data.df[, variables])] 
data.df[, variables][grepl("M", data.df[, variables])]  # M is the code for missing values (M == NA)
grep("M|-", data.df$F12D0g, value = TRUE) # From scientific notation

sum(is.na(data.df$F12D0g)) # 96
sum(is.na(data.df$Edible_factor_in_FCT) | data.df$Edible_factor_in_FCT == "M") #88 NA +M 140
sum(is.na(data.df$Edible_factor_in_FCT)) #88 NA +M 140

# Changing M to NA
data.df$Edible_factor_in_FCT[data.df$Edible_factor_in_FCT == "M"] <-  NA

# Changing food components to numeric
data.df[, variables] <- apply(data.df[, variables], 2, as.numeric)

#├  2.4 Unit of measurement  ----

# Converting EP into a fraction 
data.df$Edible_factor_in_FCT <- data.df$Edible_factor_in_FCT/100


# 2.5. Data output for harmonisation ----

# Saving the NO21, we kept all fishery product (w/ and w/o ICS code)
data.df %>% 
  #select(fdc_id:IDmcg, alpha_code, source_fct ) %>% 
  write.csv(., file = here::here("output", "NO21_FCT_FAO_Tags.csv"),
            row.names = FALSE)

# Removing object// Cleaning environment - do so if running the script on its own
#rm(list = ls())
