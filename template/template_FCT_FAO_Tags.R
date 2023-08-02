#
# Template for cleaning and standardising FCTs from diverse sources
# 
# More details can be found in the documentation. 
#
# Note: Most of the optional steps are commented out, to be run
# remove the "#" adjust for your dataset structure and need and run
#
# 0. Obtaining the raw (FCT) file(s) for importing
# # Check licensing conditions & record the data source (see README)
#
##Run this to clean the environment
#rm(list = ls())


# Loading libraries
## Note: if it is the first time: install.packages() first
library(dplyr) # For data cleaning (wrangling)
library(stringr) # For string manipulation (data cleaning)
library(measurements) # For unit conversion
source(here::here("functions.R")) # Loading nutrition functions (change to package when ready)



# 1. Importing the data (loading the data)
## What kind of file is? E.g.: xlsx (readxl::read_excel)
#Reads the excel document 
data.df <- readxl::read_excel(here::here('template', # Change to your folder name
                                'template-file-name.xlsx'), # Change to your file name
                              sheet = 5  # Change to excel sheet where the FCT is stored in the excel file
                              ) %>%  
  mutate(source_fct = 'template-xx')  # Creates the source_fct column and fills with "location-year" of FCT 


# Checking the loaded data
## How many rows & columns have the data?
dim(data.df) # rows & columns

## What are the variables names? 
names(data.df)

# We are happy that we have loaded the correct FCT file

# 2. Tidying the data  ----

## 2.1.	Formatting FCT into a tabular format  ----
### Visually checking the data

head(data.df) # Checking the first rows and columns
tail(data.df) # Checking the last rows and columns
#View(data.df) # Seeing the dataframe in a tab, need to comment out for using it, don't run it if the dataset is very very big.

### Trimming the dataframe horizontally

#data.df <- data.df %>% slice(1:300) # Removing the last row, if needed adjust for your dataset

### 2.1.1. Creating food_groups variable and tidying ----

# Extracting food group names
fgroup <- data.df %>% filter(is.na(food_desc), !is.na(fdc_id)) %>% pull(fdc_id) %>%
  stringr::str_split_fixed( '/', n = 2) %>% as_tibble() %>% pull(V1) #Creates a list of the food groups using their unique row structure in the table to identify them

# Creating the food_group variable in the FCT
data.df <- data.df %>% #Identifies the food group number from the fdc_id, and applies the correct food_group from the fgroup list to the food_group column
  mutate(food_group =
    ifelse(grepl("01_", fdc_id), fgroup[1],
    ifelse(grepl("02_", fdc_id), fgroup[2],
    ifelse(grepl("03_", fdc_id), fgroup[3], 
    ifelse(grepl("04_", fdc_id), fgroup[4], 
    ifelse(grepl("05_", fdc_id), fgroup[5],
    ifelse(grepl("06_", fdc_id), fgroup[6],
    ifelse(grepl("07_", fdc_id), fgroup[7],
    ifelse(grepl("08_", fdc_id), fgroup[8],
    ifelse(grepl("09_", fdc_id), fgroup[9], 
    ifelse(grepl("10_", fdc_id), fgroup[10], 
    ifelse(grepl("11_", fdc_id), fgroup[11],
    ifelse(grepl("12_", fdc_id), fgroup[12],     
    ifelse(grepl("13_", fdc_id), fgroup[13],
    ifelse(grepl("14_", fdc_id), fgroup[14],
    'NA'))))))))))))))) %>% 
  filter(!is.na(food_desc)) # Removes any rows without a food description entry (the food group name rows, and a row that have already been used for naming)

# Checking changes in the data structure
data.df %>% filter(is.na(food_desc), # 
                   !is.na(fdc_id))
str(data.df) # Checking columns: 1 variable per column, no empty rows.
head(dta.df)
dim(data.df) # rows and column # Note: it should have less rows (from trimming) and one more column (food_group)

### 2.1.2. Diving combined variables into two (or more) columns ----

# This is just an example!
data.df <- data.frame(a = c("x1", "x2", "x3", "x4"), 
                      b = c("399", "[899]", "[5000]", "3000"), 
                      b1 = c("399", "450*", NA, "3000"), 
                      c = c("399", "899", "[5000]", "3000"),
                      d = c("LOD", "<lod", "[5000]", "tr"))

data.df <- data.df %>% 
  mutate(e = str_extract(b, '(?<=\\[).*?(?=\\])'),  #Creating calculated values from the lower quality method and removing the original values from the original variable
         b1 = ifelse(is.na(b1), str_extract(b, '(?<=\\[).*?(?=\\])'), b1)) 


# Separating variables: 4 new variables, 3 existing one
data.df <- data.df %>% 
  mutate(FATCEg = str_extract(FATg, '(?<=\\[).*?(?=\\])'),  
         FIBCg =  str_extract(FIBTGg, '(?<=\\[).*?(?=\\])'), 
         CARTBmcg = ifelse(is.na(CARTBmcg), str_extract(CARTBEQmcg, '(?<=\\[).*?(?=\\])'), CARTBmcg), 
         TOCPHAmg = ifelse(is.na(TOCPHAmg),str_extract(VITEmg, '(?<=\\[).*?(?=\\])'), TOCPHAmg ),
         NIAmg = ifelse(is.na(NIAmg), str_extract(NIAEQmg, '(?<=\\[).*?(?=\\])'), NIAmg), 
         FOLSUMmcg = str_extract(FOLmcg, '(?<=\\[).*?(?=\\])'), 
         PHYTCPPD_PHYTCPPImg = str_extract(PHYTCPPmg, '(?<=\\[).*?(?=\\])'))

# Checking changes in the data structure
names(data.df) # Check variable names (are the new variables there?)
dim(data.df) # rows and column # Note: same rows and 4 more column (4 new variables)
str(data.df) # Checking columns: 1 variable per column, no empty rows.


## 2.2.	Renaming variables  ----
# Checking variables names
names(data.df) # Are the variable names = column names? 
#If not, more formatting is needed (back to previous step)

#If yes, 
# are the food component variable names using INFOODS tagnames & units
# e.g. [Variable]_[unit] (NA_mg)? Then, rename other variables:


# If not, do they provide INFOODS tagname information? 
# If yes, use that information to rename food components

# Automatic renaming of INFOODS tagnames & units
#for( i in 8:62){ #Loops through each column between column 8 and 64 - this is specific for each dataset!!
  first_row <- toString(names(data.df)[i]) #Takes the column names and assigns it to a variable (name & unit)
  second_row <- toString(data.df[1, i]) #Takes the first row for that column and assigns it to a variable (tagname)
  split_string <- str_split(first_row, "\\(") #Splits the first row around "(", assigning the two resulting strings to a variable (separating units from names)
  units_int <- gsub("\\*|\\(|\\)", "", split_string[[1]][length(split_string[[1]])]) #Separates the units out from the split_string (everything after the last open bracket) (getting units)
  names(data.df)[i] <- paste0(second_row, units_int) #The column name is replaced with row 1 and the units from row 2 (changing old column name w/ new tagname_unit names)
} # This loops takes units from variable (column name) extract units & combine it with the INFOODS from 
# The next row

# If not, then, manually rename each food component with their correct tagname & current unit
# Change the names in quotes ("") to those in your dataset, remove/ add as needed 

data.df <- data.df %>%  
  dplyr::rename(
  ENERCkJ = "kilojoules", 
  ENERCkcal = "kilocalories", 
  WATERg = "water", 
  FAT_g = "fat", 
  FASATg = "sat_fa",
  FAMSg = "mu_fa",
  FAPUg = "pu_fa",
  F22D6N3g = "c22_6n_3_dha", 
  F20D5N3g = "c20_5n_3_epa", 
  CHOLEmg = "cholesterol",
  CHOAVLg = "carbo", 
  SUGARg = "sugar", 
  FIBTGg = "dietary_fibre", 
  PROCNTg = "protein",
  ALCg = "alcohol", 
  VITA_RAEmcg = "vitamin_a", 
  RETOLmcg = "retinol",
  CARTBmcg = "beta_carotene",
  VITDmcg = "vitamin_d", 
  VITEmg = "vitamin_e", 
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
  ZNmg = "zinc")

# Renaming the other variables (food code/id, food name, etc.)
data.df <- data.df %>%  
  dplyr::rename(
  fdc_id = "food_id",  # Food id/code
  food_desc = "food_item", # Food name/description
  Edible_factor_in_FCT = "edible_part", # Edible portion
  nutrient_data_source = "biblio") # Reference for NVs reported

## What are the variables names now?
names(data.df) # Checking variable names (are all correctly named?)
head(data)

## 2.3.	Standardisation of values  ----
# Are there any character on the numeric variables (e.g., "tr" in CAmg)?
                      
data.df$comments <- NA # New column to add metadata (e.g., when [] are removed) for data users

# Adding metadata info to the variables before removing the characters
#fdc_id <- "a" # Variable with the food id/code to be added into the metadata

 data.df <- data.df %>% 
   mutate(comments = ifelse(stringr::str_detect(. , '\\[.*?\\]'), 
   paste0("low quality(", toString(.[stringr::str_which(. , '\\[.*?\\]'), fdc_id], " ", 
    names(.)[stringr::str_which(., '\\[.*?\\]')], ")"), comments)))
   
data.df$comments <- ifelse(stringr::str_detect(data.df[,i] , '\\[.*?\\]'), 
                      paste0("low quality(", toString(.[stringr::str_which(. , '\\[.*?\\]'), fdc_id], " ", 
                 names(.)[stringr::str_which(., '\\[.*?\\]')], ")"), comments))

# Replacing character with numeric-like values (e.g trace = 0, missing values = NA)
# Using a function - check what kind of character-like values are
#Function to remove brackets and replace trace to zero
#The following f(x) removes [] and changing tr w/ 0

variables <- names(data.df)[2:6] # Specify the NVs columns

data.df <- data.df %>% 
  mutate_at(variables, no_brackets_tr)   #This applies the function for removing brackets

# Check that there are no more characters
  # Currently, only works for presence of character strings, [], and * 
#NOTE: we are using only NVs variables as character are found in non-numeric variables (i.e., food_desc)

data.df[, variables][grepl("[:alpha:]|\\[|\\*", data.df[, variables])] 

# This steps only can be run after before step the previous step
  data.df <- data.df %>% 
  mutate_at(variables, as.numeric) # This convert all NVs into numeric 


# 2.4.	Units of measurements
# The next step is to check whether each variable (food component) has the correct units
# Some variable may be in non-standard units: Eg. Iron from mcg to mg
            
variable <- c("b", "c") # Name the variable(s) that you need to change

data.df[, variable] <- data.df[, variable]/1000 

# Also can use conv_unit()

#Then, other might be also a different denominator: 
# Amino acid in g/100g of protein to mg or EP.

#Unit conversion and in new column of Amino Acids
# multiplying AA*Protein/10 (Eq.2.3)
aa_prot <- grep( "_100gPROTCNT", names(data.df), value = TRUE) # Getting AA variables
aa_mg <- gsub("g_100gPROTCNT", "mg", aa_prot) # Getting the new variable name [NV+unit]

data.df[, aa_mg] <- data.df[, aa_prot]*data.df$PROCNTg*10 # Generating the new AA variable



# 2.5. Data output for harmonisation ----

# Once all these steps are done: The FCT is clean & standardise 
# And it's ready to be merge, exported, and/or, be used w/ the other scripts (harmonisation)

write.csv(data.df, file = here::here("Output", "template-name_FCT_FAO_Tags.csv"), # Change according to the FCT file
          row.names = FALSE) #Saves the newly-created data table to the Output folder

#Run this to clean the environment
rm(list = ls())

