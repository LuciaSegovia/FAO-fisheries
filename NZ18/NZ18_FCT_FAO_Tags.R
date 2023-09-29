library(tidyverse)



# Data Import ----

NZ18_Raw <- readxl::read_excel(here::here('NZ18', "Standard DATA.AP.xlsx"), skip = 1) #Reads the data excel document and assigns the relevant sheet to an R data frame, skipping the first row
NZ18_sources <- readxl::read_excel(here::here('NZ18', "Standard DATA.FT.xlsx"), skip = 1) #Reads the sources excel document and assigns the relevant sheet to an R data frame, skipping the first row


# Column Renaming (General) ----

column_names <- names(NZ18_Raw) # Creates a list of all column names
for (i in 1:length(column_names)){ #iterates through that list
  current_name <- names(NZ18_Raw[,i]) #takes the column name from the dataset which corresponds with the position in the list
  missing_unit <- toString(NZ18_Raw[1,i]) #Reads the first row of that column, where the units are for the data
  if(identical(current_name, missing_unit)){ #Metadata columns have the column name and first row identical; this checks for that and skips if true
    next
  }
  newname <- str_c(current_name, " (", missing_unit, ")") #A new name, combining the column name and the units from the first column is created
  colnames(NZ18_Raw)[i] <- newname #The new name is assigned
}
NZ18_int <- NZ18_Raw %>% #This removes the first row, which contained the units now merged into the column name
  slice(-1)



# Collating food data sources ----

#This chunk of code finds all data sources for nutritional information for each food item that has it, and creates a list of those sources, and adds it to the main table
#The key for these source can be found in the accompanying pdf, Appendix 2 (pg 57-58)

NZ18_int$nutrient_data_source <- NA #Creates a nutrient_data_source column
food_item_list <- unique(NZ18_sources$FoodID) #Creates a list of unique FoodID entries
for(fooditem in food_item_list){ #Loops through the entries in that list
  temp_table <- NZ18_sources %>% filter(FoodID == fooditem) #creates a temporary table comprising all rows with the food item code being checked in the loop
  source_string <- "" #creates/rewrites the source_string variable to nothing ("")
  for(u in 1:nrow(temp_table)){ #This loop iterates over each row in the temporary table
    source_string <- str_c(source_string, temp_table[u,4], sep = "") #And the source string is expanded on with each value from the 4th column of that temp table
  }
  unique_sources <- rawToChar(unique(charToRaw(source_string))) #The characters are seperated out, checked for uniqueness, and then the unique_sources string is created from that
  NZ18_int$nutrient_data_source[NZ18_int$FoodID == fooditem] <- paste(unique_sources, collapse = ", ") #For the FoodID in question, the unique sources for that item are applied to the nutrient_data_source column
}



# Renaming Food Group ----

NZ18_int$Chapter[NZ18_int$Chapter == "A"] <- "Bakery products" #Each row replaces a Chapter entry with the food group they correspond to, according to the FOODfiles supporting pdf, pg 8-9
NZ18_int$Chapter[NZ18_int$Chapter == "B"] <- "Beverages, alcoholic" #For example, this row replaces the Chapter entry "B" with "Beverages, alcholic"
NZ18_int$Chapter[NZ18_int$Chapter == "C"] <- "Beverages, non-alcoholic"
NZ18_int$Chapter[NZ18_int$Chapter == "D"] <- "Breakfast cereals"
NZ18_int$Chapter[NZ18_int$Chapter == "E"] <- "Cereals and pseudo-cereals"
NZ18_int$Chapter[NZ18_int$Chapter == "F"] <- "Dairy"
NZ18_int$Chapter[NZ18_int$Chapter == "G"] <- "Eggs"
NZ18_int$Chapter[NZ18_int$Chapter == "H"] <- "Fast foods"
NZ18_int$Chapter[NZ18_int$Chapter == "J"] <- "Fats and oils"
NZ18_int$Chapter[NZ18_int$Chapter == "K"] <- "Fin fishes"
NZ18_int$Chapter[NZ18_int$Chapter == "L"] <- "Fruits"
NZ18_int$Chapter[NZ18_int$Chapter == "M"] <- "Meats"
NZ18_int$Chapter[NZ18_int$Chapter == "N"] <- "Meat products"
NZ18_int$Chapter[NZ18_int$Chapter == "P"] <- "Miscellaneous"
NZ18_int$Chapter[NZ18_int$Chapter == "Q"] <- "Nuts and seeds"
NZ18_int$Chapter[NZ18_int$Chapter == "R"] <- "Recipes"
NZ18_int$Chapter[NZ18_int$Chapter == "S"] <- "Sauces"
NZ18_int$Chapter[NZ18_int$Chapter == "T"] <- "Shellfishes"
NZ18_int$Chapter[NZ18_int$Chapter == "U"] <- "Snack foods"
NZ18_int$Chapter[NZ18_int$Chapter == "V"] <- "Soups"
NZ18_int$Chapter[NZ18_int$Chapter == "W"] <- "Sugars, confectionaries and sweet spreads"
NZ18_int$Chapter[NZ18_int$Chapter == "X"] <- "Vegetables and pulses"
colnames(NZ18_int)[2] <- "food group" #The "Chapter" column is then renamed to "food group"



# Metadata column modification ----

NZ18_int$source_fct <- "NZ18" #the source_fct column is created and set to "NZ18"

Output_table <- NZ18_int %>%
  relocate(source_fct, .after = 'Food Name') %>% #the "source_fct" column is moved to after "Food Name"
  relocate('food group', .after = 'Food Name') %>% #the "food group" column is moved after "Food Name"
  relocate(nutrient_data_source, .after = source_fct) #the "nutrient_data_source" column is moved after "source_fct"



# Column Renaming (Manual) ----

col_tagname <- c("fdc_id", #A list of the desired column names in order. NA entries may be due to that column not having a corresponding FAO tagname, or other difficulties
                 "food_desc",
                 "food_group",
                 "source_fct", 
                 "nutrient_data_source", #change for nutrient_source / FEmg_source ?
                 "ALCg",
                 "CARTAmcg",
                 "TOCPHAmg",
                 "ASHg",
                 "CHOAVLDFg",
                 NA, 
                 "CHOAVLg",
                 "CHOAVLMg", 
                 "CARTBmcg",
                 "CARTBEQmcg",
                 "TOCPHBmg", 
                 NA, 
                 "CAmg", 
                 NA,
                 "CHOLEmg",
                 "CUmg",
                 "TOCPHDmg",
                 "FOLDFEmcg",
                 NA, 
                 "Edible_factor_in_FCT",
                 NA,
                 "ENERCkcal", 
                 NA, 
                 "ENERCkJ", 
                 NA,
                 NA,
                 NA,
                 NA, 
                 "FATg", 
                 NA, 
                 "F20D5N3g", 
                 NA, 
                 "F22D6N3g",
                 NA,
                 NA, 
                 NA, 
                 "FAMSg", 
                 "FAPUg", 
                 NA, 
                 NA, 
                 "FASATg", 
                 "FATRNg", 
                 "FIBTGg", 
                 "FIBINSg", 
                 "FIBSOLg", 
                 "FOLFDmg",
                 "FOLmg", 
                 "FOLACmg", 
                 NA, 
                 "TOCPHGmg", 
                 NA, 
                 "IDmcg",
                 "FEmg", 
                 NA,
                 "MGmg", 
                 NA, 
                 "MNmcg",
                 "NIATRPmg",
                 "NIAEQmg", 
                 "NIAmg", 
                 "NTg", 
                 "Pmg", 
                 "Kmg", 
                 "PROCNTg",
                 "RETOLmcg",
                 "RIBFmg", 
                 "SEmcg", 
                 "NAmg", 
                 "STARCHg", 
                 "SUCSg",  
                 "SUGADg", 
                 NA, 
                 "SUGARg", 
                 "THIAmg", 
                 "CHOCDFg", 
                 "CHOCSMg", 
                 "TRPmg", 
                 "VITA_RAEmcg", 
                 "VITAmcg",
                 "VITB12mcg",
                 "VITB6Amg",
                 "VITCmcg",
                 "VITDmcg", 
                 "VITEmg",
                 "WATERg", 
                 "ZNmg")

for (i in 1:length(Output_table)){ #Loops through all columns
  if (!is.na(col_tagname[i])){ #Checks ff the corresponding new column name in col_tagname is NA or not
    colnames(Output_table)[i] <- col_tagname[i] #If it isn't NA, the column name is replaced with the new column name from col_tagname
  }
}



# Column Renaming (Manual) ----

Output_table$Edible_factor_in_FCT <- as.numeric(Output_table$Edible_factor_in_FCT)/100 #Converts the percentage value to a factor


#Optional - check the data before saving
glimpse(Output_table)

# Data Output ----

write.csv(Output_table, file = here::here("Output", "NZ18_FCT_FAO_Tags.csv"),
          row.names = FALSE) #Saves the newly-created data table to the Output folder 

#Run this to clean the environment
rm(list = ls())