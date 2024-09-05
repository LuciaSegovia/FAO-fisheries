library(tidyverse)



# Data Import ----

excel_sheets <- readxl::excel_sheets(here::here('BA13', "BangladeshFCT.xlsx")) #creates a list of all the different sheets from the excel document

for (i in 5:10){ #This for loop creates an R data-frame for each sheet in the document, assigning each df the name of the sheet it comes from
  newsheet <- readxl::read_excel(here::here( 'BA13', "BangladeshFCT.xlsx"), sheet = i)
  assign(paste0(excel_sheets[i]), newsheet)
}



# Column Creation and relocation ----

BA13_Main_Table <- UserDB_Main_table[!is.na(UserDB_Main_table$`Foodname in English`),] %>% # Weeds out all the statistical rows (sample number/ SD) 
  mutate("Food Group" = NA, #Creates columns that are filled with NA, and then populated later on in the script
         "ENERCkJ" = NA,
         "FIBTGg" = NA,
         "FIBCg" = NA,
         "TOCHPAmg" = NA,
         "source_fct" = "BA13") %>% #creates the source_fct column, and populates it with "BA13"
  relocate(`Food Group`, .after = `Foodname in English`) %>% #These relocates move columns around to be closer to other relevant columns, for easier reference and checking
  relocate(`ENERCkJ`, .after = `ENERC (kcal) kJ`) %>%
  relocate(`FIBTGg`, .after = `FIBTG or [FIBC]  (g)`) %>%
  relocate(`FIBCg`, .after = `FIBTGg`) %>%
  relocate(`TOCHPAmg`, .after = `VITE, or [TOCPHA] (mg)`) %>%
  relocate(`source_fct`, .after = `Source/BiblioID`)



# Food Group extraction ----

for (i in 1:nrow(BA13_Main_Table)){ #This for loop finds the food group code from the ID code, and assigns the correct food group based on that and the accompanying pdf doc
  test_string_Code <- toString(BA13_Main_Table$Code[i]) #converts the rows code to a string
  split_string_1 <- str_split(test_string_Code, "_") #splits the string in two around the underscore
  code <- split_string_1[[1]][1] #Assigns the first half of the split string to a variable we can check against, "code"
  if (code == "01"){BA13_Main_Table$`Food Group`[i] <- "Cereals and their products"}         #These if statements are what assign the correct food group to the food group column 
  if (code == "02"){BA13_Main_Table$`Food Group`[i] <- "Pulses, legumes and their products"} #based on the "code" assigned in the previous step of the for loop
  if (code == "03"){BA13_Main_Table$`Food Group`[i] <- "Vegetables and their products"}
  if (code == "04"){BA13_Main_Table$`Food Group`[i] <- "Leafy vegetables"}
  if (code == "05"){BA13_Main_Table$`Food Group`[i] <- "Starchy roots, tubers and their products"}
  if (code == "06"){BA13_Main_Table$`Food Group`[i] <- "Nuts, seeds and their products"}
  if (code == "07"){BA13_Main_Table$`Food Group`[i] <- "Spices, condiments and herbs"}
  if (code == "08"){BA13_Main_Table$`Food Group`[i] <- "Fruits"}
  if (code == "09"){BA13_Main_Table$`Food Group`[i] <- "Fish, shellfish and their products"}
  if (code == "10"){BA13_Main_Table$`Food Group`[i] <- "Meat, poultry and their products"}
  if (code == "11"){BA13_Main_Table$`Food Group`[i] <- "Eggs and their products"}
  if (code == "12"){BA13_Main_Table$`Food Group`[i] <- "Milk and its product"}
  if (code == "13"){BA13_Main_Table$`Food Group`[i] <- "Fats and oils"}
  if (code == "14"){BA13_Main_Table$`Food Group`[i] <- "Beverages"}
  if (code == "15"){BA13_Main_Table$`Food Group`[i] <- "Miscellaneous"}
}



# Energy value seperation ----

for (i in 1:nrow(BA13_Main_Table)){ #The current energy values are in a single column - this splits them out to kcal and kj
  test_string_Energy <- toString(BA13_Main_Table$`ENERC (kcal) kJ`[i]) #this sets it to a string to be checked
  if(grepl("(", test_string_Energy, fixed = TRUE)){ #this searches for an "(" in the string (denoting a kcal value in brackets)
    split_string_1 <- str_split(test_string_Energy, "\\(") #This splits the string either side of the "("
    split_string_2 <- str_split(split_string_1[[1]][2], "\\)") #This splits second part of the first split around the ")"
    Energy_kcal <- split_string_2[[1]][1] #This takes and assigns the first part of the second split - the contents of the brackets from the original string
    Energy_kJ <- gsub(paste0("(", Energy_kcal, ")"), "", test_string_Energy, fixed=TRUE) #This takes the original spring, and replaces the brackets and value within 
  }                                                                                     # the brackets with nothing, leaving the other energy value (kJ)
  BA13_Main_Table$`ENERC (kcal) kJ`[i] <- Energy_kcal #Assigns the cleaned kcal value to the old combined (column is renamed to just kcal later on) 
  BA13_Main_Table$`ENERCkJ`[i] <- Energy_kJ #Assigns the cleaned kJ value to the new ENERCkJ column  
}



# Fibre value seperation ----

for (i in 1:nrow(BA13_Main_Table)){ #This value checks if the value is in square brackets, and if it is assigns it to its own FIBC column  
  test_string_Fibre <- toString(BA13_Main_Table$`FIBTG or [FIBC]  (g)`[i]) #this sets it to a string to be checked
  if(grepl("[", test_string_Fibre, fixed = TRUE)){ #this searches for an "[" in the string (denoting a FIBC value in square brackets)
    crude_fibre <- test_string_Fibre #this assigns the string to a variable
    crude_fibre %<>% #These remove the square brackets from the variable
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .)
    BA13_Main_Table$FIBCg[i] <- crude_fibre #this assigns the cleaned up string to the crude Fibre column
    BA13_Main_Table$`FIBTG or [FIBC]  (g)`[i] <- crude_fibre #This assigns the cleaned up string to the original column too - this will later be renamed to FIBTGg_standardised
  } else {
    BA13_Main_Table$FIBTGg[i] <- BA13_Main_Table$`FIBTG or [FIBC]  (g)`[i] #If no square brackets are detected, the whole string is assigned to FIBTGg 
  }
}



# VitE/TOCPHA value seperation ----

for (i in 1:nrow(BA13_Main_Table)){ #Very similarly to the Fibre for loop, this seperates the TOCPHA value from the VITE/TOCPHA column 
  test_string_VitE <- toString(BA13_Main_Table$`VITE, or [TOCPHA] (mg)`[i]) #this sets the cell to a string
  if(grepl("[", test_string_VitE, fixed = TRUE)){ #searches for a square bracket, denoting a TOCPHA value
    TOCPHA_string <- test_string_VitE #assigns the string to a variable
    TOCPHA_string %<>% #These remove the square brackets from the variable
      gsub("\\[", "", .) %>%
      gsub("\\]", "", .)
    BA13_Main_Table$`TOCHPAmg`[i] <- TOCPHA_string #this assigns the TOCPHA variable to its own column, created earlier
    BA13_Main_Table$`VITE, or [TOCPHA] (mg)`[i] <- NA #Sets the original column to NA where its a TOCPHA value - this leaves only the VITE entries left
  }
}





# NIAEQ/NIA value seperation ----

for (i in 1:nrow(BA13_Main_Table)){ #Very similarly to the Fibre for loop, this seperates the TOCPHA value from the VITE/TOCPHA column 
  test_string_NIA <- toString(BA13_Main_Table$`NIAEQ, or [NIA] (mg)`[i]) #this sets the cell to a string
  if(grepl("[", test_string_NIA, fixed = TRUE)){ #searches for a square bracket, denoting a NIA value, which already has its own column
    BA13_Main_Table$`NIAEQ, or [NIA] (mg)`[i] <- NA #Sets the original column to NA where its a NIA value - this leaves only the NIAEQ entries left
  }
}



# Column renaming ----

colnames(BA13_Main_Table)[9] <- "ENERCkcal" #These renames rename the original multi-variable columns to the single variable they were left with fro mthe above for loops
colnames(BA13_Main_Table)[15] <- "FIBTGg_standardised"
colnames(BA13_Main_Table)[34] <- "VITEmg"
colnames(BA13_Main_Table)[38] <- "NIAEQmg"



# data frame merging ----

BA13_Amino_acids <- Annex_Amino_acids[!is.na(Annex_Amino_acids$`Food name in English`),] %>% #This section goes through each of the other dataframes from the other sheets, and 
  mutate(`Food name in English` = NULL,                                                      #removes the columns already present in the main dataframe, preventing conflicts on merge
         `PROT` = NULL)
BA13_Fatty_acids <- Annex_Fatty_acids[!is.na(Annex_Fatty_acids$`Food name in English`),] %>%
  mutate(`Food name in English` = NULL,
         `WATER` = NULL,
         `FATCE` = NULL)
BA13_Antinutrients <- Annex_Antinutrients[!is.na(Annex_Antinutrients$`Food name in English`),] %>%
  mutate(`Food name in English` = NULL,
         `WATER` = NULL)
BA13_Antioxidants <- Annex_Antioxidants[!is.na(Annex_Antioxidants$`Food name in English`),] %>%
  mutate(`Food name in English` = NULL,
         `WATER` = NULL)
BA13_Sugar <- Annex_Sugar[!is.na(Annex_Sugar$`Food name in English`),] %>%
  mutate(`Food name in English` = NULL,
         `WATER` = NULL)

Output_table <- BA13_Main_Table #This batch of code sets the output to be a copy of the main FCT, then adds on the other FCT tables to them.
Output_table <- left_join(Output_table, BA13_Amino_acids, by = "Code")
Output_table <- left_join(Output_table, BA13_Antinutrients, by = "Code")
Output_table <- left_join(Output_table, BA13_Antioxidants, by = "Code")
Output_table <- left_join(Output_table, BA13_Fatty_acids, by = "Code")
Output_table <- left_join(Output_table, BA13_Sugar, by = "Code")



# Column Renaming ----

for(i in 1:ncol(Output_table)){ #This loop renames the columns to the FAO tagnames without brackets etc.
  original_colname <- colnames(Output_table)[i] #This pulls the column name and sets it as a variable
  if(grepl("(", original_colname, fixed = TRUE)){ #This tests the variable to see if it contains a bracket
    cleaned_name <- gsub(" \\(|\\(|\\)", "", original_colname) #If it does have a bracket, the brackets and any spacebars are removed, creating the cleaned name variable
    colnames(Output_table)[i] <- cleaned_name #The column name is then set to this cleaned name variable
  }
}

special_names <- c('fdc_id', 'food_desc', "food_group", "food_desc_bengali", #This is a list of correct column names to apply to the data frame
                   "scientific_name", "nutrient_data_source",
                   "Edible_factor_in_FCT", "PROCNTg", "FATCEg", "CHOAVLDFg",
                   "ASHg")

Output_table <- Output_table %>% rename_at(vars(1:6, 8, 12:14, 18),  ~special_names)  %>% #This renames certain columns (denoted by the column number) to the names listed above
  rename(VITB6Amg = "VITB6A mg", #This sets through some more renames, done based on name rather than position
         FASATg = "FASAT", 
         FAMSg = "FAMS", 
         FAPUg = "FAPU", 
         CHOL_mg = "CHOL-mg", 
         SUGARg = "SUGAR")

# Transforming values ----

#Check presence of trace, and [] 
#NOTE: tr will be found in non-numeric variables (i.e., fooditem)
Output_table[9:ncol(Output_table)] %>% str_extract_all(.,"tr|[tr]|Tr|\\[.*?\\]")

#Function that removes [] and changes trace with 0

no_character <- function(x){
  case_when(
    str_detect(x, 'tr|[tr]|Tr') ~ "0",
    str_detect(x, '\\[.*?\\]')  ~ str_extract(x, '(?<=\\[).*?(?=\\])'),
   # str_detect(x, '[*]')  ~ str_extract(x, "[:digit:]+"),
    TRUE ~ x)
}

#Applying the function to change trace to 0 and removing []
Output_table <- Output_table %>%
  mutate_at(vars(9:ncol(Output_table)), no_character)

#Checking that the function worked 
Output_table[9:ncol(Output_table)] %>% str_extract_all(.,"tr|[tr]|Tr|\\[.*?\\]")


# Data Output ----

#checking data before saving

glimpse(Output_table)

write.csv(Output_table, file = here::here("output", "BA13_FCT_FAO_Tags.csv"),
          row.names = FALSE) #This writes the Output table to a csv in the projects "Output" subfolder

#Run this to clean the environment - do so if running the script individually
#rm(list = ls())