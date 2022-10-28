library(tidyverse)



# Data Import ----

BR11_Raw <- readxl::read_excel(here::here('BR11', "FCTs_Feb_2022.xlsx"), sheet = 5) %>% #Reads the excel document and assigns the relevant sheet to an R data frame
  slice(-c(1:3, 610:612)) #Removes rows 1 to 3, and 610 to 612 on import - these are empty/unused.



# Column Renaming ----

for (i in 1:ncol(BR11_Raw)){ #For this FCT, the column names already consist of the FAO tagnames and units, but with some special characters in-between, which this loop removes
  col_name <- colnames(BR11_Raw)[i] #the current column name is assigned an R variable
  col_name %<>%
    gsub("\\(", "", .) %>% #Each of the problem characters are removed (in order - '(', ')', '_', 'µ', ) - most are removed, but the micro symbol is replaced with mc
    gsub("\\)", "", .) %>%
    gsub("_", "", .) %>%
    gsub("µ", "mc", .)
  colnames(BR11_Raw)[i] <- col_name #The newly tidied name is applied and overwrites the original one
}

Output_table <- BR11_Raw %>% 
  rename("fdc_id" = colnames(BR11_Raw)[1], #The metadata tags are then manually renamed
         "food_desc_portuguese" = colnames(BR11_Raw)[2],
         "food_desc" = colnames(BR11_Raw)[3],
         "scientific_name" = colnames(BR11_Raw)[4],
         "ISSCAAP" = colnames(BR11_Raw)[5],
         "alpha_code" = colnames(BR11_Raw)[6],
         "VITA_RAEmcg" = VITARAEmcg, 
  )



# New Column Creation & relocation ----

Output_table$source_fct <- "BR11" #Creates the source FCT column, and populates it with "BR11"
Output_table$nutrient_data_source <- "None listed" #Creates the nutrient data source column, and notifies no data sources are listed
BR11_FCT_FAO_Tags <- Output_table %>% 
  relocate(source_fct, .after = food_desc) %>% #Moves the source_fct column to the metadata columns at the start of the table
  relocate(nutrient_data_source, .after = source_fct) #Moves the nutrient data source column to the metadata columns at the start of the table



# Changing trace values ("Tr") to zero and tidying ----

Output_table[Output_table == "Tr"] <- "0" #Replaces trace values with 0
Output_table <- Output_table %>%
  slice(-c(598:606)) #Removes "legend" rows



# Data Output ----

glimpse(Output_table)

write.csv(Output_table, file = here::here("Output", "BR11_FCT_FAO_Tags.csv"), row.names = FALSE) #Saves the newly-cleaned table to the Output folder 

#Run this to clean the environment
rm(list = ls())