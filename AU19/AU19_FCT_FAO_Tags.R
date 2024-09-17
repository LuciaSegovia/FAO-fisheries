library(tidyverse)
library(visdat)



# Data Import ----

AU19_Main_Raw <- readxl::read_excel(here::here('AU19', "FCTs_Feb_2022.xlsx"), sheet = 1) %>% #This reads in the excel document, and assigns sheet 1 to an R data frame
  mutate(`Energy, without dietary fibre` = NULL) #This removes the column "Energy, without dietary fibre" on import



# Column Renaming ----

for (i in 1:ncol(AU19_Main_Raw)){ #This for loop runs through each column, compressing the first two rows into a replacement column name if they contain something
  col_name <- colnames(AU19_Main_Raw)[i] #This takes the column name in question and creates a variable for it
  if(!is.na(AU19_Main_Raw[1,i])){ #If the first column isn't missing/NA, the column name is replaced with the first row
    col_name <- paste0(AU19_Main_Raw[1,i])
  }
  if(!is.na(AU19_Main_Raw[2,i])){ #Then, if the second column isn't missing/NA, the column name is replaced with the first row and second row combined
    col_name <- paste0(col_name, AU19_Main_Raw[2,i])
  }
  if(toString(colnames(AU19_Main_Raw)[i]) != col_name){ #If the new column name is different to the original, then the new name is tidied up
    col_name <- iconv(col_name, to = "UTF-8") #converted to UTF-8 to deal with some special characters
    col_name <- gsub("Â", "", col_name) #Odd characters shown by the UTF-8 conversion are corrected
    col_name <- gsub("µg", "mcg", col_name) #Odd characters shown by the UTF-8 conversion are corrected
    colnames(AU19_Main_Raw)[i] <- col_name #The old name is replaced with the new name
  }
}


# New Column Creation ----

AU19_Main_Raw$source_fct <- "AU19" #This creates and populates a "source_fct" column, with the entry "AU19"
AU19_Main_Raw$EDIBLE <- NA #This creates the EDIBLEpc (Edible percent) column, and populates it with NA - this column is used later


# Edible percent extraction ----

for (i in 1:nrow(AU19_Main_Raw)){ #Loops through each row of the data frame
  test_string_Edible <- toString(AU19_Main_Raw$`Analysed portion`[i]) #extracts the EDIBLE column entry for that row into a variable
  if (test_string_Edible == "1"){ #These next steps cover most of the common entries, and the rule exceptions, with the edible percentages they correspond to
    EDIBLE <- 100                 # with the edible percentages they correspond to - e.g. "1" is 100
  } else if (test_string_Edible == "98.1 (lean meat)"){
    EDIBLE <- 98.1
  } else if (test_string_Edible == "95.9 (lean meat 95.2%, inseparable internal fat 0.7%)"){
    EDIBLE <- 95.9
  } else if (test_string_Edible == "97.2 (lean meat 95.2%, internal/external fat 2.0%)"){
    EDIBLE <- 97.2
  } else if(grepl("%", test_string_Edible, fixed = TRUE)){ #This is the general rule, for those not caught above - searches for a % sign
    split_string_1 <- str_split(test_string_Edible, "%") #and splits the string based on that % sign
    EDIBLE <- as.numeric(split_string_1[[1]][1]) #and returns the first half of the split (anything before the %) as the value
  } else if (!is.na(as.numeric(test_string_Edible))){ #The other general rule, for if theres no % sign, checks if the entry is already a number only
    EDIBLE <- as.numeric(test_string_Edible)*100 #If so, it is multiplied by 100 (to get from a fraction to a percentage)
  } else { #This else statement catches any results, and prints them for further attention
    print(i) #prints the erroneous row
    EDIBLE <- "Error" #assigns a value of "Error"
    print(paste0("Else function - ", EDIBLE)) #prints again to highlight the issue
  }
  AU19_Main_Raw$EDIBLE[i] <- EDIBLE #Finally, regardless of which rule was used, the result is assigned to the EDIBLE column here
}
AU19_Main_Raw$EDIBLE <- as.numeric(AU19_Main_Raw$EDIBLE)/100 #Makes sure that the EDIBLE column is numeric

# Unit conversion ----

Output_table <- AU19_Main_Raw %>% 
  mutate(FN20D5N3g = as.numeric(F20D5N3mg)/1000, #mg to g of EPA, 
         FN22D6N3g = as.numeric(F22D6N3mg)/1000)  #and DHA


# Tidying up the output table ----

Output_table <- Output_table %>%
  slice(-c(1:2, 1537:1538)) %>% #rows 1, 2, 1537 and 1538 are removed
  relocate(source_fct, .after = `Scientific name`) %>% #some columns are moved to be more easily read
  relocate(EDIBLE, .after = `Specific Gravity`) %>%
  rename( fdc_id = "Public Food Key", #Some columns are renamed to be in keeping with the FAO tagnames and derived metadata tagnames
         food_group = "Classification name", #E.g., here, the column "Classification name" is renamed to "food_group"
         food_desc = "Food Name",
         scientific_name = "Scientific name",
         Edible_factor_in_FCT = "EDIBLE",
         Edible_desc = "Edible/refuse description", 
         specific_gravity = "Specific Gravity",
         nutrient_data_source = "Sampling details")



#Check presence of trace, [] and * 
#NOTE: tr will be found in non-numeric variables (i.e., fooditem)
Output_table %>% str_which(.,"tr|[tr]|Tr|\\[.*?\\]|[*]")
Output_table %>% str_extract_all(.,"tr|[tr]|Tr|\\[.*?\\]|[*]")


# Data Visualisation ----

AU19_FCT_FAO_Tags <- Output_table

#Checking all variables name

glimpse(AU19_FCT_FAO_Tags) #lists all column names for checking

#Visualising NA
#remove bc not interested and high NA

#20,30, 35:45, 48:61, 63:65, 67, 70, 73:75,
#80:81, 89:91, 123:140 (Removing Fatty Acids)
#142:150, 152:164, 166:169, 171, 173:176, 179:195 (Removing AAs but TRP)


vis_dat(AU19_FCT_FAO_Tags) #Visualises the column type breakdown - character, numeric, or NA

vis_miss(AU19_FCT_FAO_Tags[191:195]) #Visualises the number of missing values for columns 191 to 195 - PROmg to VALmg

AU19_FCT_FAO_Tags %>% select(-c(20,30, 35:45, 48:61, 63:65, 67, 70, 73:75, #Visualises the missing entries for all columns except those listed
                             80:81, 89:91, 123:140,
                             142:150, 152:164, 166:169, 171, 173:176, 179:195)) %>% 
  vis_miss()


# Data Output ----

glimpse(Output_table) #checking variables before saving

write.csv(Output_table, file = here::here("output", "AU19_FCT_FAO_Tags.csv"), #Saves the table as a csv called "AU19_FCT_FAO_Tags"
          row.names = FALSE)

#Run this to clean the environment - do so if running the script on its own
#rm(list = ls())