
#Required packages - please ensure they're installed before running. Installation can be done by uncommenting the relevant line below and running that line.

#install.packages("here")
#install.packages("gt")
#install.packages("stringr")
#install.packages("purrr")
#install.packages("readr")
#install.packages("measurements")
#install.packages("visdat")
#install.packages("readxl")
#install.packages("janitor")

# tictoc::tic("total")

# library(tidyverse)

#This should allow the file save location to be changed should the user wish to, as long as the file format remains the same.
supporting_datasets_savefilename <- readLines("supporting_datasets.R") #Reads in file
supporting_datasets_savefilename <- supporting_datasets_savefilename[! supporting_datasets_savefilename %in% ""] #Removes blanks
supporting_datasets_savefilename <- supporting_datasets_savefilename[length(supporting_datasets_savefilename)] #Finds last non-blank row
supporting_datasets_savefilename <- strsplit(supporting_datasets_savefilename, "\"", fixed = T)[[1]][2] #Extracts the correct portion of it

if(!file.exists(supporting_datasets_savefilename)){ #Checks if the final file of this script is present - if it isn't, runs it. 
  source(here::here("supporting_datasets.R"))
}

source(here::here("QC.R"))
source(here::here("functions.R"))

print("here 1")

# source(here::here("summary_table_functions.R")) # TODO I have created a separate file with my functions for testing. Once all other functions are tested then we can merge.
# source(here::here("Summarised_Row_Recalculator.R"))

# Reading formatting R file (from "supporting_datasets.R”)
ics <- readRDS(file = "data/fao-ics-desc.rds")
edible_ics <- readRDS(file = "data/edible_coefficient.rds") # edible portion (lines 156-204)
fisheries <- read.csv(here::here("data", "FISHERIES-GlobalNCT_ForSharing_Feb2022.csv"), encoding = "UTF-8")

dim(fao_fish_fct)
names(fao_fish_fct)
names(fisheries)[1] <- "ï..GLOBAL.TABLE.OF.NUTRIENT.VALUES"
names(edible_ics)[1] <- "ICS.FAOSTAT.SUA.Current.Code"

#Starting with formatting

df1 <- fao_fish_fct 

# Formatting - matching columns to original excel Fisheries Global NCT, 2022
# Extra variables: ASHg_std, ASHg_bydiff, etc. from the original dataset.
# We are putting them at the end, to keep the structure consistent.

dim(df1)
df1$ICS.FAOSTAT.SUA.Current.Code <- as.character(df1$ICS.FAOSTAT.SUA.Current.Code)
ics$`ICS FAOSTAT SUA Current Code` <- as.character(ics$`ICS FAOSTAT SUA Current Code`)

df1$Edible_factor_desc <- NA
df1$FOLDFEmcg_std <- NA
df1$NIAEQmg_std <- NA
df1$NIATRIPmg_std <- NA
df1$VITEmg_std <- NA

n <- ncol(df1) + 1

df1 <- left_join(df1, ics,
                 by = c("ICS.FAOSTAT.SUA.Current.Code" = "ICS FAOSTAT SUA Current Code"))

ideal_relocate_column_list <- c(
  "quality_rating_for_food_match", "source_fct", "NDB_number",
  "fdc_id", "food_desc", "scientific_name", "ISSCAAP.Group",
  "X3.alpha.code", "Edible_factor_in_FCT", "Edible_factor_desc",
  "SOPg_calculated",
  "ENERCkJ_std", "ENERCkcal_std", "ENERCkJ", "ENERCkcal", "WATERg",
  "PROCNTg", "XN", "FAT_g_standardised", "FATg", "FATCEg", "FAT_g",
  "CHOAVLDFg_std", "CHOCDFg", "CHOAVLDFg", "CHOAVLg", "CHOAVLMg",
  "FIBTGg_std", "FIBTGg", "NSPg", "FIBCg", "ALCg", "ASHg",
  "CAmg", "FEmg", "MGmg", "Pmg", "Kmg", "NAmg", "ZNmg", "CUmg",
  "MNmg", "SEmcg", "VITA_RAEmcg_std", "VITAmcg_std", "VITA_RAEmcg",
  "VITAmcg", "RETOLmcg", "CARTBEQmcg_std", "CARTBEQmcg", "CARTAmcg",
  "CARTBmcg", "CRYPXBmcg",
  "VITEmg_std", # VITEmg --> future TBC
  "VITEmg", "TOCPHAmg", "TOCPHBmg", "TOCPHGmg", "TOCPHDmg",
  "TOCTRAmg", "TOCTRBmg", "TOCTRGmg", "THIAmg_std", "THIAmg",
  "THIAHCLmg",
  "RIBFmg", "VITB6_mg_standardised", "VITB6Cmg", "VITB6Amg",
  "VITB6_mg", "FOLDFEmcg_std", # FOLDFEmcg --> standardise calculated #future
  "FOLDFEmcg", "FOLmcg", "FOLACmcg", "FOLFDmcg",
  "FOLSUMmcg", "FOL_mcg", "NIAEQmg_std", # NIAEQmg --> standardise calculated #future
  "NIAEQmg", "NIAmg", "NIATRIPmg_std", # NIATRIPmg --> standardise calculated #future
  "NIATRPmg", "TRPmg", "VITB12mcg", "VITCmg", "ASCLmg", "FASATg",
  "FAMSg", "FAPUg", "FATRNg", "CHOLEmg", "CHOL_mg", "SUGARg",
  "F22D6N3g", "F20D5N3g", "NIAmg_std", "IDmcg", "ASHg_std",
  "ASHg_bydiff",
  "comments"
)

current_relocate_list <- ideal_relocate_column_list[ideal_relocate_column_list %in% colnames(df1)]

ideal_remove_list <- c("ics_faostat_sua_english_description",
  "Food.description", "Scientific.name", "ICS_FAOSTAT", "quality",
  "fish_type", "fish_prep", "food_group", #CHOg, 
  "ALCg_100mL")

current_remove_list <- ideal_remove_list[ideal_remove_list %in% colnames(df1)]

 df1 <- df1 %>%
  relocate(ICS.FAOSTAT.SUA.Current.Code,
           .before = "ics_faostat_sua_english_description"
  ) %>%
  relocate(c(n:(n + 4)), .after = "ics_faostat_sua_english_description") %>%
  relocate(current_relocate_list, .after = "ISSCAAP Group") %>%
  select(-current_remove_list)


# Selecting variables that should be numeric
n1 <- match("SOPg_calculated", names(df1))
n2 <- match("comments", names(df1)) - 1

# names(df1[, c(n:ncol(df1))])
# checking variables
names(df1[, c(n1:n2)])
#>83
dim(df1[, c(n1:n2)])

## Change the no. variables to the above and run
# data_columns <- c(10, 12:92, 101:103) # changed this so source_fct, scientific.name and nutrient_source are not converted to NA, and I and VitB6_standardised are included

data_columns <- names(df1[, c(n1:n2)])

df1[, data_columns] <- apply(
  df1[, data_columns], 2,
  function(x) as.numeric(as.character(x))
)

results_table <- Group_Summariser(df1, "ICS.FAOSTAT.SUA.Current.Code", sep_row = T) %>%
  mutate_at(data_columns, as.numeric)

# Checking nrow() - Should be true - only adding 1 row or 2 row if: sep_row = T
nrow(df1) == (nrow(results_table) - (2 * 95))

print("here 2")


dim(results_table)
names(results_table)

head(results_table)
# Re - calculating variables (#43)

source("functions/Summarised_Row_Recalculator.R")

recalculated_results_table <- Grp_Smrsr_row_update(results_table, 1, CARTBEQmcg_std = "CARTBEQmcg_combined")

# Copying FISHERIES-GlobalNCT_ForSharing_Feb2022 (excel) - format (#43)

# Need to find the columns we need to add - and which ones are already present

# Adding extra columns to FAO spreadsheet to account for new variables
dim(fisheries)
fisheries[, c(ncol(fisheries):(ncol(fisheries) + 12))] <- NA

new_col_names <- as.character(fisheries[2, 7:29])

# Adding extra columns to keep the structure as FAO spreadsheet
recalculated_results_table[, c(ncol(recalculated_results_table):(ncol(recalculated_results_table) + 23))] <- NA
names(recalculated_results_table[, c((ncol(recalculated_results_table) - 22):ncol(recalculated_results_table))])
dim(recalculated_results_table)

# Renaming and reorganising columns in our dataset

first_NA_col <- which(colnames(recalculated_results_table) == colnames(recalculated_results_table %>% select(last_col(22))))
last_NA_col <- which(colnames(recalculated_results_table) == colnames(recalculated_results_table %>% select(last_col())))

new_col_names[1] <- "NA1" #Both were previously "NA"
new_col_names[2] <- "NA2"

new_col_names[6] <- "Seq1"
new_col_names[18] <- "Seq2"

new_col_names[9] <- "f=fortified with micronutrients1"
new_col_names[20] <- "f=fortified with micronutrients2"


new_col_names[10] <- "Edible coefficient to be used1"
new_col_names[21] <- "Edible coefficient to be used2"

new_col_names[11] <- "1 = as purchased i.e. edible factors of FCTs can be used after verification of definition of edible factors. 2 = as produced i.e. the default factors given here should be used unless country-specific factors are available1"
new_col_names[22] <- "1 = as purchased i.e. edible factors of FCTs can be used after verification of definition of edible factors. 2 = as produced i.e. the default factors given here should be used unless country-specific factors are available2"

new_col_names[12] <- "Source for edible coeff.and/or comments1"
new_col_names[23] <- "Source for edible coeff.and/or comments2"


new_col_names[17] <- " " #This was "", but that was blank and throwing errors when trying to run the rest of the code


new_col_names %in% colnames(recalculated_results_table)



colnames(recalculated_results_table)[first_NA_col:last_NA_col] <- new_col_names

duplicate_colnames <- colnames(recalculated_results_table)[duplicated(colnames(recalculated_results_table))]

duplicate_colnames %in% new_col_names #All duplicates are in the new column names

first_NA_colname <- colnames(recalculated_results_table)[first_NA_col]
last_NA_colname <- colnames(recalculated_results_table)[last_NA_col]

recalculated_results_table <- recalculated_results_table %>% relocate(all_of(new_col_names), .after = "ISSCAAP Group")

# Combining variable names

#names(recalculated_results_table)[1:122] <- names(fisheries)[1:122]
#names(fisheries)[123:139] <- names(recalculated_results_table)[123:139]

# Merging variable names
#recalculated_results_table <- rbind(
# fisheries[c(1:2), ],
#  recalculated_results_table
#)

# Add Edible portion from "Edible coefficient to be used"
# TO-DO: Check if could be calculated

n1 <- match("quality_rating_for_food_match", names(recalculated_results_table)) -3
names(recalculated_results_table[n1])

recalculated_results_table <- recalculated_results_table %>%
  select(-n1) %>%
  left_join(., edible_ics ) %>%
  relocate("X.24", .after = new_col_names[20])

#27
(match("X.24", names(recalculated_results_table)))


#n1 <- match("X.36", names(recalculated_results_table))
#n2 <- match("comments", names(recalculated_results_table))-1
#
#recalculated_results_table <-  recalculated_results_table %>% mutate_at(n1:n2, as.numeric)
#  
#recalculated_results_table$X.110[1:3]
# class(recalculated_results_table$X.110)

class(recalculated_results_table$FOLFDmcg)

n1 <- which(recalculated_results_table$CHOAVLDFg_calculated< 0)

recalculated_results_table$comments[n1] <- ifelse(is.na(recalculated_results_table$comments[n1]),
                                                 "CHOAVLDFg_calculated assumed zero", paste(recalculated_results_table$comments, "| CHOAVLDFg_calculated assumed zero") )
recalculated_results_table$CHOAVLDFg_calculated[recalculated_results_table$CHOAVLDFg_calculated< 0] <- 0

recalculated_results_table$CHOAVLDFg_calculated

# tictoc::toc()

#checking duplicates w/i each category

recalculated_results_table %>% group_by(ICS.FAOSTAT.SUA.Current.Code) %>% 
  count(source_fct, fdc_id) %>% 
  filter(n>1) %>% 
  arrange(desc(n))


# writing the output table
recalculated_results_table %>%
  # mutate_if(is.numeric, round, digits = 2) %>%
  write.csv(.,
            file = here::here(
              "output",
              paste0("Fisheries-Global-NCT_", Sys.Date(), ".csv")
            ),
            row.names = FALSE
  )

recalculated_results_table %>% select("comments") %>%
  # mutate_if(is.numeric, round, digits = 2) %>%
  write.csv(.,
            file = here::here(
              "output",
              paste0("Fisheries-Global-NCT_comments_", Sys.Date(), ".csv")
            ),
            row.names = FALSE
  )
