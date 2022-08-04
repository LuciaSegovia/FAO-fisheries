
# Packages dependencies
# tinytex::install_tinytex(version = "latest")
# dplyr, ggplot, gt


library(tidyverse)

source(here::here("QC.R"))
source(here::here("Group_Summariser.R"))

dim(fao_fish_fct)

df1 <- fao_fish_fct
data_columns=c(10,12:92, 101:103) #changed this so source_fct, scientific.name and nutrient_source are not converted to NA, and I and VitB6_standardised are included
df1[ ,data_columns] <- apply(df1[ ,data_columns], 2,
                             function(x) as.numeric(as.character(x)))

results_table <- Group_Summariser(df1, 'ICS.FAOSTAT.SUA.Current.Code', 'Scientific.name') %>%
  mutate_at(data_columns, as.numeric)

dim(results_table)

#Adding extra columns to keep the structure as FAO spreadsheet
results_table[,c(104:122) ] <- NA

#27
results_table <- results_table %>% 
  relocate(c("ICS.FAOSTAT.SUA.Current.Code", "ics_faostat_sua_english_description", 
             "quality_rating_for_food_match", "source_fct", "NDB_number",
             "fdc_id", "food_desc", "scientific_name", "ISSCAAP.Group", 
             "X3.alpha.code",  "Edible_factor_in_FCT", "V105", "SOPg", 
             "ENERCkcal"  , "ENERCkJ", "V106", "V107",  "WATERg",
             "PROCNTg" , "XN", "FAT_g_standardised", "FATg" , "FATCEg","FAT_g", 
             "V108", "CHOCDFg" ,"CHOAVLMg", "CHOAVLg", "CHOAVLDFg",
             "V109", "FIBTGg", "NSPg", "FIBCg", "ALCg" , "ASHg", 
             "CAmg", "FEmg", "MGmg", "Pmg", "Kmg", "NAmg", "ZNmg", "CUmg",
             "MNmg", "SEmcg" , "V110", "V111", "VITA_RAEmcg", "VITAmcg",
             "RETOLmcg","V112", "CARTBEQmcg","CARTAmcg", "CARTBmcg","CRYPXBmcg",
             "V113", "VITEmg", "TOCPHAmg", "TOCPHBmg", "TOCPHGmg", "TOCPHDmg",
             "TOCTRAmg", "TOCTRBmg", "TOCTRGmg", "V114", "THIAmg", "THIAHCLmg",
             "RIBFmg",  "VITB6_mg_standardised", "VITB6Cmg", "VITB6Amg", 
             "VITB6_mg", "V115", "FOLDFEmcg", "FOLmcg", "FOLACmcg", "FOLFDmcg",
             "FOLSUMmcg", "FOL_mcg", "V116", "NIAEQmg", "NIAmg", "V117",
             "NIATRPmg", "TRPmg", "VITB12mcg", "VITCmg", "ASCLmg", "FASATg", 
             "FAMSg", "FAPUg", "FATRNg", "CHOLEmg", "CHOL_mg", "SUGARg", 
             "F22D6N3g", "F20D5N3g","NIAmg_std", "IDmcg", "comment")) %>%
  select(-c(Food.description, Scientific.name, ICS_FAOSTAT, quality,
            V118:V122)) %>%               
   mutate_if(is.numeric,  round,
             digits = 2)

write.csv(results_table, file = here::here("Output",
                              "Summarised_Results_Table_v1.csv"), 
          row.names = FALSE)


            