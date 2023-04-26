
library(tidyverse)

# Important Note ----

IN17_Raw <- readxl::read_excel(here::here('IN17', "FCTs_Feb_2022.xlsx"), sheet = 4) %>%
  slice(-1)


#<LOD (below the limit of detention) will be converted to 0
#This was agreed w/ FAO team.

IN17_Raw[IN17_Raw == "<LOD"] <- "0"

# Renaming variables from the columns

for (i in 1:ncol(IN17_Raw)){
  col_name <- colnames(IN17_Raw)[i]
  if(!is.na(IN17_Raw[4,i])){
    col_name <- paste0(IN17_Raw[4,i])
  }
  if(!is.na(IN17_Raw[3,i])){
    col_name <- paste0(col_name, IN17_Raw[3,i])
  }
  if(toString(colnames(IN17_Raw)[i]) != col_name){
    colnames(IN17_Raw)[i] <- col_name
  }
}

colnames(IN17_Raw) <- sub("/100g PTN", "_100gPROTCNT", colnames(IN17_Raw))

#Manually renaming some of the variables
IN17_Renamed <- IN17_Raw %>%
  rename(fdc_id = "Food code", 
         food_desc = "Food name",
         scientific_name = "Scientific name",
         CHOAVLg = "Table 6. STARCH AND INDIVIDUAL SUGARSg",
         LACSg =  "...70g",
         FASATmg ="FASATFatty Acids (mg)",
         FAMSmg = "FAMSFatty Acids (mg)",
         FAPUmg ="FAPUFatty Acids (mg)", 
         PROCNTg = "PROTCNTg")

#Variable names for manual renaming of the organic acids

#generating a vector with the names for the organic acids
in17_colnames <- c( "Oxalate_Total",
                    "Oxalate_Soluble",
                    "Oxalate_Insoluble",
                    "Cis_Aconitic_Acid",
                    "CITACmg",
                    "FUMACmg",
                    "MALACmg",
                    "Quinic Acid",
                    "SUCACmg",
                    "TARACmg",
                    "three_4_Dihydroxybenzoic_Acid",
                    "three_Hydroxybenzaldehyde",
                    "Protocatechuic_Acid",
                    "Vanillic_Acid",
                    "GALLACmg",
                    "Cinnamic_Acid",
                    "O_Coumaric_Acid",
                    "P_Coumaric_Acid",
                    "Caffeic_Acid",
                    "CHLRACmg",
                    "FERACmg",
                    "APIGENmg",
                    "Apigenin_6Cgluoside",
                    "Apigenin_7Oneohesperidoside",
                    "LUTEOLmg",
                    "KAEMFmg",
                    "QUERCEmg",
                    "Quercitin3_betaDglucoside",
                    "Quercetin3_Orutinoside",
                    "Quercetin3_betagalactoside",
                    "Isorhamnetin",
                    "Myricetin",
                    "Resveratol",
                    "HESPTmg",
                    "Naringenin",
                    "HESPDmg",
                    "DAIDZNmg",
                    "GNSTEINmg",
                    "EPICATECmg",
                    "EPICATEGCmg",
                    "EPICATEGC_3gallatemg", #changed (-) to negative and (+) to positive
                    "positiveCatechin",
                    "negativeGallocatechin_gallate", #Used to be (-) Gallactechin gallate
                    "negativeGallocatechin",
                    "Syringic_Acid",
                    "Sinapinic_Acid",
                    "Ellagic_Acid",
                    "Total_polyphenols",
                    "RAFSg",
                    "STASg",
                    "VERSg",
                    "Ajugose",
                    "CAMTmg",
                    "STGSTRmg",
                    "b_Sitosterol",
                    "PHYTCPPmg",
                    "Total_Saponin")

which(colnames(IN17_Renamed) == "Table 9. ORGANIC ACIDSmg")

IN17_Renamed <- IN17_Renamed %>% #renaming organic acids
  rename_at(vars(`Table 9. ORGANIC ACIDSmg`:`...179g`), ~in17_colnames) %>% 
  slice(-c(1:4, 533:535)) %>% #removing empty or useless rows
  mutate_at(4:ncol(IN17_Renamed), as.numeric) #converting components variables into numeric (needed for next steps unit conversion)

# Unit conversion and renaming of the converted variables ----

#Unit conversion and renaming of Fatty Acids 

#Identifying column numbers
which(colnames(IN17_Renamed) == "F4D0mg")
which(colnames(IN17_Renamed) == "FAPUmg")

#Applying unit conversion eq. to the identified columns
IN17_Renamed <- IN17_Renamed %>% mutate_at(72:103 , ~./1000, na.rm = T) 

#Renaming the columns according to the new units
colnames(IN17_Renamed)[72:103] <- str_replace(colnames(IN17_Renamed)[72:103], "mg", "g")

#Checking that the conversion have been done adequately
IN17_Renamed %>%  select(F18D2N6g)
IN17_Raw %>%  select(F18D2N6mg)

#Unit conversion and renaming of Amino Acids

#Applying unit conversion in the columns by name
IN17_Renamed <-IN17_Renamed %>% 
  mutate_at(vars(matches("*_100gPROTCNT")), ~.*PROCNTg*10, na.rm = T)

#Renaming the columns according to the new units
colnames(IN17_Renamed) <- str_replace(colnames(IN17_Renamed), "g_100gPROTCNT", "mg")

#Checking that the conversion have been done properly
IN17_Renamed %>% select(matches("*_100gPROTCNT"))
IN17_Raw %>% select(matches("*_100gPROTCNT"))

#Adding a new column with the FCT name
IN17_Renamed$source_fct <- "IN17"

#Checking all the variables names before saving 
IN17_Renamed %>% glimpse()

write.csv(IN17_Renamed,
          file = here::here("Output", 
                            "IN17_FCT_FAO_Tags.csv"), row.names = FALSE)

#clean environment
rm(list = ls())


