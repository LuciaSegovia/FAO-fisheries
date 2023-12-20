
##Run this to clean the environment
rm(list = ls())
#
# Data Compilation ----
#
# Loading libraries
## Note: if it is the first time: install.packages() first
library(dplyr) # For data cleaning (wrangling)
library(stringr) # For string manipulation (data cleaning)
library(purrr) # Map function
library(readr) # Reading data in
library(measurements) # For unit conversion
# source(here::here("functions.R")) # Loading nutrition functions (change to package when ready)
library(visdat) # Data visualisation

# 0) Only run if first time or updated original FCDB scripts ----
# There are four scripts that need to run from the file
# AU19/AU19_FCT_FAO_Tags.R
# JA15/JA15_FCT_FAO_Tags.R
# US19/US19_FCT_FAO_Tags.R
# BR11/BR11_FCT_FAO_Tags.R


# To run, remove the # and run

##Checking and loading updates

#source_fct_name <- c("DK19" ,
#                     "IN17" ,
#                     "KE18" ,   
#                     "NZ18" ,
#                     "UK21" ,
#                     "BA13" ,  
#                     "UF16" ,
#                     "WA19" ,
#                     "NO21")
#
#
#for(i in source_fct_name){
#  source(paste0(i, "/", i, "_FCT_FAO_Tags.R"))
#}


# 1) Loading all FCDBs into one single database ----

# finding all the cleaned FCTs/FCDBs from the output folder
# list.files("Output/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, #so it is not taking the fcts in the folder
#            full.names=TRUE) %>% 
#   map_df(~read_csv(., col_types = cols(.default = "c"), locale = locale(encoding = "Latin1")))  

# saving all the cleaned FCTs/FCDBs into one single object (data.frame)
fct_cover <- list.files("Output/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, full.names=TRUE) %>% 
 map_df(~read_csv(., col_types = cols(.default = "c"), locale = locale(encoding = "Latin1"))) 

# checking that we have loaded all the FCT/FCDBs (n=13)
fct_cover %>% distinct(source_fct) 
colnames(fct_cover)

# creating a vector with all the variables of interest
# identification variables, components that were included in the Global FCT plus new components

col_names <- c("fdc_id",
               "food_desc",
               "food_group",
               "scientific_name",
               "ISSCAAP",
               "alpha_code", 
               "source_fct",
               "nutrient_data_source",
               "Edible_factor_in_FCT",
               "ICS_FAOSTAT", # we need for NO21
               #"Edible_desc",
               "specific_gravity",
               "SOPg",
               "ASHg",
               #"ASHg_bydiff",
               "ENERCkJ",
               "ENERCkcal",
               "WATERg",
               "PROCNTg",
               "NTg",
               "XN",
               "FATg",
               "FAT_g",
               "FATCEg",
               "CHOAVLg",
               "CHOAVLDFg",
               "CHOAVLMg",
               "CHOCDFg",
               "FIBTGg",
               "FIBCg",
               "NSPg",
               "ALCg",
               "ALCg_100mL",
               "SUGARg",  
               "FASATg",
               "FAMSg",
               "FAPUg",
               "FATRNg",
               "F22D6N3g",
               "F20D5N3g",
               "CHOLEmg",
               "CHOL_mg",
               "RETOLmcg",
               "VITAmcg",
               "VITA_RAEmcg",
               "CARTBEQmcg",
               "CARTAmcg",
               "CARTBmcg",
               "CRYPXBmcg",
               "VITEmg",
               "TOCPHAmg",
               "TOCPHBmg",
               "TOCPHGmg",
               "TOCPHDmg",
               "TOCTRAmg",
               "TOCTRBmg",
               "TOCTRGmg",
               "THIAmg",
               "THIAHCLmg",
               "RIBFmg",
               "VITB6Amg",
               "VITB6Cmg",
               "VITB6_mg",
               "FOLmcg",
               "FOLACmcg",
               "FOLFDmcg",
               "FOLSUMmcg",
               "FOL_mcg",
               "NIAEQmg",
               "NIAmg",
               "NIATRPmg",
               "TRPmg",
               "FOLDFEmcg",
               "VITB12mcg",
               "VITCmg",
               "ASCLmg",
               "VITDEQmcg",
               "VITDmcg",
               "CHOCALmcg",
               "ERGCALmcg",
               "CHOCALOHmcg",
               "ERGCALOHmcg",
               "CAmg",
               "MGmg",
               "MNmg",
               "Pmg",
               "FEmg",
               "NAmg",
               "Kmg",
               "CUmg",
               "ZNmg",
               "SEmcg",
               "IDmcg")

#checking and counting No. of items (before filtering only fish)
fct_cover %>% dplyr::select(col_names) %>% 
  count(source_fct) 

#Filtering out components that are not used and removing "_FCT" from the FCTs/FCDB name
#added quality for NO21
#fct_cover <- fct_cover %>% select(col_names, quality) %>% 
#  mutate_at("source_fct", ~str_replace(., "_FCT", "")) 

#Checking that we have all the variables of interest
fct_cover %>% str()

# 2) Generating the Fish and Fishery FCBD ----

#├ Extracting fish entries from each FCTs/FCDBs ----

#├├ Filtering only fish in all FCTs/FCDBs ----

#checking fish entries in the UK21 (n=277)
fct_cover %>% filter(source_fct == "UK21",
                     food_group %in% c("JA", "JC", "JK", "JM", "JR")) 

fish_fct <- fct_cover %>% 
  left_join(., ics_code_file, by = c("source_fct",
                                     "fdc_id")) %>% 
  filter(!is.na(ICS.FAOSTAT.SUA.Current.Code) | source_fct %in% c("NO21") |
           food_group %in% c("JA", "JC", "JK", "JM", "JR")) 

# checking the ICS FAO code in NO21
fish_fct %>% filter(!is.na(ICS_FAOSTAT)) %>% distinct(ICS_FAOSTAT)

fish_fct %>% filter(source_fct == "NO21") %>% distinct(ICS_FAOSTAT)

#Getting the ICS FAOSTAT code, ISSCAAP, and alpha code  for NO21 that 
#was added "manually"
fish_fct <- fish_fct %>% 
  mutate(ICS.FAOSTAT.SUA.Current.Code = ifelse(source_fct == "NO21",
                                               ICS_FAOSTAT,
                                               ICS.FAOSTAT.SUA.Current.Code)) %>% 
  mutate(ISSCAAP.Group = ifelse(source_fct == "NO21",
                                ISSCAAP,
                                ISSCAAP.Group)) %>% 
  mutate(X3.alpha.code = ifelse(source_fct == "NO21",
                                alpha_code,
                                X3.alpha.code)) %>% 
  select(-c(ISSCAAP, alpha_code))

#Checking the no. of entries after filtering out all foods but fish
fish_fct %>% group_by(source_fct) %>% count()

#Checking the no. of fish entries in  the original FAO file used for filtering
ics_code_file  %>% group_by(source_fct) %>% count()
dim(ics_code_file) #checking rows (n=4643) and column 

#├ Adding ICS FAOSTAT description for the fish entries ----

#Checking entries w/o ICS FAOSTAT code
fish_fct %>% filter(is.na(ICS.FAOSTAT.SUA.Current.Code)) %>% count(source_fct)

##├├ Preparing the ICS FAOSTAT (df) with the descriptions ----

#loading file with the ICS FAOSTAT code and description 

ics_code <- readxl::read_excel(here::here("data",
                                          "List_SUA_ICS_fish.xlsx")) %>% 
  janitor::clean_names() #cleaning variables names

#Fixing a typo in the ICS FAOSTAT description
ics_code$ics_faostat_sua_english_description[ics_code$ics_faostat_sua_english_description == "Demersal fish frozen, fillets"] <- "Demersal fish, frozen, fillets"

#Selecting only fish entries with ICS FAOSTAT code and
#generating a new variable with no missing values for the ICS FAOSTAT code
ics_code <- ics_code %>% 
  filter(!is.na(ics_faostat_sua_english_description)) %>% #filtering out two empty 
  mutate(ics_faostat_sua_code =
           ifelse(is.na(ics_faostat_sua_current_code), #combining ics code of "current" and 
             ics_faostat_sua_future_code,  #those that are only "future ics code"
                ics_faostat_sua_current_code)) %>% 
  mutate_at("ics_faostat_sua_code", as.factor) #converting ICS code variable into a factor 

##├├ Adding ICS FAOSTAT fish description

#converting ICS code variable into a factor 
fish_fct$ICS.FAOSTAT.SUA.Current.Code <- as.factor(fish_fct$ICS.FAOSTAT.SUA.Current.Code) 

#adding the ICS FAOSTAT description by joining two datasets
#filtering out entries from NO21 
#fish_fct %>% filter(source_fct == "NO21", is.na(ICS_FAOSTAT))
#and UK21
#fct_cover %>% filter(source_fct == "UK21",
#food_group %in% c("JA", "JC", "JK", "JM", "JR")) 


fao_fish_fct <- fish_fct %>%
  filter(!is.na(ICS.FAOSTAT.SUA.Current.Code)) %>% #filtering fish with ICS code
 # select(-ics_faostat_sua_english_description) %>%  #removing ICS code from NO21 (only needed when no filtering colnames)
  left_join(., ics_code %>%             #joining two datasets
              select(ics_faostat_sua_code, ics_faostat_sua_english_description), #selecting variables
            by = c("ICS.FAOSTAT.SUA.Current.Code" = "ics_faostat_sua_code")) #variables for joining the two datasets

#Checking the results

glimpse(fao_fish_fct)

#Total fish entries - entries w/o ICS == to final count
(count(fish_fct) - (277+41)) == count(fao_fish_fct)



# 4) Visualisation of results and QC ----
#Check "visualisation.R" and "QC.R"

# 4.1) Generating a dataset for visualization and analysis ----

#├ Generating a two grouping variables: Fish type and Fish prep  ----

#last column in the dataset (for relocating variables)
n1 <- length(fao_fish_fct)
#last column + the new two grouping variables (for relocating)
n2<- n1+2

fao_fish_fct <- fao_fish_fct %>%
  cbind(str_split_fixed(fao_fish_fct$ics_faostat_sua_english_description, ", " , n=2)) %>% 
  rename(fish_type = "1", 
         fish_prep = "2")  %>% 
  relocate(c(n1:n2), .before = fdc_id) %>% 
  mutate_at("fish_prep", str_squish)

# Check the grouping variables
fao_fish_fct %>% count(fish_type, fish_prep) %>% arrange(desc(n))

#Check ISSCAAP code and groups
colnames(fao_fish_fct)
subset(fao_fish_fct, is.na(ISSCAAP.Group))

#Checking scientific names
subset(fao_fish_fct, !is.na(scientific_name)) %>% 
  group_by(source_fct) %>% count()

table(!is.na(fao_fish_fct$scientific_name), fish_fct$source_fct)

saveRDS(fao_fish_fct, here::here("data", "FAO-fish-standardised-updated_v1.0.0.RDS"))

