
##Run this to clean the environment
rm(list = ls())
#
# Data Compilation ----
#
# Loading libraries
## Note: if it is the first time: install.packages() first
library(dplyr) # For data cleaning (wrangling)
library(stringr) # For string manipulation (data cleaning)
library(measurements) # For unit conversion
source(here::here("functions.R")) # Loading nutrition functions (change to package when ready)
library(visdat) # Data visualisation

#0) Only run if first time or updated original FCDB scripts ----
#There are four scripts that need to run from the file
#AU19/AU19_FCT_FAO_Tags.R
#JA15/JA15_FCT_FAO_Tags.R
#US19/US19_FCT_FAO_Tags.R
#BR11/BR11_FCT_FAO_Tags.R

#To run, remove the # and run

##Checking and loading updates

#source_fct_name <- c("DK19" ,
#                     "IN17" ,
#                     "KE18" ,   
#                     "NZ18" ,
#
#                     "BA13" ,  
#                     "UF16" ,
#                     "WA19" 
#                      )
#
#
#for(i in source_fct_name){
#  source(paste0(i, "/", i, "_FCT_FAO_Tags.R"))
#}


# 1) Loading all FCDBs into one single database ----
# This is possible because all the FCTs have been standardised previously.

# finding all the cleaned FCTs/FCDBs from the output folder
list.files("Output/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, # so it is not taking the fcts in the folder
           full.names=TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"), locale = locale(encoding = "Latin1")))  

# Importing all the cleaned FCTs/FCDBs into one single object (data.frame)
fct_cover <- list.files("Output/", pattern = "*_FCT_FAO_Tags", recursive=FALSE, full.names=TRUE) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"), locale = locale(encoding = "Latin1"))) 


# Importing the ICS Codes for all the fisheries

ics_code_file <- readRDS(here::here("data", "ics-code_fish-code.RDS"))

# Checking that we have loaded all the FCT/FCDBs (n=11, excluding UK21 & NO21)
fct_cover %>% distinct(source_fct) 
names(ics_code_file )
length(unique(ics_code_file$ICS.FAOSTAT.SUA.Current.Code))
dim(ics_code_file)

# Excluding NO21 & UK21 for users illustration purposes 
subset(fct_cover, !source_fct %in% c("NO21", "UK21")) %>% distinct(source_fct) 
fct_cover <- subset(fct_cover, !source_fct %in% c("NO21", "UK21"))

fct_cover$food_desc[fct_cover$fdc_id == "10362"]
fct_cover$source_fct[fct_cover$fdc_id == "1573"]
fct_cover$scientific_name[fct_cover$fdc_id == "173712"]

# 2) Generating the Fish and Fishery FCBD ----

#├ Extracting fish entries from each FCTs/FCDBs ----

fish_fct <- fct_cover %>% 
  left_join(., ics_code_file, by = c("fdc_id", "source_fct")) %>% 
  dplyr::filter(!is.na(ICS.FAOSTAT.SUA.Current.Code))  

dim(fish_fct)

#Checking the no. of entries after filtering out all foods but fish
table(fish_fct$source_fct)
table(fish_fct$ICS.FAOSTAT.SUA.Current.Code)
length(unique(fish_fct$ICS.FAOSTAT.SUA.Current.Code))
table(fish_fct$ICS.FAOSTAT.SUA.Current.Code, fish_fct$source_fct)

fish_fct %>% group_by(ICS.FAOSTAT.SUA.Current.Code) %>% count() %>% pull() %>% max()
