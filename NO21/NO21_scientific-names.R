##Run this to clean the environment
rm(list = ls())
#
# Data Import ----
#
# Loading libraries
## Note: if it is the first time: install.packages() first
library(dplyr) # For data cleaning (wrangling)
library(stringr) # For string manipulation (data cleaning)

#├  Norwegian FCDB data - Scientific names ----

#reading excel
readxl::excel_sheets(here::here("NO21",
                                "Scientific-name_Norwegian-FCDB.xlsx"))

#loading the data
sci_no21 <- readxl::read_excel(here::here("NO21",
                                          "Scientific-name_Norwegian-FCDB.xlsx"),
                               sheet = 1) %>% 
  janitor::clean_names() 



#├ Tidying the scientific names dataset (NO21)  ----

# Checking data structure
head(sci_no21)

sci_no21 %>% separate(x4,c("name2", "ref"),
                      sep = " ") %>% filter(str_detect(name2, "^[:lower:]"))

#Getting one column with the scientific name and one column with the ref. for 
#the scientific name.

sci_no21 <- sci_no21 %>% separate(x4,c("name2", "ref", "X5"),
                                  sep = " ") %>% 
  mutate(Scientific_name = ifelse(str_detect(name2, "^[:lower:]"),
                                  paste(scientific_name, name2), 
                                  scientific_name),
         Ref = ifelse(!is.na(X5),
                      paste(ref, X5), 
                      ifelse(str_detect(name2, "^[:upper:]"),
                             paste(name2, ref),
                             ref))) %>% 
  select(1:2, Scientific_name, Ref)

#Checking the result
head(sci_no21)

sci_no21 <- sci_no21 %>% rename(fdc_id = "x1")

names(sci_no21)


# Saving clean Scientific names (fish) ----

saveRDS(sci_no21, here::here("NO21", "scientific-name_NO21.RDS"))