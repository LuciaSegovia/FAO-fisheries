#                                                                             # 
#                                                                             # 
#                           FAO - Fisheries                                   # 
#                     Producing supporting datasets                           #     
#                                                                             #
#                                                                             #
#                                                                             #
###############################################################################



# Loading libraries
## Note: if it is the first time: install.packages() first
library(dplyr) # For data cleaning (wrangling)
library(stringr) # For string manipulation (data cleaning)

## Food matching: Fish and Fisheries ----
# Preparing data frame that was prepared for the Global FCT (only fish)
# contains information for each fish on ISSCAAP code, ICS FAOSTAT fish codes, and
# alpha-three code, when available. 

#├  FAO data - ICS codes and ISSCAAP groups  ----

#reading excel
readxl::excel_sheets(here::here("data",
                                "List_SUA_ICS_fish.xlsx"))
#loading the data
ics_code <- readxl::read_excel(here::here("data",
                                          "List_SUA_ICS_fish.xlsx"))%>% 
  janitor::clean_names() %>%                         #tidying colnames
  filter(!is.na(ics_faostat_sua_english_description)) #removing empty rows

#Fixing a typo

ics_code$ics_faostat_sua_english_description  <- gsub( "frozen, fillet", ", frozen fillet", ics_code$ics_faostat_sua_english_description)


#Checking categories w/o processing code
#Aquatic mammals and aquatic plants

ics_code %>% mutate(
  product_type = case_when( 
    str_detect(ics_faostat_sua_english_description, "fresh fillets") ~ "3", 
    str_detect(ics_faostat_sua_english_description, "frozen fillets") ~ "4", 
    str_detect(ics_faostat_sua_english_description, "fresh") ~ "1", 
    str_detect(ics_faostat_sua_english_description, "frozen") ~ "2", 
    str_detect(ics_faostat_sua_english_description, "cured") ~ "5", 
    str_detect(ics_faostat_sua_english_description, "canned") ~ "6", 
    str_detect(ics_faostat_sua_english_description, "preparations") ~ "7", 
    str_detect(ics_faostat_sua_english_description, "body oils") ~ "8", 
    str_detect(ics_faostat_sua_english_description, "liver oils") ~ "9",
    TRUE ~ "NA"
  )
) %>% filter(product_type == "NA") %>% 
  pull(ics_faostat_sua_english_description, isscaap_group)

#adding processing group no. (product_type) to ics file
#order should be from the most specific to the least

ics_code <- ics_code %>% mutate(
  product_type = case_when( 
    str_detect(ics_faostat_sua_english_description, "fresh fillets") ~ "3", 
    str_detect(ics_faostat_sua_english_description, "frozen fillets") ~ "4",
    str_detect(ics_faostat_sua_english_description, "fresh") ~ "1", 
    str_detect(ics_faostat_sua_english_description, "frozen") ~ "2", 
    str_detect(ics_faostat_sua_english_description, "cured") ~ "5", 
    str_detect(ics_faostat_sua_english_description, "canned") ~ "6", 
    str_detect(ics_faostat_sua_english_description, "preparations") ~ "7", 
    str_detect(ics_faostat_sua_english_description, "body oils") ~ "8", 
    str_detect(ics_faostat_sua_english_description, "liver oils") ~ "9",
    TRUE ~ "NA"
  )
) %>% relocate(product_type, .before = "ics_faostat_sua_english_description")

#├ Saving ICS, ISSCAAP groups & product type info (fish matching) ----

saveRDS(ics_code, file = here::here("data", "ics-code.RDS"))


#├ Loading the file ----

ics_code_file <- read.csv(here::here("data", "ics-code_fish-code.csv")) %>% 
  rename(source_fct = "Source.FCT.for.NVs") #renaming variable the FCT source (e.g. BA13, WA19)

#├ Standardising the file for FCT standard fdc_id ----

#fixing discrepancy between fcd_id in our dataframe (df) and Global FCT df for KE18 and US19
#This is needed for merging and filtering the fish and adding the ICS FAOSTAT code

ics_code_file %>% filter(str_detect(fdc_id, "^0")) 

ics_code_file <- ics_code_file %>% 
  mutate(fdc_id = ifelse(source_fct == "KE18",
                         str_replace(fdc_id, "^0", ""), fdc_id))  %>%  #removing the 0 of the fdc_id
  mutate(fdc_id = ifelse(source_fct == "US19",
                         NDB_number , fdc_id)) #using NDB_number as the fdc_id

#checking the US19 data from the FAO Global Fisheries data
ics_code_file %>% filter(source_fct == "US19")

#├  Updating fish matches ----
## Following expert advise (2023)
names(ics_code_file)
ics_code_file$Food.description[ics_code_file$ICS.FAOSTAT.SUA.Current.Code == "1532"]

#├├  Excluding: ----

# Excluded, not fresh fish (surimi)
ics_code_file$ICS.FAOSTAT.SUA.Current.Code[ics_code_file$ICS.FAOSTAT.SUA.Current.Code %in% c("1514", "1515") & 
                                             ics_code_file$fdc_id == "10200"]
ics_code_file <- subset(ics_code_file, !(ICS.FAOSTAT.SUA.Current.Code %in% c("1514", "1515") & fdc_id == "10200"))

# Excluded, frozen and not fresh fish
ics_code_file$ICS.FAOSTAT.SUA.Current.Code[ics_code_file$ICS.FAOSTAT.SUA.Current.Code %in% c("1503", "15030") &
                                             ics_code_file$fdc_id == "091019"]
ics_code_file <- subset(ics_code_file, !(ICS.FAOSTAT.SUA.Current.Code %in% c("1503", "15030") & fdc_id == "091019"))

# Duplicated item!
ics_code_file$ICS.FAOSTAT.SUA.Current.Code[ics_code_file$ICS.FAOSTAT.SUA.Current.Code %in% c("1504") &
                                             ics_code_file$fdc_id == "091019"]
subset(ics_code_file, (ICS.FAOSTAT.SUA.Current.Code %in% c("1504") & fdc_id == "091019"))

# Removing dulpicated (15)
dim(ics_code_file)
ics_code_file <- distinct(ics_code_file)

# Excluded, fish not specified in the description - Not found
#ics_code_file$ICS.FAOSTAT.SUA.Current.Code[ics_code_file$ICS.FAOSTAT.SUA.Current.Code %in% c("1532") &
 #                                            ics_code_file$fdc_id == "9003"]

# Excluded, recipe with many ingredients
ics_code_file$ICS.FAOSTAT.SUA.Current.Code[ics_code_file$ICS.FAOSTAT.SUA.Current.Code %in% c("1554") &
                                             ics_code_file$fdc_id == "15142"]
ics_code_file <- subset(ics_code_file, !(ICS.FAOSTAT.SUA.Current.Code %in% c("1554") & fdc_id == "15142"))

# Excluded, refers to fat only
ics_code_file$ICS.FAOSTAT.SUA.Current.Code[ics_code_file$ICS.FAOSTAT.SUA.Current.Code %in% c("1580") &
                                             ics_code_file$fdc_id == "11112"]
ics_code_file <- subset(ics_code_file, !(ICS.FAOSTAT.SUA.Current.Code %in% c("1580") & fdc_id == "11112"))

#Excluded food from the matching for the four SUA items - Not found
#ics_code_file$fdc_id[ICS.FAOSTAT.SUA.Current.Code$ICS.FAOSTAT.SUA.Current.Code %in% c("1505","15051","1518","1544") &
#                       ics_code_file$fdc_id == "173712"]
#ics_code_file <- subset(ics_code_file, !(ICS.FAOSTAT.SUA.Current.Code %in% c("1580") & fdc_id == "11112"))
#
#ics_code_file$source_fct[ics_code_file$ICS.FAOSTAT.SUA.Current.Code %in% c("1573")]

## Identification of the ICS fish category in the ICS file  


#├ Saving ICS and food_id (fish matching) ----

saveRDS(ics_code_file, file = here::here("data", "ics-code_fish-code.RDS"))


# Loading the data

#Original 122 columns
fao_fish <- read.csv(here::here("data", "FISHERIES-GlobalNCT_ForSharing_Feb2022.csv"))

# Checking the dataset
dim(fao_fish)
names(fao_fish)

names(fao_fish[, c(1:122)])
fao_fish[c(1:2), c(1:122)]
fao_fish[2, c(13:30)]


which(is.na(fao_fish[1, c(1:122)]))
which(fao_fish[1, c(1:122)] == "")

fao_fish[1, c(1:30)]

#Renaming variables

n <- which(fao_fish[1, c(1:122)] == "")

which(fao_fish[2, c(1:122)] == "Quality rating for food match")

fao_fish[1, n]
fao_fish[1, c(1:122)[!c(1:122) %in% n]]

#Renaming ICS codes variables
names(fao_fish)[1:6] <- fao_fish[2, c(1:6)]
#Renaming nutrients
names(fao_fish)[(7:122)[!c(7:122) %in% n]] <- fao_fish[1, c(7:122)[!c(7:122) %in% n]]
names(fao_fish)

# Getting info on variable: "Edible coefficient to be used" to be added to our dataset

which(fao_fish[2, c(1:122)] == "Edible coefficient to be used")

n <- which(fao_fish[, 27] != "")

#There is one duplicated - 1509
dim(fao_fish[n, c(1,27)])
dim(unique(fao_fish[n, c(1,27)]))

edible_ics <- unique(fao_fish[n, c(1,27)])

edible_ics[,1] <- paste("SUMMARY ROW -", edible_ics[,1])

# Save Edible coefficient to be used" to a R file - for formatting.
saveRDS(edible_ics[c(2:96),], 
        file = "data/edible_coefficient.rds")


# Getting the info for each ICS FAOSTAT category info (n=95)
#there seems to be duplicates (n=104)
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])

#Checking duplicated
duplicated(unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[1])
#Sequence of no. seems like a typo from excel
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[c(32:40),]
#Wrong code for one item
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[c(85:87),]

# Correcting column 5 () - 1518 == 15180 (were coded as seq. 15180:15190)
fao_fish[,5][fao_fish[,1] == "1518"] <- 15180
# Correcting column 5 () - 1557 == 15570 (was coded as 15530) 
fao_fish[,5][fao_fish[,1] == "1557"] <- 15570

# Checking duplicates w/i each category
#fao_fish %>% group_by(`ICS FAOSTAT SUA Current Code`) %>% 
#  count(Source, fdc_id) %>% 
#  filter(n>1) %>% 
#  arrange(desc(n))

#Save this one to complete the FAO table. 
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])
n <- nrow(unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)]))
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[c(3:n),]


# Save ICS info  to a R file - for formatting.
saveRDS(unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[c(3:n),], 
        file = "data/fao-ics-desc.rds")



