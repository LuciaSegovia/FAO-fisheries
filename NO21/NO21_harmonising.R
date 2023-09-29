


# 3. Data compilation and harmonisation ----

## Extracting only fishery products 

#Checking food groups
data.df %>% 
  filter(str_detect(fdc_id, "^[:digit:]{1}$"))

#Identifying the location (row no.) of the fishery products
row1 <- which(data.df$fdc_id == "4") # 459
row2 <- which(data.df$fdc_id == "5") # 702

#Selecting only fish
no21_fish <- data.df %>% slice(row1:row2) %>%
  filter(!is.na(WATERg)) # To remove food category & sub-category rows.

#Checking the result
colnames(no21_fish)
head(sci_no21)
dim(no21_fish) #232


#├  3.4. Food name/ description standardisation and food matching ----


#├├  Merging fish names w/ scientific names (no21)  ----

### List of fishery entries without Scientific name
no21_fish %>% left_join(., sci_no21, by = "fdc_id") %>% 
  mutate(Scientific_name = case_when(
    str_detect(food_desc, "cod|Cod|Stockfish") ~ "Gadus morhua",
    str_detect(food_desc, "Anchovy") ~ "Engraulis encrasicolus",
    str_detect(food_desc, "Saithe|saithe") ~ "Pollachius virens",
    str_detect(food_desc, "Capelin|capelin") ~ "Mallotus villosus",
    str_detect(food_desc, "Polar|polar") ~ "Boreogadus saida",
    str_detect(food_desc, "Salmon|salmon") ~ "Salmo salar",
    str_detect(food_desc, "Herring|herring") ~ "Clupea harengus", 
    str_detect(food_desc, "Mackerel|mackerel") ~ "Scomber scombrus", 
    str_detect(food_desc, "Lobster|lobster") ~ "Homarus gammarus",
    TRUE ~ Scientific_name)) %>% filter(is.na(Scientific_name))


## Fixing scientific names by fish name (NO21) 
no21_fish <- no21_fish %>% left_join(., sci_no21, by = "fdc_id") %>% 
  mutate(Scientific_name = case_when(
    str_detect(food_desc, "cod|Cod|Stockfish") ~ "Gadus morhua",
    str_detect(food_desc, "Anchovy") ~ "Engraulis encrasicolus",
    str_detect(food_desc, "Saithe|saithe") ~ "Pollachius virens",
    str_detect(food_desc, "Capelin|capelin") ~ "Mallotus villosus",
    str_detect(food_desc, "Polar|polar") ~ "Boreogadus saida",
    str_detect(food_desc, "Salmon|salmon") ~ "Salmo salar",
    str_detect(food_desc, "Herring|herring") ~ "Clupea harengus", 
    str_detect(food_desc, "Mackerel|mackerel") ~ "Scomber scombrus", 
    str_detect(food_desc, "Lobster|lobster") ~ "Homarus gammarus",
    TRUE ~ Scientific_name)) %>% 
  select(-name)


#checking results
head(no21_fish)
dim(no21_fish) #shouldn't add/remove any obs (rows) but add 2 variables (cols)
subset(no21_fish, !is.na(Scientific_name)) #No of fish w/ scientific name

## Merging the corresponding ISSCAAP to Scientific names 

#checking variables in isscaap
str(isscaap)
#checking variable names in no21_fish
colnames(no21_fish)

## Merging the two datasets and tidying the dataset

no21_fish <- no21_fish %>% 
  left_join(., isscaap %>% select(1:5)) %>%
  relocate(any_of(c("Scientific_name", "Ref", "ISSCAAP")),
           .after = food_desc)

#checking results
head(no21_fish)
dim(no21_fish) # shouldn't add/remove any obs (rows) but add 4 variables (cols)

#├├ Fixing ISSCAAP code by Scientific name ----

#Checking the fish with scientific name w/o ISSCAAP
no21_fish %>% filter(!is.na(Scientific_name), is.na(ISSCAAP)) %>% 
  distinct(fdc_id, Scientific_name) 

#List of fish with Scientific name w/o ISSCAAP
fish.name <- no21_fish  %>% filter(!is.na(Scientific_name), is.na(ISSCAAP)) %>% 
  distinct(Scientific_name) %>% pull()

#Identifying the ISSCAAP of the fish by fist name of the fish w/o ISSCAAP
fish.isscaap <- isscaap %>% 
  filter(str_detect(Scientific_name, "Theragra")) %>%
  distinct(ISSCAAP) %>% pull()
fish.isscaap[2] <-  isscaap %>% 
  filter(str_detect(Scientific_name, "Anarhichas")) %>% 
  distinct(ISSCAAP) %>% pull()
fish.isscaap[3] <- isscaap %>%
  filter(str_detect(Scientific_name, "Pangasius")) %>% 
  distinct(ISSCAAP) %>% pull()
fish.isscaap[4] <- isscaap %>% 
  filter(str_detect(Scientific_name, "Sebastes")) %>% 
  distinct(ISSCAAP) %>% pull()
#isscaap %>% filter(str_detect(Scientific_name, "maxima")) 
fish.isscaap[5] <- isscaap %>%
  filter(str_detect(English_name, "Turbot")) %>% 
  distinct(ISSCAAP) %>% pull()

#Adding the ISSCAAP codes for the fish identified above
for(i in 1:length(fish.name)){
  no21_fish$ISSCAAP[grep(fish.name[i], no21_fish$Scientific_name)] <- fish.isscaap[i]
  print(i)}

#checking results
head(no21_fish)
dim(no21_fish) #shouldn't add/remove any obs (rows) but add 4 variables (cols)

# Checking results:  all entries w/ scientific name have an ISSCAAP code
no21_fish %>% filter(!is.na(Scientific_name), is.na(ISSCAAP)) %>% 
  distinct(fdc_id, Scientific_name)

#├├ Fixing ISSCAAP code by fish name ----

# List of fishery products w/o ISSCAAP
no21_fish %>% filter(is.na(ISSCAAP)) %>% 
  distinct(fdc_id, food_desc)

no21_fish %>% filter(is.na(ISSCAAP)) %>% 
  distinct(fdc_id, food_desc) %>% arrange(food_desc) %>% knitr::kable()

#checking fish names w/o ISSCAAP code
no21_fish %>% filter(is.na(ISSCAAP)) %>%  
  pull(food_desc) %>% str_extract_all(., "^[:upper:][:alpha:]{1,}")

no21_fish %>% 
  filter(str_detect(food_desc, "Shrimp|shrimp"))

#Fixing ISSCAAP code by fish names
no21_fish <-  no21_fish %>% 
  mutate(ISSCAAP = ifelse(is.na(ISSCAAP), case_when(
    str_detect(food_desc, "Crab,|crab") ~ "42",
    str_detect(food_desc, "King|king") ~ "45",
    str_detect(food_desc, "Shrimp|shrimp") ~ "45", 
    TRUE ~ ISSCAAP), ISSCAAP))

# Checking results:  entries w/  ISSCAAP code

no21_fish %>% filter(!is.na(ISSCAAP)) 
dim(no21_fish)

#Saving into csv to inspect the ISSCAAP code allocations. 
# no21_fish %>% select(fdc_id:ref_116) %>% 
#  write.csv(., here::here("inter-output", "NorwegianFCBD_fish_isscaap.csv"),
#   row.names = F)

#├├  Identifying the processing category of the fish ----

## Identification of the ICS fish category in the NO21  

dim(no21_fish)
# Raw  (1)
no21_fish  %>% 
  filter(str_detect(food_desc, " raw")) %>% 
  filter(!str_detect(food_desc, "slice|fillet")) %>%
  pull(food_desc)

#Frozen  (2, 4)
#No frozen fish, only preparations
#Note: crabsticks are considered "Demersal fish, preparations
no21_fish  %>% 
  filter(str_detect(food_desc, "Frozen|frozen")) %>%
  pull(food_desc)

#Fillet (3, 4)
#No fish fillet, raw only preparations and one "Cod, slices, raw" 
no21_fish %>% 
  filter(str_detect(food_desc, " raw")) %>%
  filter(str_detect(food_desc, "Fillet|fillet|slice|Slice")) %>%
  pull(food_desc)

#Cured (5)
no21_fish  %>% 
  filter(str_detect(food_desc, "Cure|cure|smoke|Smoke|salt|Salt|dried|Dried")) %>%
  pull(food_desc)

#Canned (6)
no21_fish  %>% 
  filter(str_detect(food_desc, "can|Can")) %>%
  pull(food_desc)

#Preparations, nei (7)
no21_fish  %>% 
  filter(!str_detect(food_desc, " raw")) %>%
  filter(!str_detect(food_desc,
                     "Cure|cure|smoke|Smoke|salt|Salt|dried|Dried")) %>%
  filter(!str_detect(food_desc, "can|Can")) %>%
  pull(food_desc)

#Oil (8, 9)
#No fish or liver oil 
no21_fish  %>% 
  filter(str_detect(food_desc, "Oil| oil|Fat|fat")) %>%
  pull(food_desc)

##  Adding the preparation code (1-9) 

no21_fish  <- no21_fish   %>% mutate(
  product_type = case_when(
    str_detect(food_desc, " raw") & 
      str_detect(food_desc, "slice|fillet|added|cured|roe", negate = TRUE) ~ "1",
    str_detect(food_desc, "Cure|cure|smoke|Smoke|salt|Salt|dried|Dried") ~"5",
    str_detect(food_desc, " raw|roe|Roe", negate = T) &  #Canned are only fish, not roe
      str_detect(food_desc, "Canned|canned") ~"6",
    TRUE ~ "NA"
  )) %>%
  mutate(product_type = ifelse(product_type == "NA", "7", product_type))

dim(no21_fish)
#Adding the fillet
no21_fish$product_type[no21_fish$food_desc == "Cod, slices, raw"] <- "3"

#Checking the processing allocation
no21_fish  %>% filter(product_type == "1") %>%
  pull(food_desc)

#Checking roes
no21_fish  %>% 
  filter(str_detect(food_desc, "Roe|roe"), product_type != "7") %>% 
  select(fdc_id, food_desc, product_type)

#Checking the Special cases
moll <-  c(51, 52, 53, 54, 55, 56, 59)
aqua_mam <- c(61, 62, 63, 64)
aqua_anim <- c(71, 72, 74, 75, 76, 77)
aqua_plan <-  c(91, 92, 93, 94)

no21_fish  %>% filter(ISSCAAP %in% aqua_plan) %>%
  pull(food_desc)

#Molluscs - there is no prep, so it need to be re-allocated
#product_type 3, 4, 7, 8, 9 should be zero
no21_fish  %>% filter(ISSCAAP %in% moll, product_type == "7") %>%
  pull(food_desc)

#Re-allocating products in group 7 to its corresponding
no21_fish$product_type[no21_fish$food_desc == "Scallop, boiled"] <- "6"
no21_fish$product_type[no21_fish$food_desc == "Periwinkle, common, Norwegian"] <- "1"


#Checking the fishery dataset
head(no21_fish)
dim(no21_fish)

#Checking raw and fillet, fresh
no21_fish  %>% filter(product_type %in% c("1", "3"))

#Adding "frozen" from "raw" and tidying the column order
no21_fish <- no21_fish  %>% filter(product_type %in% c("1", "3")) %>%
  mutate(product_type = ifelse(product_type == "1", 
                               "2", "4")) %>% 
  rbind(., no21_fish) %>% 
  relocate("product_type", .after = Scientific_name) %>% 
  arrange(fdc_id, product_type)


#Checking that the raw to frozen was added to fishery dataset
head(no21_fish)
dim(no21_fish) #this should be +21 "new" entries


#prep <- c("fresh", "frozen", "fresh fillets", "frozen fillets", 
#         "cured", "canned",
#        "preparations, nei", "body oils", "liver oils")

#├├  Matching NO21 fish to ICS FAOSTAT fish category ----

isscaap_group <- list(unique(ics_code$isscaap_group[grepl("Freshwater & ",
                                                          ics_code$ics_faostat_sua_english_description)]))

isscaap_group[2] <- list(unique(ics_code$isscaap_group[grepl("Freshwater fish",
                                                             ics_code$ics_faostat_sua_english_description)]))

#ISSCAAP of each fish category (e.g, Freshwater & diadromous fish)
fresh_dia <- c(11, 12, 13, 21, 22, 23, 24, 25)
deme <- c(31, 32, 33, 34, 38)
pela <- c(35, 36, 37)
marine <- 39
crus <- c(41, 42, 43, 44, 45, 46, 47 )
moll <-  c(51, 52, 53, 54, 55, 56, 59)
cepha <- 57
fresh <- c(11, 12, 13)
dia <- c(21, 22, 23, 24, 25)
small_pela <-  c(35, 37)
other_pela <-  36

#Checking ICS FAOSTAT codes (current) by fish category
ics_code %>% 
  filter(str_detect(ics_faostat_sua_english_description, "Mollus"))%>% 
  pull(ics_faostat_sua_current_code, ics_faostat_sua_english_description)

#├├ Allocating the ICS FAOSTAT "current" code  ----
#based on ISSCAAP code of each category (see above, line 442-448)
#and preparation category (product_type)

no21_fish <- no21_fish %>% relocate(product_type, .before = "ISSCAAP") %>% 
  mutate(
    ICS_FAOSTAT = case_when(
      ISSCAAP %in% fresh_dia & product_type == 1 ~ "1501", 
      ISSCAAP %in% fresh_dia & product_type == 2 ~ "1502", 
      ISSCAAP %in% fresh_dia & product_type == 3 ~ "1503", 
      ISSCAAP %in% fresh_dia & product_type == 4 ~ "1504", 
      ISSCAAP %in% fresh_dia & product_type == 5 ~ "1505", 
      ISSCAAP %in% fresh_dia & product_type == 6 ~ "1506", 
      ISSCAAP %in% fresh_dia & product_type == 7 ~ "1507", 
      TRUE ~ "NA")) %>%   
  mutate(
    ICS_FAOSTAT = case_when(
      ISSCAAP %in% deme & product_type == 1 ~ "1514", 
      ISSCAAP %in% deme & product_type == 2 ~ "1515", 
      ISSCAAP %in% deme & product_type == 3 ~ "1516", 
      ISSCAAP %in% deme & product_type == 4 ~ "1517", 
      ISSCAAP %in% deme & product_type == 5 ~ "1518", 
      ISSCAAP %in% deme & product_type == 6 ~ "1519", 
      ISSCAAP %in% deme & product_type == 7 ~ "1520", 
      TRUE ~ ICS_FAOSTAT)) %>%
  mutate(
    ICS_FAOSTAT = case_when(
      ISSCAAP %in% pela & product_type == 1 ~ "1527", 
      ISSCAAP %in% pela & product_type == 2 ~ "1528", 
      ISSCAAP %in% pela & product_type == 3 ~ "1529", 
      ISSCAAP %in% pela & product_type == 4 ~ "1530", 
      ISSCAAP %in% pela & product_type == 5 ~ "1531", 
      ISSCAAP %in% pela & product_type == 6 ~ "1532", 
      ISSCAAP %in% pela & product_type == 7 ~ "1533", 
      TRUE ~ ICS_FAOSTAT))  %>% 
  mutate(
    ICS_FAOSTAT = case_when(
      ISSCAAP %in% marine & product_type == 1 ~ "1540", 
      ISSCAAP %in% marine & product_type == 2 ~ "1541", 
      ISSCAAP %in% marine & product_type == 3 ~ "1542", 
      ISSCAAP %in% marine & product_type == 4 ~ "1543", 
      ISSCAAP %in% marine & product_type == 5 ~ "1544", 
      ISSCAAP %in% marine & product_type == 6 ~ "1545", 
      ISSCAAP %in% marine & product_type == 7 ~ "1546", 
      TRUE ~ ICS_FAOSTAT)) %>%
  mutate(
    ICS_FAOSTAT = case_when(
      ISSCAAP %in% crus & product_type == 1 ~ "1553", 
      ISSCAAP %in% crus & product_type == 2 ~ "1554", 
      ISSCAAP %in% crus & product_type == 5 ~ "1555", 
      ISSCAAP %in% crus & product_type == 6 ~ "1556", 
      ISSCAAP %in% crus & product_type == 7 ~ "1557", 
      TRUE ~ ICS_FAOSTAT)) %>%
  mutate(
    ICS_FAOSTAT = case_when(
      ISSCAAP %in% moll & product_type == 1 ~ "1562", 
      ISSCAAP %in% moll & product_type == 2 ~ "1563", 
      ISSCAAP %in% moll & product_type == 5 ~ "1564", 
      ISSCAAP %in% moll & product_type == 6 ~ "1565", 
      TRUE ~ ICS_FAOSTAT)) %>%
  mutate(
    ICS_FAOSTAT = case_when(
      ISSCAAP %in% cepha & product_type == 1 ~ "1570", 
      ISSCAAP %in% cepha & product_type == 2 ~ "1571", 
      ISSCAAP %in% cepha & product_type == 5 ~ "1572", 
      ISSCAAP %in% cepha & product_type == 6 ~ "1573", 
      ISSCAAP %in% cepha & product_type == 7 ~ "1574", 
      TRUE ~ ICS_FAOSTAT)) 

#Checking the results - All items w/ ISSCAAP and product_type has a ICS code
no21_fish %>% filter(!is.na(ISSCAAP), !is.na(product_type),
                     ICS_FAOSTAT == "NA")

#Checking the results - Missing ICS code (overall)
no21_fish %>% filter(ICS_FAOSTAT == "NA") %>%  pull(food_desc)


#Checking ICS FAOSTAT codes (future) by fish category
ics_code %>% 
  filter(str_detect(ics_faostat_sua_english_description, "Cepha"))%>% 
  pull(ics_faostat_sua_future_code, ics_faostat_sua_english_description)

#├├  Allocating the ICS FAOSTAT "future" code ---- 
#based on ISSCAAP code of each category (see above, line 445-452)
#and preparation category (product_type) 

no21_fish <- no21_fish %>% 
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% fresh & product_type == 1 ~ "15010", 
      ISSCAAP %in% fresh & product_type == 2 ~ "15020", 
      ISSCAAP %in% fresh & product_type == 3 ~ "15030", 
      ISSCAAP %in% fresh & product_type == 4 ~ "15040", 
      ISSCAAP %in% fresh & product_type == 5 ~ "15050", 
      ISSCAAP %in% fresh & product_type == 6 ~ "15060", 
      ISSCAAP %in% fresh & product_type == 7 ~ "15070", 
      TRUE ~ "NA"))  %>%
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% dia & product_type == 1 ~ "15011", 
      ISSCAAP %in% dia & product_type == 2 ~ "15021", 
      ISSCAAP %in% dia & product_type == 3 ~ "15031", 
      ISSCAAP %in% dia & product_type == 4 ~ "15041", 
      ISSCAAP %in% dia & product_type == 5 ~ "15051", 
      ISSCAAP %in% dia & product_type == 6 ~ "15061", 
      ISSCAAP %in% dia & product_type == 7 ~ "15071", 
      TRUE ~ ICS_FAOSTAT_future))  %>%
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% deme & product_type == 1 ~ "15140", 
      ISSCAAP %in% deme & product_type == 2 ~ "15150", 
      ISSCAAP %in% deme & product_type == 3 ~ "15160", 
      ISSCAAP %in% deme & product_type == 4 ~ "15170", 
      ISSCAAP %in% deme & product_type == 5 ~ "15180", 
      ISSCAAP %in% deme & product_type == 6 ~ "15190", 
      ISSCAAP %in% deme & product_type == 7 ~ "15200", 
      TRUE ~ ICS_FAOSTAT_future)) %>% 
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% small_pela & product_type == 1 ~ "15270", 
      ISSCAAP %in% small_pela & product_type == 2 ~ "15280", 
      ISSCAAP %in% small_pela & product_type == 3 ~ "15290", 
      ISSCAAP %in% small_pela & product_type == 4 ~ "15300", 
      ISSCAAP %in% small_pela & product_type == 5 ~ "15310", 
      ISSCAAP %in% small_pela & product_type == 6 ~ "15320", 
      ISSCAAP %in% small_pela & product_type == 7 ~ "15330", 
      TRUE ~ ICS_FAOSTAT_future))  %>%
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% other_pela & product_type == 1 ~ "15271", 
      ISSCAAP %in% other_pela & product_type == 2 ~ "15281", 
      ISSCAAP %in% other_pela & product_type == 3 ~ "15291", 
      ISSCAAP %in% other_pela & product_type == 4 ~ "15301", 
      ISSCAAP %in% other_pela & product_type == 5 ~ "15311", 
      ISSCAAP %in% other_pela & product_type == 6 ~ "15321", 
      ISSCAAP %in% other_pela & product_type == 7 ~ "15331", 
      TRUE ~ ICS_FAOSTAT_future)) %>%
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% marine & product_type == 1 ~ "15400", 
      ISSCAAP %in% marine & product_type == 2 ~ "15410", 
      ISSCAAP %in% marine & product_type == 3 ~ "15420", 
      ISSCAAP %in% marine & product_type == 4 ~ "15430", 
      ISSCAAP %in% marine & product_type == 5 ~ "15440", 
      ISSCAAP %in% marine & product_type == 6 ~ "15450", 
      ISSCAAP %in% marine & product_type == 7 ~ "15460", 
      TRUE ~ ICS_FAOSTAT_future)) %>% 
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% crus & product_type == 1 ~ "15530", 
      ISSCAAP %in% crus & product_type == 2 ~ "15540", 
      ISSCAAP %in% crus & product_type == 5 ~ "15550", 
      ISSCAAP %in% crus & product_type == 6 ~ "15560", 
      ISSCAAP %in% crus & product_type == 7 ~ "15570", 
      TRUE ~ ICS_FAOSTAT_future)) %>%
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% moll & product_type == 1 ~ "15620", 
      ISSCAAP %in% moll & product_type == 2 ~ "15630", 
      ISSCAAP %in% moll & product_type == 5 ~ "15640", 
      ISSCAAP %in% moll & product_type == 6 ~ "15650", 
      TRUE ~ ICS_FAOSTAT_future))  %>%
  mutate(
    ICS_FAOSTAT_future = case_when(
      ISSCAAP %in% cepha & product_type == 1 ~ "15700", 
      ISSCAAP %in% cepha & product_type == 2 ~ "15710", 
      ISSCAAP %in% cepha & product_type == 5 ~ "15720", 
      ISSCAAP %in% cepha & product_type == 6 ~ "15730", 
      ISSCAAP %in% cepha & product_type == 7 ~ "15740", 
      TRUE ~ ICS_FAOSTAT_future)) 

dim(no21_fish)
#Checking the results - All items w/ ISSCAAP and product_type has a ICS code
no21_fish %>% filter(!is.na(ISSCAAP), !is.na(product_type),
                     ICS_FAOSTAT_future == "NA")

#Checking the results - Missing ICS code (overall)
no21_fish %>% filter(ICS_FAOSTAT_future == "NA") %>%  pull(food_desc)


#Checking some names w/o ICS code
no21_fish %>% 
  filter(str_detect(food_desc, "Snails, canned"))

subset(no21_fish, fdc_id %in% c("04.265", "04.079"))

#├├ Adding extra ICS FAOSTAT codes - manually ----

# Excluded: Crabstick, fish balls (acc. to FAO advise) from 1520

no21_fish <- no21_fish %>% mutate(
  ICS_FAOSTAT = case_when(
    str_detect(food_desc, "burger|Surimi") ~ "1520", 
    str_detect(food_desc, "Lutef") ~ "1518", 
    str_detect(food_desc, "Snails, canned") ~ "1562", 
    TRUE ~ ICS_FAOSTAT),
  ICS_FAOSTAT_future = case_when(
    str_detect(food_desc, "burger|Surimi") ~ "15200", 
    str_detect(food_desc, "Lutef") ~ "15180", 
    str_detect(food_desc, "Snails, canned") ~ "15620", 
    TRUE ~ ICS_FAOSTAT_future)) 

#Adding the ICS FAOSTAT description for the "current" and "Future" code
#And tidying the columns
dim(no21_fish)
no21_fish <- no21_fish  %>%
  left_join(., ics_code %>% select(1,ics_faostat_sua_english_description) %>%
              mutate_at("ics_faostat_sua_current_code", as.character), 
            by = c("ICS_FAOSTAT" = "ics_faostat_sua_current_code")) %>% 
  left_join(., ics_code %>% select(2,ics_faostat_sua_english_description) %>%
              mutate_at("ics_faostat_sua_future_code", as.character), 
            by = c("ICS_FAOSTAT_future" = "ics_faostat_sua_future_code")) %>%
  mutate(
    ics_faostat_sua_english_description = 
      ifelse(is.na(ics_faostat_sua_english_description.x),
             ics_faostat_sua_english_description.y, 
             ics_faostat_sua_english_description.x)) %>% 
  select(-c("ics_faostat_sua_english_description.x", 
            "ics_faostat_sua_english_description.y")) %>% 
  relocate(any_of(c("ISSCAAP","ICS_FAOSTAT", "ICS_FAOSTAT_future",
                    "ics_faostat_sua_english_description")), 
           .before = "fdc_id") %>% 
  rename(scientific_name = "Scientific_name")

#├├ Adding Quality Code for the matching ----

no21_fish <- no21_fish %>% 
  mutate(quality = 
           ifelse(ICS_FAOSTAT != "NA",
                  "A2",      #We are adding A2 to all, as the matches were good 
                  NA)) %>%   #to be identified by the description provided and most of them by scientific name
  relocate(any_of(c("quality")), 
           .after = "product_type") 

#Converting NA into `NA` and checking the no. of fish w/o ICS code
NO_FCT_Data %>% naniar::replace_with_na(replace = list(ICS_FAOSTAT = "NA")) %>% 
  filter(is.na(ICS_FAOSTAT))

# Save an NO21 to a R file - for QC.
saveRDS(NO_FCT_Data, file = "NO21/fish_NO21.rds")

#├ 3.6. Dealing with missing values ----

#├├ 3.6.3. Back-calculating variables  ----

# Generating the ASHg_bydiff variable to be used in calculations

proximates <- c("WATERg", "PROCNTg", "FAT_g",  "CHOAVLg", "FIBTGg", "ALCg")

NO_FCT_Data[, proximates] <- apply(NO_FCT_Data[, proximates], 2, as.numeric)


NO_FCT_Data$ASHg_bydiff <-  100-(NO_FCT_Data$WATERg + NO_FCT_Data$PROCNTg + 
                                   NO_FCT_Data$FAT_g + NO_FCT_Data$FIBTGg + 
                                   NO_FCT_Data$ALCg + NO_FCT_Data$CHOAVLg)


# 4. Data visualisation & Quality Checks ----

#Excluding fishery entries w/ extreme values from the final selection
# See documentation and NO21_QC.R)
#Removing some fish items from the included selection that did not pass the QC.
# "04.366", - Surimi, LobNobs --> SOP_std too small
# "04.373", - Caviar, polar" --> CHO too high
# "04.307", - Caviar, cod roe with mayonnaise
# "04.089", - Caviar, capelin roe --> CHO too high
# "04.323 ", - Fish burger, breaded, --> Not only the fish burger

excluded <- c("04.366", "04.373", "04.307", "04.089", "04.323")

NO_FCT_Data$ICS_FAOSTAT[NO_FCT_Data$fdc_id %in% excluded] <- NA
NO_FCT_Data$ics_faostat_sua_english_description[NO_FCT_Data$fdc_id %in% excluded] <- NA

#Final checks
head(NO_FCT_Data) # First 6 rows of the data
dim(NO_FCT_Data) # No. of rows and columns

subset(NO_FCT_Data, !is.na(ics_faostat_sua_english_description))

# 5. Exporting data and metadata  ----