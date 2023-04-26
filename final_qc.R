
#Original 122 columns
fao_fish <- read.csv(here::here("data", "FISHERIES-GlobalNCT_ForSharing_Feb2022.csv"))
#fao_fish <- readxl::read_excel(here::here("data", "FAO_fish.xlsx"))
source(here::here("Summary_Table_Production.R"))

dim(fao_fish)
names(fao_fish)

names(fao_fish[, c(1:122)])
fao_fish[c(1:2), c(1:122)]
fao_fish[2, c(13:30)]


which(is.na(fao_fish[1, c(1:122)]))
which(fao_fish[1, c(1:122)] == "")

fao_fish[1, c(1:30)]

n <- which(fao_fish[1, c(1:122)] == "")

which(fao_fish[2, c(1:122)] == "Quality rating for food match")

fao_fish[1, n]
fao_fish[1, c(1:122)[!c(1:122) %in% n]]

names(fao_fish)[1:6] <- fao_fish[2, c(1:6)]
names(fao_fish)[(7:122)[!c(7:122) %in% n]] <- fao_fish[1, c(7:122)[!c(7:122) %in% n]]
names(fao_fish)


# Checking duplicates - in original file 

which(fao_fish[2, c(1:122)] == "fdc_id")

fao_fish[3:nrow(fao_fish), c(1, 33)] %>% 
  group_by(`ï..GLOBAL.TABLE.OF.NUTRIENT.VALUES`) %>% count(X.30) %>% 
  filter(n>1) %>% 
  arrange(desc(n))


# fao_fish <- readxl::read_excel(here::here("data", "FAO_fish.xlsx"))




fao_fish$`FIBTG(g) (standardized)`
unique(fao_fish$Source)

unique(subset(fao_fish, Source == "IN17", select =c(`FIBTG(g) (standardized)`)))

fao_fish$ICS_FAOSTAT <- ifelse(is.na(fao_fish$`ICS FAOSTAT SUA Current Code`), 
                               fao_fish$`ICS FAOSTAT SUA Future Code`,
                               fao_fish$`ICS FAOSTAT SUA Current Code` )

fao_fish$fdc_id <- ifelse(!is.na(fao_fish$fcd_id_old), 
                          fao_fish$fcd_id_old,
                          fao_fish$fdc_id )


# Getting only mean values
which(is.na(fao_fish$`ICS FAOSTAT SUA title`))
mean_loc <- which(is.na(fao_fish$`Food description`))

fao_cat <- fao_fish[mean_loc,]

## Getting summary rows (FISHERIES in R)

summary.df <- subset(recalculated_results_table, 
                     str_detect(ICS.FAOSTAT.SUA.Current.Code, "SUMMARY ROW")) 

subset(summary.df, !is.na(comment), 
       select = c(ICS.FAOSTAT.SUA.Current.Code, CHOAVLDFg_std, comment))

# MEAN CHOALVDF (standardise) ----

#Checking non-calculated values
subset(summary.df, is.na(CHOAVLDFg_std), 
       select = c(ICS.FAOSTAT.SUA.Current.Code,
                  `ICS FAOSTAT SUA title`, CHOAVLDFg_std, ALCg))

#Checking negative values
subset(summary.df, CHOAVLDFg_std <0, 
       select = c(ICS.FAOSTAT.SUA.Current.Code,
                  `ICS FAOSTAT SUA title`, CHOAVLDFg_std))

subset(fao_cat, `CHOAVLDF(g)_standardized` < 0, 
       select = c( `CHOAVLDF(g)_standardized`))

hist(fao_cat$`CHOAVLDF(g)_standardized`, 
     xlab = "Carbohydrates by difference", 
     main = "Fisheries Global NCT (Feb, 2022)")


hist(summary.df$CHOAVLDFg_std, 
     xlab = "Carbohydrates by difference", 
     main = "Fisheries Global NCT (Oct, 2022)")

mean_proxi$ICS_FAOSTAT <-  as.factor(mean_proxi$ICS_FAOSTAT)
fao_cat$ICS_FAOSTAT <-  as.factor(fao_cat$ICS_FAOSTAT)

left_join(mean_proxi, fao_cat %>% select(ICS_FAOSTAT,
                                         `CHOAVLDF(g)_standardized`,
                                         `ICS FAOSTAT SUA title`) ) %>% arrange(ICS_FAOSTAT)

left_join(mean_proxi, fao_cat %>% select(ICS_FAOSTAT,
                                         `CHOAVLDF(g)_standardized`,
                                         `ICS FAOSTAT SUA title`) ) %>% 
  mutate(`CHOAVLDF(g)_standardized` = ifelse(`CHOAVLDF(g)_standardized` <0, 
                                             0, `CHOAVLDF(g)_standardized`),
         CHOAVLDFg_std  = ifelse(CHOAVLDFg_std  <0, 
                                 0, CHOAVLDFg_std ),
         CHO_diff = CHOAVLDFg_std-`CHOAVLDF(g)_standardized`) %>% 
  arrange(desc(CHO_diff))


subset(fao_fish, ICS_FAOSTAT == "1533") %>% count(Source)

#MEAN ASH 
hist(fao_cat$`ASH(g)`, 
     xlab = "ASH(g)", 
     main = "Fisheries Global NCT (FAO)")

fao_cat$`ICS FAOSTAT SUA title`[fao_cat$`ASH(g)` > 3]


#MEAN FIBGT 

fao_cat$`FIBTG(g) (standardized)` <- as.numeric(fao_cat$`FIBTG(g) (standardized)`)

hist(as.numeric(fao_cat$`FIBTG(g) (standardized)`), 
     xlab = "Fibre (g/100g)", 
     main = "Fisheries Global NCT (FAO)")

fao_cat$`ICS FAOSTAT SUA title`[as.numeric(fao_cat$`FIBTG(g) (standardized)`) > 0]
subset(fao_cat, as.numeric(`FIBTG(g) (standardized)`) > 0, 
       select = c(`ICS FAOSTAT SUA title`,
                  round(`FIBTG(g) (standardized)`, 2)))

#MEAN ALC
hist(as.numeric(fao_cat$`ALC(g)`), 
     xlab = "Alcohol (g/100g)", 
     main = "Fisheries Global NCT (FAO)")

fao_cat$`ICS FAOSTAT SUA title`[as.numeric(fao_cat$`ALC(g)`) == 0]



# MEAN SOP (own calculations) ----

#Checking non-calculated values
subset(summary.df, is.na(SOP_std), 
       select = c(ICS.FAOSTAT.SUA.Current.Code,
                  `ICS FAOSTAT SUA title`, SOP_std))

#Checking values outside of range (95-105) & (98-102)
subset(summary.df, SOP_std <98 | SOP_std >102, 
       select = c(ICS.FAOSTAT.SUA.Current.Code,
                  `ICS FAOSTAT SUA title`, SOP_std))

hist(as.numeric(summary.df$SOP_std), main = "Sum of Proximate")
abline(v = 95, col = 2, lwd=3, lty =2)
abline(v = 105, col = 2, lwd=3, lty =2)

#Checking EP

hist(as.numeric(fao_cat$`Edible factor in FCT`), 
     xlab = "Alcohol (g/100g)", 
     main = "Fisheries Global NCT (FAO)")

fao_cat$`Edible factor in FCT`
fao_cat$`Edible coefficient to be used`

subset(fao_fish, ICS_FAOSTAT == "1545", select = c(fdc_id, Source,  `WATER(g)`)) %>% 
  left_join(subset(fao_fish_fct, ICS.FAOSTAT.SUA.Current.Code == "1545",
                   select = c(fdc_id, WATERg, source_fct))) %>% 
  mutate(water_diff =WATERg- `WATER(g)`) %>% filter(water_diff>0)


#MEAN THIA
#Differences - 1573 (bc we added values i.e., raw as per agreed w/ F.G)
#1541 - we couldn't find but no NO21 values, and seemed the same (n=154)

hist(as.numeric(fao_cat$`THIA(mg)_standardized`), 
     xlab = "Thiamine (mg/100g)", 
     main = "Fisheries Global NCT (FAO)")

match("THIA(mg)_standardized", names(fao_cat))


fao_cat$`ICS FAOSTAT SUA Current Code` <- as.character(fao_cat$`ICS FAOSTAT SUA Current Code`)

subset(results_table, str_detect(ICS.FAOSTAT.SUA.Current.Code,
                                 "SUMMARY ROW"),
       select = c(ICS.FAOSTAT.SUA.Current.Code,
                  ics_faostat_sua_english_description, THIAmg_std)) %>% 
  mutate(ICS.FAOSTAT.SUA.Current.Code = str_extract(ICS.FAOSTAT.SUA.Current.Code, 
                                                    "[:digit:]{4,6}")) %>% 
  left_join(., fao_cat[c(1:95), c(1,76)], by =
              c("ICS.FAOSTAT.SUA.Current.Code" = "ICS FAOSTAT SUA Current Code")) %>% 
  mutate(THIAmg_diff = as.numeric(THIAmg_std) - as.numeric(`THIA(mg)_standardized`)) %>% 
  mutate_at(c(3:5), as.numeric) %>%  mutate_if(is.numeric, round,
                                               digits = 2)  %>% 
  filter(THIAmg_diff >0) %>% arrange(desc(THIAmg_diff))


fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code[fao_fish_fct$fdc_id == "10366"]
fao_fish_fct$THIAmg[fao_fish_fct$fdc_id == "04.100"]
results_table$THIAmg_std[results_table$ICS.FAOSTAT.SUA.Current.Code == "1541"]
results_table$THIAHCLmg[results_table$ICS.FAOSTAT.SUA.Current.Code == "1590"]

#MEAN RETOL ----
#Need to keep checking, odd calculations and imputations...

#Histogram of retinol values from Fisheries version Feb., 2022.
hist(as.numeric(fao_cat$`RETOL(µg)`), 
     xlab = "Retinol (mcg/100g)", 
     main = "Fisheries Global NCT (FAO), Feb., 2022 \r\n (Only mean values of 95 categories)")

#Histogram of retinol values from Fisheries version Oct., 2022.
hist(as.numeric(recalculated_results_table$RETOLmcg), 
     xlab = "Retinol (mcg/100g)", 
     main = "Fisheries Global NCT (FAO), Oct., 2022")

#Max value of retinol in Feb., fisheries dataset
max(as.numeric(fao_fish$`RETOL(µg)`), na.rm = T)
#Checking high values
subset(recalculated_results_table, as.numeric(RETOLmcg) >33000)

#Checking that no mean values in our dataset are below 0
subset(recalculated_results_table, as.numeric(RETOLmcg) <0 & 
         !str_detect(ICS.FAOSTAT.SUA.Current.Code,
                     "SUMMARY ROW"))

#Identifying the retinol column in Feb., 2022 dataset
n <- match("RETOL(µg)", names(fao_cat))

#Aligning data class for joining datasets
fao_cat$`ICS FAOSTAT SUA Current Code` <- as.character(fao_cat$`ICS FAOSTAT SUA Current Code`)

#Identifying max. difference between retinol values by category
retol_max <- subset(recalculated_results_table, str_detect(ICS.FAOSTAT.SUA.Current.Code,
                                                           "SUMMARY ROW"),
                    select = c(ICS.FAOSTAT.SUA.Current.Code,
                               `ICS FAOSTAT SUA title`, RETOLmcg)) %>% 
  mutate(ICS.FAOSTAT.SUA.Current.Code = str_extract(ICS.FAOSTAT.SUA.Current.Code, 
                                                    "[:digit:]{4,6}")) %>% 
  left_join(., fao_cat[c(1:95), c(1,n)], by =
              c("ICS.FAOSTAT.SUA.Current.Code" = "ICS FAOSTAT SUA Current Code")) %>% 
  mutate(diff = as.numeric(RETOLmcg) - as.numeric(`RETOL(µg)`)) %>% 
  mutate_at(c(3:5), as.numeric) %>%  mutate_if(is.numeric, round,
                                               digits = 2)  %>% 
  filter(diff >0) %>% pull(diff) %>% max(.)

#Identifying max. negative difference between retinol values by category
retol_min <- subset(recalculated_results_table, str_detect(ICS.FAOSTAT.SUA.Current.Code,
                                                           "SUMMARY ROW"),
                    select = c(ICS.FAOSTAT.SUA.Current.Code,
                               `ICS FAOSTAT SUA title`, RETOLmcg)) %>% 
  mutate(ICS.FAOSTAT.SUA.Current.Code = str_extract(ICS.FAOSTAT.SUA.Current.Code, 
                                                    "[:digit:]{4,6}")) %>% 
  left_join(., fao_cat[c(1:95), c(1,n)], by =
              c("ICS.FAOSTAT.SUA.Current.Code" = "ICS FAOSTAT SUA Current Code")) %>% 
  mutate(diff = as.numeric(RETOLmcg) - as.numeric(`RETOL(µg)`)) %>% 
  mutate_at(c(3:5), as.numeric) %>%  mutate_if(is.numeric, round,
                                               digits = 2)  %>% 
  filter(diff <0) %>% pull(diff) %>% min(.)

#Checking categories w/ higher RETOL (negative) and lower RETOL (positive) values
#in the Feb/, 2022 vs Oct., 2022 dataset. 
subset(recalculated_results_table, str_detect(ICS.FAOSTAT.SUA.Current.Code,
                                              "SUMMARY ROW"),
       select = c(ICS.FAOSTAT.SUA.Current.Code,
                  `ICS FAOSTAT SUA title`, RETOLmcg)) %>% 
  mutate(ICS.FAOSTAT.SUA.Current.Code = str_extract(ICS.FAOSTAT.SUA.Current.Code, 
                                                    "[:digit:]{4,6}")) %>% 
  left_join(., fao_cat[c(1:95), c(1,n)], by =
              c("ICS.FAOSTAT.SUA.Current.Code" = "ICS FAOSTAT SUA Current Code")) %>% 
  mutate(diff = as.numeric(RETOLmcg) - as.numeric(`RETOL(µg)`)) %>% 
  mutate_at(c(3:5), as.numeric) %>%  mutate_if(is.numeric, round,
                                               digits = 2)  %>% 
  filter(diff <0) %>% arrange(desc(diff))

#Checking actual fisheries entries to identify differences in unique entries
subset(recalculated_results_table, ICS.FAOSTAT.SUA.Current.Code == "1510") %>% 
  count(source_fct)

#Evaluating influence of NO21 values into mean category values
subset(recalculated_results_table, ICS.FAOSTAT.SUA.Current.Code == "1519" &
         source_fct == "NO21") %>% 
  pull(RETOLmcg) %>% mean(.)

#Evaluting values (important for imputed values) differences between datasets
recalculated_results_table$RETOLmcg[recalculated_results_table$ICS.FAOSTAT.SUA.Current.Code == "1510"]
fao_fish_fct$RETOLmcg[fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code == "1510"]
fao_fish$`RETOL(µg)`[fao_fish$`ICS FAOSTAT SUA Current Code` == "1541"]

#Evaluting influence of the values (important for imputed values) to the mean
mean(recalculated_results_table$RETOLmcg[recalculated_results_table$ICS.FAOSTAT.SUA.Current.Code == "1541"])
mean(as.numeric(fao_fish_fct$RETOLmcg[fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code == "1541"]
))
mean(as.numeric(fao_fish$`RETOL(µg)`[fao_fish$`ICS FAOSTAT SUA Current Code` == "1541"][c(1:154)]), na.rm = T)



fao_fish_fct$VITA_RAEmcg[fao_fish_fct$fdc_id == "35014"]
results_table$THIAmg_std[results_table$ICS.FAOSTAT.SUA.Current.Code == "1541"]
results_table$fdc_id[results_table$ICS.FAOSTAT.SUA.Current.Code == "1519"]
results_table$source_fct[results_table$ICS.FAOSTAT.SUA.Current.Code == "1505"]
fao_fish_fct$food_desc[fao_fish_fct$fdc_id == "04.286"]
fao_fish_fct$RETOLmcg[str_detect(fao_fish_fct$food_desc, "Saithe")]
fao_fish_fct$fdc_id[str_detect(fao_fish_fct$food_desc, "Saithe")]


# MEAN CARTBEQ (standardise) ----

hist(as.numeric(fao_cat$`CARTBEQ(mcg) (standardized)`), 
     xlab = "Beta-Carotene eq. (mcg/100g)", 
     main = "Fisheries Global NCT (FAO), Feb., 2022 \r\n (Only mean values of 95 categories)")

#Checking that no mean values in our dataset are below 0
subset(recalculated_results_table, as.numeric(CARTBEQmcg_std) <0 & 
         !str_detect(ICS.FAOSTAT.SUA.Current.Code,
                     "SUMMARY ROW"))

#Identifying the beta-carotene eq. column in Feb., 2022 dataset
n <- match("CARTBEQ(mcg) (standardized)", names(fao_cat))


fao_cat$`ICS FAOSTAT SUA Current Code` <- as.character(fao_cat$`ICS FAOSTAT SUA Current Code`)

subset(recalculated_results_table, str_detect(ICS.FAOSTAT.SUA.Current.Code,
                                              "SUMMARY ROW"),
       select = c(ICS.FAOSTAT.SUA.Current.Code,
                  `ICS FAOSTAT SUA title`, CARTBEQmcg_std, CARTBEQmcg,  
                  RETOLmcg, VITA_RAEmcg)) %>% 
  mutate(ICS.FAOSTAT.SUA.Current.Code = str_extract(ICS.FAOSTAT.SUA.Current.Code, 
                                                    "[:digit:]{4,6}")) %>% 
  mutate(CARTBEQ_fromVITA_RAE = ifelse(!is.na(VITA_RAEmcg) & !is.na(RETOLmcg), (VITA_RAEmcg - RETOLmcg)*12, NA)) %>% 
  left_join(., fao_cat[c(1:95), c(1,n)], by =
              c("ICS.FAOSTAT.SUA.Current.Code" = "ICS FAOSTAT SUA Current Code")) %>% 
  mutate(CARTBEQ_diff = as.numeric(CARTBEQmcg_std) - as.numeric(`CARTBEQ(mcg) (standardized)`)) %>% 
  mutate_at(c(3:5), as.numeric) %>%  mutate_if(is.numeric, round,
                                               digits = 2)  %>% 
  filter(CARTBEQ_diff <0) %>% arrange(desc(CARTBEQ_diff))

#Checking actual fisheries entries to identify differences in unique entries
subset(recalculated_results_table, ICS.FAOSTAT.SUA.Current.Code == "1549") %>% 
  count(source_fct)


#fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code[fao_fish_fct$fdc_id == "1549"]
fao_fish_fct$CARTBEQmcg[fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code == "15360"]
recalculated_results_table$CARTBEQmcg_std[recalculated_results_table$ICS.FAOSTAT.SUA.Current.Code == "15360"]
fao_fish$`CARTBEQ(mcg) (standardized)`[fao_fish$`ICS FAOSTAT SUA Current Code` == "15360"]

# MEAN VITA_RAE (standardise) ----

subset(recalculated_results_table, fdc_id == "09_0007") %>% distinct()

subset(recalculated_results_table, fdc_id == "09_0007", 
       select = c(RETOLmcg, CARTBEQmcg, CARTBEQmcg_std))

#Identifying the beta-carotene eq. column in Feb., 2022 dataset
n <- match("VITA_RAE(µg) (standardized)", names(fao_cat))
fao_cat$`ICS FAOSTAT SUA Current Code` <- as.character(fao_cat$`ICS FAOSTAT SUA Current Code`)
#Checking categories w/ higher RETOL (negative) and lower RETOL (positive) values
#in the Feb/, 2022 vs Oct., 2022 dataset. 
subset(recalculated_results_table, str_detect(ICS.FAOSTAT.SUA.Current.Code,
                                              "SUMMARY ROW"),
       select = c(ICS.FAOSTAT.SUA.Current.Code,
                  `ICS FAOSTAT SUA title`, VITA_RAEmcg_std)) %>% 
  mutate(ICS.FAOSTAT.SUA.Current.Code = str_extract(ICS.FAOSTAT.SUA.Current.Code, 
                                                    "[:digit:]{4,6}")) %>% 
  left_join(., fao_cat[c(1:95), c(1,n)], by =
              c("ICS.FAOSTAT.SUA.Current.Code" = "ICS FAOSTAT SUA Current Code")) %>% 
  mutate(diff = as.numeric(VITA_RAEmcg_std) - as.numeric(`VITA_RAE(µg) (standardized)`)) %>% 
  mutate_at(c(3:5), as.numeric) %>%  mutate_if(is.numeric, round,
                                               digits = 2)  %>% 
  filter(diff <0) %>% arrange(desc(diff))


#VITA 
subset(results_table, food_desc == "Algae, green laver, dried", 
       select = c(RETOLmcg,CARTBEQmcg,  CARTBEQmcg_std, VITA_RAEmcg_std))