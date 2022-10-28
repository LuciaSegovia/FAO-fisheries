
# 0) Loading packages and data ----

library(tidyverse)

# Restore the NO21 fish object
NO_FCT_Data <- readRDS(file = "NO21/fish_NO21.rds")

names(NO_FCT_Data)

#04.208, 04.313

subset(NO_FCT_Data, fdc_id == "04.313", 
       select=c(food_desc, WATERg, CHOAVLg))

# Sum of Proximate (SOP)----

# Calculating SOP and ASHg 

#Ash value (g/100 g EP)> (CA (mg) + FE (mg) + MG (mg)+ P (mg) + K (mg) + 
#NA (mg) + ZN (mg)+ CU (mg) + MN (mg) + CL (mg))/1000

#sum of proximates (=∑of water + protein + fat + available carbohydrates + dietary fibre + alcohol + ash)

#We calculate ASHg as the formula above and as difference from the total component

test_df <- NO_FCT_Data %>%
  mutate_at(c("CAmg", "FEmg", "MGmg", "Pmg",
              "Kmg", "NAmg", "ZNmg", "CUmg", "WATERg", 
              "PROCNTg", "FAT_g",  "CHOAVLg", "FIBTGg", "ALCg"), as.numeric) %>% 
  mutate(CLmg_cal = NAmg*2.5) %>%
  mutate(ASHg_std =  (CAmg+ FEmg+ MGmg+ Pmg+ Kmg+ NAmg+
                        ZNmg+ CUmg+ CLmg_cal)/1000) %>%
  mutate(SOP_std = (WATERg + PROCNTg + FAT_g + CHOAVLg + FIBTGg + ALCg +
                      ASHg_std)) %>% 
  mutate(CHOAVLDFg_std = 100-(WATERg + PROCNTg + FAT_g + FIBTGg + ALCg +
                                ASHg_std)) %>% 
  mutate(ASHg_bydiff = 100-(WATERg + PROCNTg + FAT_g + FIBTGg + ALCg +
                              CHOAVLg)) %>% 
  mutate(ASHg_diff = ASHg_bydiff - ASHg_std)

#Checking fish entries outside the SOP range
#Remove 04.366 Surimi, LobNobs bc SOP_std was too small.

test_df%>% 
  #  filter( SOP_std < 105 & SOP_std > 95) %>% 
  filter( SOP_std < 95 | SOP_std > 105) %>% 
  select(ICS_FAOSTAT, fdc_id, food_desc,  SOP_std, FAT_g, ASHg_std, 
         CHOAVLg, WATERg, PROCNTg)

# Carbohydrates (CHOAVLg)----

#Checking CHOAVLg

#Checking CHOAVLg distribution
hist(as.numeric(NO_FCT_Data$CHOAVLg))

#Checking values included in Fisheries Global NCT w/ CHOAVLg > 0
NO_FCT_Data %>% filter(!is.na(ics_faostat_sua_english_description), as.numeric(CHOAVLg) > 0)

hist(as.numeric(NO_FCT_Data$CHOAVLg[!is.na(NO_FCT_Data$ics_faostat_sua_english_description) & as.numeric(NO_FCT_Data$CHOAVLg) > 0]), 
     xlab = "Carbohydrates (g/100g)",
     main = "Fish included in NO21 with CHOALVg > 0")

#Checking fresh (raw) fish w/ CHOAVLg > 0
NO_FCT_Data$food_desc[!is.na(NO_FCT_Data$ics_faostat_sua_english_description) & NO_FCT_Data$CHOAVLg > 0 & NO_FCT_Data$product_type<3]


#Identifying ICS description with CHOAVLg > 0
subset(NO_FCT_Data, !is.na(ics_faostat_sua_english_description) & 
         as.numeric(NO_FCT_Data$CHOAVLg) > 0) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(
    n = length(ISSCAAP),
    mean_WATERg=mean(as.numeric(WATERg)),
    # mean_ASHg = mean(as.numeric(ASHg), na.rm = T),
    mean_CHOAVLDFg =mean(as.numeric(CHOAVLg), na.rm =T))

#Checking those with higher CHOAVLg
#Pelagic fish, cured
#Cephalopods, preparations nei
#Demersal fish, preparations nei
#Pelagic fish, preparations nei

subset(NO_FCT_Data, as.numeric(CHOAVLg) > 10 &
         ics_faostat_sua_english_description == "Pelagic fish, preparations nei",
       select = c(fdc_id, food_desc,CHOAVLg ))

#visuaL
hist(as.numeric(NO_FCT_Data$CHOAVLg[NO_FCT_Data$ics_faostat_sua_english_description == "Demersal fish, preparations nei"]),
     main = "Fish with high CHOAVLg",
     xlab = "Carbohydrates (g/100g)")

#Excluding:
#"04.373 - Caviar, polar" &
#"04.307- Caviar, cod roe with mayonnaise" SOP_std and CHOAVLg were both high and source of CHO was not reliable
#"04.089 Caviar, capelin roe" CHOAVLg was high and source of CHO was not reliable
#"04.323 Fish burger, breaded, fried, with bread, cheese, sauce, fast food restaurant" 


#Storing the isscaap of the fishery prod. w/ CHO >0 that were included to check
#the CHO, ASH and other NVs. 
isscaap <- unique(NO_FCT_Data$ISSCAAP[!is.na(NO_FCT_Data$ics_faostat_sua_english_description) & NO_FCT_Data$CHOAVLg > 0])
#We are storing a dataset with more information 

CHO_check <-  unique(subset(NO_FCT_Data, !is.na(ics_faostat_sua_english_description) & 
                              NO_FCT_Data$CHOAVLg > 0, 
                            select = c(ISSCAAP, product_type,ics_faostat_sua_english_description )))




test_df$CHO_diff <- test_df$CHOAVLg -test_df$CHOAVLDFg_std

hist(test_df$CHO_diff) 

x <- test_df %>%
  filter(!is.na(ics_faostat_sua_english_description), CHOAVLg > 0) %>%
  mutate(CHOAVLDFg_std = ifelse(CHOAVLDFg_std<0, 0, CHOAVLDFg_std)) %>% 
  select(food_desc, CHOAVLg, CHOAVLDFg_std, CHO_diff) %>% arrange(desc(CHO_diff))

# Calculating ASHg to be used for 

#Mean ASHg recalculated using CHO by difference formula
aggregate(test_df$ASHg_bydiff,
          list(test_df$ics_faostat_sua_english_description), FUN = mean)
#Mean ASHg difference between two methods 
aggregate(test_df$ASHg_diff,
          list(test_df$ics_faostat_sua_english_description), FUN = mean)

#Checking SUA fish cat. w/ high differences
subset(NO_FCT_Data,
       ics_faostat_sua_english_description == "Demersal fish, canned", 
       select = c(fdc_id, food_desc,CHOAVLg ))


