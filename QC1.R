

#QC 
library(gt)
#If data is not loaded 
source("merging_all.R") #original fct
#source("missing.R") #added missing

## Checking Ash by difference  ----

# Proximate variables

proxi <- c( "WATERg", "PROCNTg",  "FATg",  "CHOAVLg", "FIBTGg", "ALCg", "ASHg" )

# Converting them into numeric
fct_cover[, proxi] <- apply(fct_cover[, proxi], 2, as.numeric)

# Creating a dataset for comparisson (only when Ash & CHO were analysed)
## Realised that there were some cases that proximate (realised some analysed values were 
# not accurated) 
# Removed those with SOP below acceptable range.

datadf <- fct_cover %>% select(fdc_id, food_desc, food_group, source_fct, proxi) %>% 
  filter(source_fct %in% c("AU19",  "NZ18", "IN17")) %>% 
  drop_na(c( "WATERg", "PROCNTg",  "FATg",  "CHOAVLg", "FIBTGg", "ALCg")) %>% 
  mutate(ASHDFg = (100-(WATERg+ PROCNTg+ CHOAVLg+ FATg+ FIBTGg+ALCg)),
         CHOAVLDFg = (100-(WATERg+ PROCNTg+ ASHg+ FATg+ FIBTGg+ALCg)),
         SOP = (WATERg+ PROCNTg+ CHOAVLg+ ASHg+ FATg+ FIBTGg+ALCg),
         ASHDFg = ifelse(ASHDFg<0, 0, ASHDFg),
         CHOAVLDFg = ifelse(CHOAVLDFg<0, 0, CHOAVLDFg)) %>% 
        filter(SOP<105 & SOP> 95) # Removing items above below acceptable range. 

## comparing accuracy of Ash by difference vs CHO by difference ----

par(mfrow = c(1, 2))
hist(datadf$ASHDFg)
hist(datadf$ASHg)

par(mfrow = c(2, 2))
plot(datadf$ASHg, datadf$ASHDFg)
plot(datadf$CHOAVLg, datadf$CHOAVLDFg)
hist(datadf$ASHDFg)
hist(datadf$CHOAVLDFg)


plot(datadf$ASHg, datadf$ASHDFg)
qqplot(datadf$ASHg, datadf$ASHDFg)
cor(datadf$ASHg, datadf$ASHDFg)

plot(datadf$CHOAVLg, datadf$CHOAVLDFg)
qqplot(datadf$CHOAVLg, datadf$CHOAVLDFg)
cor(datadf$CHOAVLg, datadf$CHOAVLDFg)

subset(datadf, ASHDFg>40 & ASHg<40) %>%  View()
subset(datadf, SOP<95 |SOP>105) %>%  View()



#1) Checking the fisheries dataset

dim(fao_fish_fct)
names(fao_fish_fct)

fao_fish_fct %>% 
  group_by(source_fct) %>% count()

# Checking n of fish per category

fao_fish_fct %>% count(fao_fish_fct) %>% 
  arrange(desc(n))

# Checking n of fish per category
#mean which is equal to nrow(fao_fish_fct)/95
fao_fish_fct %>% count(ics_faostat_sua_english_description) %>% pull(n) %>% mean()

  fao_fish_fct %>% count(ics_faostat_sua_english_description) %>%   
  mutate(perc = n/nrow(fao_fish_fct)) %>% 
  #ggplot(aes(x=reorder(ics_faostat_sua_english_description, perc), y = perc*100)) +
  ggplot(aes(x=reorder(ics_faostat_sua_english_description, n), y = n)) +
  geom_bar(stat = "identity") +
  theme_light() +
  coord_flip() +
  geom_hline(aes(yintercept = nrow(fao_fish_fct)/95), linetype = "dashed") + #mean
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  labs( x= "", y= "") + 
  guides(x = guide_axis(n.dodge = 2))


#Checking NV references

fao_fish_fct %>% filter(is.na(nutrient_data_source))%>% count(source_fct)

# 0) Water ----

#Checking water is complete

fao_fish_fct %>% filter(is.na(WATERg)) %>% count(source_fct)

#Overall
hist(as.numeric(fao_fish_fct$WATERg))

#Checking dried products
subset(fao_fish_fct, 
       str_detect(food_desc, " dry| dried")&
       !str_detect(food_desc, "stewed|cooked")&
         WATERg>30, select = c(WATERg, food_desc, source_fct)) %>% 
  distinct()

#Checking dried products
subset(fao_fish_fct, 
        fish_prep == "cured" &
         WATERg>60, select = c(WATERg, food_desc, source_fct)) %>% 
  distinct() %>% arrange(desc(WATERg))



#Checking NV for inclusion ####

# 1) Niacin ----

#Checking for missing values
fao_fish_fct %>% filter(is.na(NIAmg)) %>% count(source_fct)
fao_fish_fct %>% filter(is.na(TRPmg)) %>% count(source_fct)
fao_fish_fct %>% filter(is.na(NIAEQmg)) %>% count(source_fct)

fao_fish_fct %>% filter(is.na(NIAmg_std)) %>% count(source_fct)

#Checking variability in the values

#Overall
hist(as.numeric(fao_fish_fct$NIAmg_std))
quantile(as.numeric(fao_fish_fct$NIAmg_std), na.rm = T)

#By FCTs
fao_fish_fct %>% ggplot(aes(as.numeric(NIAmg_std), source_fct)) +
  geom_boxplot()

#Checking high end values
fao_fish_fct %>% filter(as.numeric(NIAmg_std) >40) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, NIAmg_std)

#By fish type
fao_fish_fct %>% ggplot(aes(as.numeric(NIAmg_std), fish_type)) +
  geom_boxplot()

#By fish preparation
fao_fish_fct %>% ggplot(aes(as.numeric(NIAmg_std), fish_prep)) +
  geom_boxplot()

#├├ Extreme values ----

#Checking values by ics 

x1 <- fao_fish_fct %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean_nia = mean(as.numeric(NIAmg_std), na.rm = T),
            sd_nia = sd(as.numeric(NIAmg_std), na.rm = T)) %>% 
  arrange(desc(mean_nia))

#Checking values with the highest mean  
fao_fish_fct %>% 
  filter(ics_faostat_sua_english_description == "Other pelagic fish, cured") %>% 
  select(source_fct, fdc_id, food_desc, WATERg, NIAmg_std)

#Checking skipjack values for different preparations
fao_fish_fct %>% 
  filter(str_detect(food_desc, "skipjack ")) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, NIAmg_std)


#Checking high end values
fao_fish_fct %>% filter(as.numeric(NIAmg_std) >20) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, NIAmg)

#Calculating mean concentration by ics w/o "extreme values"
x2 <- fao_fish_fct %>% 
  filter(as.numeric(NIAmg) < 40) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean_nia = mean(as.numeric(NIAmg_std), na.rm = T),
            sd_nia = sd(as.numeric(NIAmg_std), na.rm = T)) %>% 
  arrange(desc(mean_nia))

#Checking data w/ and w/o outliers
diff <- as.data.frame(all.equal(x1, x2))

# 2) Copper ----

#Checking for availability
fao_fish_fct %>% filter(is.na(CUmg)) %>% count(source_fct)

#Checking for brackets
fao_fish_fct %>% filter(!is.na(CUmg)) %>% 
  filter(str_detect(CUmg, "\\(|\\[")) 

#Checking for trace or similar
fao_fish_fct %>% filter(!is.na(CUmg)) %>% 
  filter(str_detect(CUmg, "^[:alpha:]")) 

#Checking variability in the values

#Overall
hist(as.numeric(fao_fish_fct$CUmg))

median(as.numeric(fao_fish_fct$CUmg), na.rm = T)

quantile(as.numeric(fao_fish_fct$CUmg), na.rm = T)

fao_fish_fct %>% filter(as.numeric(CUmg)>0.12) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, CUmg ) %>% 
  arrange(desc(as.numeric(CUmg)))

### octopus outlier ----
subset(fao_fish_fct, str_detect(food_desc, "octopus|Octopus") &
         !str_detect(food_desc, "boiled|grilled|cooked"), 
       select = c(source_fct, fdc_id, food_desc, WATERg, CUmg )) %>% 
  distinct()

#By FCTs
fao_fish_fct %>% ggplot(aes(as.numeric(CUmg), source_fct)) +
  geom_boxplot()

#By fish type
fao_fish_fct %>% ggplot(aes(as.numeric(CUmg), fish_type)) +
  geom_boxplot()

#By preparation
fao_fish_fct %>% ggplot(aes(as.numeric(CUmg), fish_prep)) +
  geom_boxplot()

#├├ Extreme values ----

#Calculating mean concentration by ics 

x1 <- fao_fish_fct %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean_cu = mean(as.numeric(CUmg), na.rm = T),
            sd_cu = sd(as.numeric(CUmg), na.rm = T)) %>% 
  arrange(desc(mean_cu))

#Checking values with the highest mean 
#Cephalopods, cured
#Crustaceans, cured
fao_fish_fct %>% 
  filter(ics_faostat_sua_english_description == "Cephalopods, cured") %>% 
  select(source_fct, fdc_id, food_desc, WATERg, CUmg)

#Checking firefly squid values for different preparations
fao_fish_fct %>% 
  filter(str_detect(food_desc, "firefly squid")) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, CUmg)

#Checking values for similar items 
fao_fish_fct %>% filter(str_detect(food_desc, "Oyster|oyster")) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, CUmg ) %>% 
  arrange(desc(as.numeric(CUmg)))


#Checking high end values

outlier_value <- 2.5

fao_fish_fct %>% filter(as.numeric(CUmg)> outlier_value) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, CUmg ) %>% 
  arrange(desc(as.numeric(CUmg)))


#Calculating mean concentration by ics w/o "extreme values"
x2 <- fao_fish_fct %>% 
  filter(as.numeric(CUmg)<5) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean_cu = mean(as.numeric(CUmg), na.rm = T),
            sd_cu = sd(as.numeric(CUmg), na.rm = T)) %>% 
  arrange(desc(mean_cu))


#Checking data w/ and w/o outliers
diff <- as.data.frame(all.equal(x1, x2))

# 3) Vitamin B6  ----

fao_fish_fct %>% filter(as.numeric(fao_fish_fct$VITB6Amg) > 1) %>% 
  select(food_desc, WATERg, VITB6Amg ) %>%  distinct() %>% knitr::kable()

which(fao_fish_fct$VITB6Amg > 1)

which(!is.na(fao_fish_fct$VITB6_mg))
which(is.na(fao_fish_fct$VITB6Amg) & is.na(fao_fish_fct$VITB6Cmg))


fao_fish_fct %>% filter(!is.na(VITB6_mg_standardised)) %>% 
  count(source_fct)

#[VITB6_mg_standardised] bc is calculated we do not need
#to check for bracket/trace as it was done before converting

#Checking variability in the values (total)

hist(as.numeric(fao_fish_fct$VITB6_mg_standardised))

median(as.numeric(fao_fish_fct$VITB6_mg_standardised), na.rm = T)
quantile(as.numeric(fao_fish_fct$VITB6_mg_standardised), na.rm = T)


#Checking variability in the values
fao_fish_fct %>% ggplot(aes(as.numeric(VITB6_mg_standardised), source_fct)) +
  geom_boxplot()

fao_fish_fct %>% ggplot(aes(as.numeric(VITB6_mg_standardised), fish_type)) +
  geom_boxplot()

fao_fish_fct %>% ggplot(aes(as.numeric(VITB6_mg_standardised), fish_prep)) +
  geom_boxplot()

#├├ Extreme values ----

#Calculating mean concentration by ics 

x1 <- fao_fish_fct %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean = mean(as.numeric(VITB6_mg_standardised), na.rm = T),
            sd = sd(as.numeric(VITB6_mg_standardised), na.rm = T)) %>% 
  arrange(desc(mean))

#Checking values with the highest mean 
#Other pelagic fish, canned

fao_fish_fct %>% 
  filter(ics_faostat_sua_english_description == "Other pelagic fish, canned") %>% 
  select(source_fct, fdc_id, food_desc, WATERg, VITB6_mg_standardised)


#Checking high end values

outlier_value <- 0.75

fao_fish_fct %>% filter(as.numeric(VITB6_mg_standardised)> outlier_value) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, VITB6_mg_standardised ) %>% 
  arrange(desc(as.numeric(VITB6_mg_standardised)))


#Calculating mean concentration by ics w/o "extreme values"
x2 <- fao_fish_fct %>% 
  filter(as.numeric(VITB6_mg_standardised)< outlier_value) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean = mean(as.numeric(VITB6_mg_standardised), na.rm = T),
            sd = sd(as.numeric(VITB6_mg_standardised), na.rm = T)) %>% 
  arrange(desc(mean))


#Checking data w/ and w/o outliers
diff <- as.data.frame(all.equal(x1, x2))



# 4) Vitamin B12 ----

#Checking for availability
fao_fish_fct %>% filter(!is.na(VITB12mcg)) %>% count(source_fct)

#Checking for brackets
fao_fish_fct %>% filter(!is.na(VITB12mcg)) %>% 
  filter(str_detect(VITB12mcg, "\\(|\\[")) 

#Checking for trace or similar
fao_fish_fct %>% filter(!is.na(VITB12mcg)) %>% 
  filter(str_detect(VITB12mcg, "^[:alpha:]")) 

#Checking variability in the values (total)
hist(as.numeric(fao_fish_fct$VITB12mcg))
median(as.numeric(fao_fish_fct$VITB12mcg), na.rm = T)
quantile(as.numeric(fao_fish_fct$VITB12mcg), na.rm = T)

fao_fish_fct %>% filter(as.numeric(VITB12mcg)>4.6) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, VITB12mcg ) 

#Checking variability in the values
fao_fish_fct %>% ggplot(aes(as.numeric(VITB12mcg), source_fct)) +
  geom_boxplot()

fao_fish_fct %>% ggplot(aes(as.numeric(VITB12mcg), fish_type)) +
  geom_boxplot()

fao_fish_fct %>% ggplot(aes(as.numeric(VITB12mcg), fish_prep)) +
  geom_boxplot()

#├├ Extreme values ----

#Calculating mean concentration by ics 

x1 <- fao_fish_fct %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean = mean(as.numeric(VITB12mcg), na.rm = T),
            sd = sd(as.numeric(VITB12mcg), na.rm = T)) %>% 
  arrange(desc(mean))

#Checking values with the highest mean 
#Molluscs, excluding cephalopods, frozen
#Molluscs, excluding cephalopods, fresh
#Molluscs, excluding cephalopods, canned
#Cephalopods, preparations nei
#Aquatic plants, preparations nei

test <- "Molluscs, excluding cephalopods, frozen"

fao_fish_fct %>% 
  filter(ics_faostat_sua_english_description %in% test) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, VITB12mcg) %>% 
  arrange(desc(as.numeric(VITB12mcg)))

hist(as.numeric(fao_fish_fct$VITB12mcg[fao_fish_fct$ics_faostat_sua_english_description %in% test]))


#Checking high end values

outlier_value <- 20

fao_fish_fct %>% filter(as.numeric(VITB12mcg)> outlier_value) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, VITB12mcg , 
         ics_faostat_sua_english_description) %>% 
  arrange(desc(as.numeric(VITB12mcg)))


#Calculating mean concentration by ics w/o "extreme values"

x2 <- fao_fish_fct %>% 
  filter(as.numeric(VITB12mcg) < outlier_value) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean = mean(as.numeric(VITB12mcg), na.rm = T),
            sd = sd(as.numeric(VITB12mcg), na.rm = T)) %>% 
  arrange(desc(mean))


#Checking data w/ and w/o outliers
diff <- as.data.frame(all.equal(x1, x2))


#5) DHA (22:6 n-3) ----

#Checking for availability
fao_fish_fct %>% filter(is.na(F22D6N3g)) %>% count(source_fct)
#fct_cover %>% filter(!is.na(F22D6N3mg)) %>% count(source_fct)

#Checking for brackets
fao_fish_fct %>% filter(!is.na(F22D6N3g)) %>% 
  filter(str_detect(F22D6N3g, "\\(|\\[")) 

#Checking for trace or similar
fao_fish_fct %>% filter(!is.na(F22D6N3g)) %>% 
  filter(str_detect(F22D6N3g, "^[:alpha:]")) 

#Checking variability in the values (total)
hist(as.numeric(fao_fish_fct$F22D6N3g))
median(as.numeric(fao_fish_fct$F22D6N3g), na.rm = T)
quantile(as.numeric(fao_fish_fct$F22D6N3g), na.rm = T)

#Checking values higher than Q4
fao_fish_fct %>% filter(as.numeric(F22D6N3g)>0.695, 
                        as.numeric(F22D6N3g)<4) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, FAT_g_standardised,
         F22D6N3g) %>% 
  arrange(as.numeric(F22D6N3g))

#Checking variability in the values
#By FCT
fao_fish_fct %>% ggplot(aes(as.numeric(F22D6N3g), source_fct)) +
  geom_boxplot()

#By fish type
fao_fish_fct %>% ggplot(aes(as.numeric(F22D6N3g), fish_type)) +
  geom_boxplot()

#By fish preparation
fao_fish_fct %>% ggplot(aes(as.numeric(F22D6N3g), fish_prep)) +
  geom_boxplot()

#├├ Extreme values ----
fao_fish_fct %>% filter(as.numeric(F22D6N3g)>10) %>% 
  select(food_desc, WATERg, FATg, F22D6N3g ) %>%  distinct() %>% knitr::kable()

fao_fish_fct %>% filter(as.numeric(F22D6N3g)>5) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, F22D6N3g )


#Calculating mean concentration by ics 

x1 <- fao_fish_fct %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean_fat = mean(as.numeric(FAT_g_standardised), na.rm = T),
            mean = mean(as.numeric(F22D6N3g), na.rm = T),
            sd = sd(as.numeric(F22D6N3g), na.rm = T)) %>% 
  arrange(desc(ics_faostat_sua_english_description))


#Checking values with no NA

fao_fish_fct %>% filter(!is.na(F22D6N3g)) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, 
         FAT_g_standardised, F22D6N3g , 
         ics_faostat_sua_english_description) %>% 
  arrange(desc(as.numeric(F22D6N3g)))


#Calculating mean concentration by ics w/o "extreme values"

x2 <- fao_fish_fct %>% 
  filter(!is.na(F22D6N3g)) %>%
  group_by(ics_faostat_sua_english_description)  %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean_fat = mean(as.numeric(FAT_g_standardised), na.rm = T),
            mean = mean(as.numeric(F22D6N3g), na.rm = T),
            sd = sd(as.numeric(F22D6N3g), na.rm = T)) %>% 
  arrange(desc(ics_faostat_sua_english_description))


#Checking data w/ and w/o outliers
diff <- as.data.frame(all.equal(x1, x2))

#Generating a table with difference between 
#fat in the fct (x1) and
#fat only for items with DHA values (x2)

fat_check <- x1 
fat_check$fat_diff <- (x1$mean_fat-x2$mean_fat)/x1$mean_fat*100
fat_check$mean_fat_FA <- x2$mean_fat

#Checking SUA fish with difference higher than:
#5, 10
subset(fat_check, fat_diff>10 | fat_diff< -10,
       select = c(ics_faostat_sua_english_description,
                  fat_diff))

#Checking values with the highest mean 
#Aquatic mammals, meat
#Aquatic animals nei, fresh
#Aquatic plants, dried

test <- "Aquatic mammals, meat"

fao_fish_fct %>% 
  filter(ics_faostat_sua_english_description %in% test) %>% 
  select(source_fct, fdc_id, food_desc,
         WATERg, FAT_g_standardised, F22D6N3g) %>% 
  arrange(desc(as.numeric(F22D6N3g)))

hist(as.numeric(fao_fish_fct$F22D6N3g[fao_fish_fct$ics_faostat_sua_english_description %in% test]))
hist(as.numeric(fao_fish_fct$FAT_g_standardised[fao_fish_fct$ics_faostat_sua_english_description %in% test]))

fao_fish_fct %>% filter(ics_faostat_sua_english_description %in% test) %>% 
  ggplot(aes(FAT_g_standardised)) + geom_dotplot() + theme_classic()

#6) EPA (20:5 n-3) ----

#Checking for availability
fao_fish_fct %>% filter(!is.na(F20D5N3g)) %>% count(source_fct)

#Checking for brackets
fao_fish_fct %>% filter(!is.na(F20D5N3g)) %>% 
  filter(str_detect(F20D5N3g, "\\(|\\[")) 

#Checking for trace or similar
fao_fish_fct %>% filter(!is.na(F20D5N3g)) %>% 
  filter(str_detect(F20D5N3g, "^[:alpha:]")) 

#Checking variability in the values (total)
hist(as.numeric(fao_fish_fct$F20D5N3g))
median(as.numeric(fao_fish_fct$F20D5N3g), na.rm = T)
quantile(as.numeric(fao_fish_fct$F20D5N3g), na.rm = T)

#Checking values higher than Q4
fao_fish_fct %>% filter(as.numeric(F22D6N3g)>0.363) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, FAT_g_standardised,
         F20D5N3g) %>% 
  arrange(desc(as.numeric(F20D5N3g)))


#Checking variability in the values
fao_fish_fct %>% ggplot(aes(as.numeric(F20D5N3g), source_fct)) +
  geom_boxplot()

fao_fish_fct %>% ggplot(aes(as.numeric(F20D5N3g), fish_type)) +
  geom_boxplot()

fao_fish_fct %>% ggplot(aes(as.numeric(F20D5N3g), fish_prep)) +
  geom_boxplot()

#├├ Extreme values ----

fao_fish_fct %>% filter(as.numeric(F20D5N3g)>5) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, F20D5N3g )

fao_fish_fct %>% filter(as.numeric(F20D5N3g)>2.0, fish_prep == "fresh") %>% 
  select(source_fct, fdc_id, food_desc, WATERg, F20D5N3g )


#Calculating mean concentration by ics 

x1 <- fao_fish_fct %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean_fat = mean(as.numeric(FAT_g_standardised), na.rm = T),
            mean = mean(as.numeric(F20D5N3g), na.rm = T),
            sd = sd(as.numeric(F20D5N3g), na.rm = T)) %>% 
  arrange(desc(ics_faostat_sua_english_description))


#Checking values with no NA
fao_fish_fct %>% filter(!is.na(F20D5N3g)) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, 
         FAT_g_standardised, F20D5N3g , 
         ics_faostat_sua_english_description) %>% 
  arrange(desc(as.numeric(F20D5N3g)))


#Calculating mean concentration by ics w/o "extreme values"

x2 <- fao_fish_fct %>% 
  filter(!is.na(F20D5N3g)) %>%
  group_by(ics_faostat_sua_english_description)  %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean_fat = mean(as.numeric(FAT_g_standardised), na.rm = T),
            mean = mean(as.numeric(F20D5N3g), na.rm = T),
            sd = sd(as.numeric(F20D5N3g), na.rm = T)) %>% 
  arrange(desc(ics_faostat_sua_english_description))


#Checking data w/ and w/o outliers
diff <- as.data.frame(all.equal(x1, x2))


#7 ) Selenium  ----

#Checking for availability
fao_fish_fct %>% filter(!is.na(SEmcg)) %>% count(source_fct)


#Checking for brackets
fao_fish_fct %>% filter(!is.na(SEmcg)) %>% 
  filter(str_detect(SEmcg, "\\(|\\[")) 

#Checking for trace or similar
fao_fish_fct %>% filter(!is.na(SEmcg)) %>% 
  filter(str_detect(SEmcg, "^[:alpha:]")) 

#Checking variability in the values (total)
hist(as.numeric(fao_fish_fct$SEmcg))
median(as.numeric(fao_fish_fct$SEmcg), na.rm = T)
quantile(as.numeric(fao_fish_fct$SEmcg), na.rm = T)

#Checking values on the high end of the histogram

fao_fish_fct %>% filter(as.numeric(SEmcg)>100) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, SEmcg ) %>% 
  arrange(desc(as.numeric(SEmcg)))

fao_fish_fct %>% filter(str_detect(food_desc, "Tuna|tuna")) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, SEmcg ) %>% 
  arrange(desc(as.numeric(SEmcg)))

#Checking variability in the values
#By FCT - Note that if WA19 is showing 1 values is due to
#Added values (see missing.R)
fao_fish_fct %>% ggplot(aes(as.numeric(SEmcg), source_fct)) +
  geom_boxplot()

#By fish type
fao_fish_fct %>% ggplot(aes(as.numeric(SEmcg), fish_type)) +
  geom_boxplot()

#Checking fish type with "outliers"
#Pelagic fish
#Crustaceans
fao_fish_fct %>% filter(fish_type == "Crustaceans") %>% 
  select(source_fct, fdc_id, food_desc, WATERg, SEmcg ) %>% 
  arrange(desc(as.numeric(SEmcg)))

#By preparation
fao_fish_fct %>% ggplot(aes(as.numeric(SEmcg), fish_prep)) +
  geom_boxplot()

#├├ Extreme values ----

#Calculating mean concentration by ics 

x1 <- fao_fish_fct %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean = mean(as.numeric(SEmcg), na.rm = T),
            sd = sd(as.numeric(SEmcg), na.rm = T)) %>% 
  arrange(desc(mean))

#Checking values with the highest mean
#Other pelagic fish, cured
#Aquatic animals nei, cured
#Aquatic mammals, preparations nei
#Aquatic mammals, meat
#Cephalopods, preparations nei

test <- "Other pelagic fish, cured"

fao_fish_fct %>% 
  filter(ics_faostat_sua_english_description %in% test) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, SEmcg)

fao_fish_fct %>% filter(str_detect(food_desc, "mackerel"), 
                        source_fct == "JA15"
) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, SEmcg ) %>% 
  arrange(desc(as.numeric(SEmcg)))

#Checking high end values

outlier_value <- 100

fao_fish_fct %>% filter(as.numeric(SEmcg)> outlier_value) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, SEmcg ) %>% 
  arrange(desc(as.numeric(SEmcg)))

#Calculating mean concentration by ics w/o "extreme values"
x2 <- fao_fish_fct %>% 
  filter(as.numeric(SEmcg)< outlier_value) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise(mean_water = mean(as.numeric(WATERg), na.rm = T),
            mean = mean(as.numeric(SEmcg), na.rm = T),
            sd = sd(as.numeric(SEmcg), na.rm = T)) %>% 
  arrange(desc(mean))


#Checking data w/ and w/o outliers
diff <- as.data.frame(all.equal(x1, x2))

#Categories that would have no Se value if we remove the outliers.
#Five of them has no Se values and needed to be completed.
anti_join(x1, x2, by = "ics_faostat_sua_english_description")

## Removing low quality items ----

#Caviar, cod roe with mayonnaise (04.307)
#Caviar, polar (04.373)

fao_fish_fct %>% 
  filter(ICS.FAOSTAT.SUA.Current.Code == "1520",
         FAT_g_standardised > 30) 


low_q <- c("35013", "35055", "35079", "15142", #US19
           "04.318", "04.307", "04.373") #NO21

fao_fish_fct <- fao_fish_fct %>% 
  filter(!fdc_id %in% low_q)


## Adding Quality Scores to the data ------- 

qc <- read.csv(here::here("data", "quality_check.csv")) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(fdc_id, "[:alnum:]")) %>% 
  rename(source_fct = "source_fct_for_n_vs") #renaming variable the FCT source (e.g. BA13, WA19)

#fixing discrepancy between fcd_id in our dataframe (df) and Global FCT df for KE18 and US19
#This is needed for merging and filtering the fish and adding the ICS FAOSTAT code

qc %>% filter(str_detect(fdc_id, "^0")) 
qc <- qc %>% mutate(fdc_id = ifelse(source_fct == "KE18",
                                    str_replace(fdc_id, "^0", ""), fdc_id))  %>%  #removing the 0 of the fdc_id
  mutate(fdc_id = ifelse(source_fct == "US19",
                         ndb_number , fdc_id)) #using NDB_number as the fdc_id

#checking the US19 data from the FAO Global Fisheries data
qc %>% filter(source_fct == "US19")


#Added quality rating from previous Fishery dataset
# add NO21 and Cephalopods, canned

fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code <- as.integer(fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code)

fao_fish_fct <- fao_fish_fct %>% left_join(.,qc %>% 
                                             select(fdc_id, ics_faostat_sua_current_code, 
                                                    quality_rating_for_food_match),
                                           by = c("fdc_id" , 
                                                  "ICS.FAOSTAT.SUA.Current.Code" = "ics_faostat_sua_current_code")) %>% 
  mutate(
    quality_rating_for_food_match = 
      case_when(
        ics_faostat_sua_english_description == "Cephalopods, canned" ~ "C2", 
        is.na(quality_rating_for_food_match) ~ quality,
        TRUE ~ quality_rating_for_food_match)) 

fao_fish_fct %>% 
  filter(source_fct == "NO21") %>% 
  distinct(source_fct, ics_faostat_sua_english_description,
           quality_rating_for_food_match) %>% 
  arrange(source_fct)


## Checking NV for Energy_standardised calculation ----

#├ 1) Carbohydrates ----

#IN17 did not reported CHO for fish (assumed zero?)
#CHOAVLDFg
fao_fish_fct %>% filter(is.na(CHOAVLDFg)) %>% count(source_fct)
#CHOCDFg
fao_fish_fct %>% filter(is.na(CHOAVLDFg), !is.na(CHOCDFg)) %>% count(source_fct)
#CHOAVLg
fao_fish_fct %>% filter(is.na(CHOAVLDFg), !is.na(CHOAVLg)) %>% count(source_fct)
#CHOAVLMg
fao_fish_fct %>% filter(is.na(CHOAVLDFg), !is.na(CHOAVLMg)) %>% count(source_fct)
#CHOCSMg - Not available
#fao_fish_fct %>% filter(is.na(CHOAVLDFg), !is.na(CHOCSMg)) %>% count(source_fct)

# Checking values that the CHOAVLDFg calculated was assumed zero, but fractions
#of were higher than zero. 

subset(fao_fish_fct, #str_detect(comment, "CHOAVLDFg_std assumed zero") &
       CHOAVLDFg_std == 0 &  
       (CHOCDFg > 0 & FIBTGg == 0| CHOAVLg >0), 
       select = c(fdc_id, source_fct, CHOAVLDFg_std,
                  CHOAVLDFg, CHOCDFg, FIBTGg, CHOAVLg, CHOAVLMg)) %>% distinct() %>% View()

id_to_check <- subset(fao_fish_fct, str_detect(comment, "CHOAVLDFg_std assumed zero") &
         (CHOCDFg > 0  | CHOAVLg >0), 
       select = c(fdc_id, source_fct, CHOAVLDFg_std,
                  CHOAVLDFg, CHOCDFg, CHOAVLg, CHOAVLMg)) %>% distinct() %>% 
  pull(fdc_id)


subset(fao_fish_fct, fdc_id %in% id_to_check, 
       select = c(WATERg, PROCNTg, FAT_g_standardised, FIBTGg, ALCg, ASHg, CHOCDFg))


#├ 2) Proteins ----

fao_fish_fct %>% filter(is.na(PROCNTg)) %>% count(source_fct)
#fixed
#fao_fish_fct %>% filter(is.na(PROCNTg), !is.na(PROTCNTg)) %>%  count(source_fct)

#├ 3) Fats ----

fao_fish_fct %>% filter(is.na(FATg)) %>% count(source_fct)
fao_fish_fct %>% filter(is.na(FATg), !is.na(FAT_g)) %>% count(source_fct)
fao_fish_fct %>% filter(is.na(FATg), !is.na(FATCEg)) %>% count(source_fct)
fao_fish_fct %>% filter(is.na(FAT_g_standardised)) %>% count(source_fct)
#JA15 lipid fixed
#fao_fish_fct %>% filter(fdc_id == "10130") %>% select(Lipid_g)

#├ 4) Fibre -----

#Checking fibre fractions available
fao_fish_fct %>% select(starts_with("FIB")) 

#IN17 did not reported Fibre for fish (assumed zero?)
fao_fish_fct %>% filter(is.na(FIBTGg)) %>% count(source_fct)

#Checking values of other fractions when dietary fibre was missing
fao_fish_fct %>% filter(is.na(FIBTGg), !is.na(FIBCg)) %>% count(source_fct)
#Checking values for Crude fibre when dietary fibre was missing
fao_fish_fct %>% filter(is.na(FIBTGg), !is.na(NSPg)) %>% count(source_fct)

#├ 5) Alcohol ------
#Assumed zero
fao_fish_fct %>% filter(!is.na(ALCg)) %>% count(source_fct)
fao_fish_fct %>% filter(!is.na(ALCg)) %>% select(source_fct, ALCg)

###  Ash ------
fao_fish_fct %>% filter(is.na(ASHg)) %>% count(source_fct)
fao_fish_fct %>% filter(is.na(ASHg)) %>% count(source_fct)



## Checking Sum of Proximate  ----

n <- length(fao_fish_fct$food_desc[fao_fish_fct$SOP_std <95|fao_fish_fct$SOP_std >105])
hist(fao_fish_fct$SOP_std, main = "Sum of Proximate",
     xlab = paste0("There are ", n, " fish items outside range (95-105g)"))
abline(v = 95, col = 2, lwd=3, lty =2)
abline(v = 105, col = 2, lwd=3, lty =2)


# 3.4.3. Retinol & 5.2.3. Retinol ----

variables <- c("RETOLmcg", "VITA_RAEmcg", "VITAmcg", "CARTBEQmcg", "ICS.FAOSTAT.SUA.Current.Code")
fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code <- as.factor(fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code)


subset(fao_fish_fct, is.na(RETOLmcg) & is.na(CARTBEQmcg) & 
         (!is.na(VITA_RAEmcg)| !is.na(VITAmcg)), 
       select = variables)

fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code <- as.factor(fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code)

boxplot(as.numeric(RETOLmcg) ~ ICS.FAOSTAT.SUA.Current.Code, 
        data = fao_fish_fct, 
        ylab = "Retinol (mcg)",
        xlab = "ICS FAOSTAT SUA Fisheries Code") 


## Suppl. Figure X - Back-calculation of retinol. (use with Plot (1)).

# Checking missing values 
subset(fao_fish_fct, is.na(RETOLmcg)) %>% count(ICS.FAOSTAT.SUA.Current.Code) %>% 
  arrange(desc(n))

#Checking missing values by category and the perc. of each
fao_fish_fct %>%  group_by(ICS.FAOSTAT.SUA.Current.Code, ics_faostat_sua_english_description) %>% 
  summarise(retol = sum(is.na(RETOLmcg)), 
            VitARAE = sum(is.na(VITA_RAEmcg)), 
            total = length(RETOLmcg),
            perc = retol/total*100) %>% 
  arrange(desc(perc))

# Checking fish entries for that codes

subset(fao_fish_fct,
       ICS.FAOSTAT.SUA.Current.Code %in% c("1580", "1582", "1583"), 
       select = variables) 


##├├ Plot (1): Missing values by ICS code ----
fao_fish_fct[, variables ] %>%  #selecting variables
  naniar::gg_miss_fct(., fct = ICS.FAOSTAT.SUA.Current.Code) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))







sd(as.numeric(fao_fish_fct$RETOLmcg), na.rm = T)



# 3.4.3. Beta-Carotene equivalents ----

subset(fao_fish_fct, !grepl("CARTBEQmcg_std calculated from", comment) &
                !grepl("CARTBEQmcg_std back", comment) &
         grepl("CARTBEQmcg_std", comment),
       select = comment)

# Checking:
# CARTBEQmcg_std imputed with CARTBEQmcg
# CARTBEQmcg_std calculated from CARTBmcg, CARTAmcg and CRYPXBmcg but only CARTB was used
# CARTBEQmcg_std back calculated from VITA_RAEmcg and VITAmcg

subset(fao_fish_fct, 
         grepl("CARTBEQmcg_std calculated from CARTBmcg, CARTAmcg and CRYPXBmcg but only CARTB was used", comment),
       select = comment)


subset(fao_fish_fct, 
       grepl("RETOLmcg value re-calulated from VITA_RAEmcg and VITAmcg", comment),
       select = comment)

