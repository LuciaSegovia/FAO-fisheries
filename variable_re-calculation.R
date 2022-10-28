
library(gt)
#If data is not loaded 
source("merging_all.R")
source("functions.R")

# 0) Check that we have all FCTs merged ----

fao_fish_fct %>% 
  group_by(source_fct) %>% count()


# 3. 4) Combining Tagnames to generate variables ----

##├ ) Fibre - standardised  ---- 

# No other fibre fractions available (See "QC.R")
df1$FIBTGg_std <- df1$FIBTGg 


##├ ) Ash - standardised  ---- 

subset(fao_fish_fct, !is.na(ASHg)) %>% count(source_fct)

fao_fish_fct$ASHg_std <- ifelse(is.na(fao_fish_fct$ASHg), fao_fish_fct$ASHg_bydiff, fao_fish_fct$ASHg )
fao_fish_fct$comment <- ifelse(!is.na(fao_fish_fct$ASHg_bydiff), "Ash values were calculated by difference, see documentation", NA)

subset(fao_fish_fct, is.na(ASHg_std)) %>% count(source_fct)

##├  ) Fat - standardised ----

#This function combine all the Tagnames for FAT_g_standardised

for(i in 1:nrow(fao_fish_fct)){
  print(i)
  if (!is.na(fao_fish_fct$FATg[i])) {
    print(!is.na(fao_fish_fct$FATg[i]))
    fao_fish_fct$FAT_g_standardised[i] <- fao_fish_fct$FATg[i]
  }  
  if (is.na(fao_fish_fct$FATg[i])) { 
    fao_fish_fct$FAT_g_standardised[i] <- fao_fish_fct$FAT_g[i]
  } 
  if (is.na(fao_fish_fct$FATg[i]) & is.na(fao_fish_fct$FAT_g[i])) {
    fao_fish_fct$FAT_g_standardised[i] <- fao_fish_fct$FATCEg[i]
  }
  if (is.na(fao_fish_fct$FATg[i]) & is.na(fao_fish_fct$FAT_g[i]) & 
      is.na(fao_fish_fct$FATCEg[i])) {
    fao_fish_fct$FAT_g_standardised[i] <- NA
  }
  print(fao_fish_fct$FAT_g_standardised[i])
}


#├ ) Carbohydrates - standardised ----

#Recalculated variable:

fao_fish_fct  <- fao_fish_fct %>% CHOAVLDFg_std_creator()

##├├ Plot: Missing values for carbohydrates by difference in each FCT ----
fao_fish_fct[,c("CHOAVLDFg_std",  "source_fct")] %>%  #selecting variables
  naniar::gg_miss_fct(., fct = source_fct) #making the plot

##├├ Hist: Carbohydrates by difference ----
# before assumning zero of the negative values
hist(fao_fish_fct$CHOAVLDFg_std)

#No. negative value
sum(fao_fish_fct$CHOAVLDFg_std < 0)

#Checking negative values 
n1 <- which(fao_fish_fct$CHOAVLDFg_std< 0)

fao_fish_fct$comment[n1] <- ifelse(is.na(fao_fish_fct$comment[n1]),
                                   "CHOAVLDFg_std assumed zero", paste(fao_fish_fct$comment, "| CHOAVLDFg_std assumed zero") )
fao_fish_fct$CHOAVLDFg_std[fao_fish_fct$CHOAVLDFg_std< 0] <- 0

#├ ) Energy  ----

#Energy in kcal
fao_fish_fct$ENERCkcal_std <- ENERCKcal_standardised(
  fao_fish_fct$PROCNTg,fao_fish_fct$FAT_g_standardised,
  fao_fish_fct$CHOAVLDFg_std, fao_fish_fct$FIBTGg_std,
  fao_fish_fct$ALCg)

#Energy in kJ
fao_fish_fct$ENERCkJ_std <-  ENERCKj_standardised(
  fao_fish_fct$PROCNTg, fao_fish_fct$FAT_g_standardised,
  fao_fish_fct$CHOAVLDFg_std, fao_fish_fct$FIBTGg_std,
  fao_fish_fct$ALCg)

#├ ) Sum of proximate  ----

fao_fish_fct  <- fao_fish_fct %>% SOP_std_creator() 

#├ )  Vitamin A  ----  

fao_fish_fct  <- fao_fish_fct %>%
VITA_RAEmcg_std_creator() %>%  #This function recalculate VITA_RAEmcg_std (standardised)
  VITAmcg_std_creator()   #This function recalculate VITAmcg_std (standardised)
  

##├ ) VITB6_mg_standardised  ----
#This function combine all the Tagnames for VITB6

for(i in 1:nrow(fao_fish_fct)){
  print(i)
  if (!is.na(fao_fish_fct$VITB6Amg[i])) {
    print(!is.na(fao_fish_fct$VITB6Amg[i]))
    fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6Amg[i]
  }  
  if (is.na(fao_fish_fct$VITB6Amg[i])) { 
    fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6Cmg[i]
  } 
  if (is.na(fao_fish_fct$VITB6Amg[i]) & is.na(fao_fish_fct$VITB6Cmg[i])) {
    fao_fish_fct$VITB6_mg_standardised[i] <- fao_fish_fct$VITB6_mg[i]
  }
  if (is.na(fao_fish_fct$VITB6Amg[i]) & is.na(fao_fish_fct$VITB6Cmg[i]) & is.na(fao_fish_fct$VITB6_mg[i])) {
    fao_fish_fct$VITB6_mg_standardised[i] <- NA
  }
  print(fao_fish_fct$VITB6_mg_standardised[i])
}
