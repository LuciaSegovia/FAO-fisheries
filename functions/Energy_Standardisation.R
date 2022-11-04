library(tidyverse)
library (docstring)

ENERCKj_standardised <- function(PROT, FAT, CHOAVLDF, FIBGTg, ALC){
  
  
  #' A function used to calculate standardized Energy values in kJ
  #' 
  #' @description This function works as a basic calculator - The values for 
  #' Total Protein (PROT), Total Fat (FAT), Available Carbohydrate (CHOAVLDF), 
  #' Fibre, Total Dietary (FIBTG) and Alcohol (ALC). Alcohol is optional, whereas
  #' the other inputs are required - if Alcohol is missing it is assumed to be
  #' 0.
  #' 
  #' @param PROT Required - The Total Protein value (in grams) for the food 
  #' item being examined.
  #' @param FAT Required - The Total Fat value (in grams) for the food item 
  #' being examined.
  #' @param CHOAVLDF Required - The Total Available Carbohydrate value (in 
  #' grams) for the food item being examined.
  #' @param FIBTGg Required - The Total Dietary Fibre value (in grams) for the 
  #' food item being examined.
  #' @param ALC Optional - The Total Alcohol value (in grams) for the food item 
  #' being examined.
  #' 
  #' @return The calculated Energy value in kJ.
  #' 
  #' @examples Three examples will be covered - two variants for a one-off 
  #' calculation, and to create a column with the calculated results.
  #' 
  #' Single calculation:
  #' 
  #' Bread, wheat, white, unfortified
  #' 
  #' Protein_value <- 7.5
  #' Fat_value <- 1.3
  #' Carb_value <- 50.5
  #' Fibre_value <- 2.9
  #' Alcohol_value <- 0
  #' 
  #' standardised_kJ <- ENERCKj_standardised(PROT = Protein_value, FAT = Fat_value, 
  #' CHOAVLDF = Carb_value, FIBTG = Fibre_value, ALC = Alcohol_value)
  #' 
  #' alternatively:
  #' 
  #' standardised_kJ <- ENERCKj_standardised(PROT = 7.5, FAT = 1.3, 
  #' CHOAVLDF = 50.5, FIBTG = 2.9, ALC = 0)
  #' 
  #' data.frame calculation:
  #' 
  #' First, an example dataframe is outlined and created - 
  #' 
  #' test_df_WAFCT2019 <- data.frame(
  #' c("Bread, wheat, white, unfortified",
  #' "Beer, European (4.6% v/v alcohol)",
  #' "Maize, yellow, meal, whole grains, unfortified",
  #' "Sweet potato, yellow flesh, raw",
  #' "Cassava, tuber, white flesh, raw"),
  #' c(7.5, 0.3, 9.4, 1.5, 1.3),
  #' c(1.3, 0, 3.7, 0.2, 0.3),
  #' c(50.5, 3.7, 65.2, 25.5, 31.6),
  #' c(2.9, 0, 9.4, 3, 3.7),
  #' c(0, 3.6, 0, NA, 0))
  #' 
  #' Then, the columns are renamed:
  #' 
  #' colnames(test_df_WAFCT2019) <- c("food_name", "protein", "fat", "carbs",
  #' "fb", "alcohol")
  #' 
  #' Once renamed, the function is applied. the assigned output is a new column 
  #' in the data.frame, and the inputs are the different columns detailing the 
  #' relevant food nutrient values.
  #' 
  #' test_df_WAFCT2019$ENERCKj_stnd <- ENERCKj_standardised(
  #'          test_df_WAFCT2019$protein, 
  #'          test_df_WAFCT2019$fat, 
  #'          test_df_WAFCT2019$carbs,
  #'          test_df_WAFCT2019$fb,
  #'          test_df_WAFCT2019$alcohol)

  
  
  ALC <- ALC %>% replace_na(0)
  FIBGTg <- replace_na(as.numeric(FIBGTg), 0)
  ENERCKj_std <- as.numeric(PROT)*17 + as.numeric(FAT)*37 + as.numeric(CHOAVLDF)*17 + as.numeric(FIBGTg)*8 + as.numeric(ALC)*29
  return(ENERCKj_std)
}

ENERCKcal_standardised <- function(PROTg, FATg_standardised, CHOAVLDFg, FIBGTg, ALCg){
  ALCg <- ALCg %>% replace_na(0)
  FIBGTg <- replace_na(as.numeric(FIBGTg), 0)
  ENERCKcal_std <- PROTg*4 + FATg_standardised*9 + CHOAVLDFg*4 + as.numeric(FIBGTg)*2 + ALCg*7
  return(ENERCKcal_std)
}

test_df_WAFCT2019 <- data.frame(c("Bread, wheat, white, unfortified",
                                   "Beer, European (4.6% v/v alcohol)",
                                   "Maize, yellow, meal, whole grains, unfortified",
                                   "Sweet potato, yellow flesh, raw",
                                   "Cassava, tuber, white flesh, raw"),
                       c(7.5, 0.3, 9.4, 1.5, 1.3),
                       c(1.3, 0, 3.7, 0.2, 0.3),
                       c(50.5, 3.7, 65.2, 25.5, 31.6),
                       c(2.9, 0, NA, 3, 3.7),
                       c(0, 3.6, 0, NA, 0))

colnames(test_df_WAFCT2019) <- c("food_name", "protein", "fat", "carbs", "fb", "alcohol")

test_df_WAFCT2019$ENERCKj_standardised <- ENERCKj_standardised(test_df_WAFCT2019$protein, 
                                                     test_df_WAFCT2019$fat, 
                                                     test_df_WAFCT2019$carbs,
                                                     test_df_WAFCT2019$fb,
                                                     test_df_WAFCT2019$alcohol)


test_df_WAFCT2019$ENERCKcal_standardised <- ENERCKcal_standardised(test_df_WAFCT2019$protein, 
                                                     test_df_WAFCT2019$fat, 
                                                     test_df_WAFCT2019$carbs,
                                                     test_df_WAFCT2019$fb,
                                                     test_df_WAFCT2019$alcohol)