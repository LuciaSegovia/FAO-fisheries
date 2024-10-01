library(tidyverse)

#plan:
# Extract the summary rows
# run functions
# replace summary rows into table
# return refreshed table

#list of inputs: 
#"WATERg", "PROCNTg", "FAT_g_standardised", #Change FAT_g to FAT_g_standardised
#"CHOAVLg", "FIBTGg_std", "ALCg", "ASHg_std"
#"CARTBmcg", "CARTAmcg", "CRYPXBmcg"
#"RETOLmcg", "CARTBEQmcg"
#"THIAmg", "THIAHCLmg"
#"NIAEQmg", "TRPmg", "NIAmg"


# Uncomment and run for debugging, if going block by block
# df1 <- results_table
# group_ID_col <- 1
# ENERCKcal_std = "ENERCkcal_std"
# ENERCKj_std = "ENERCkj_std"
# SOP_std = "SOPg_calculated"
# CHOAVLDFg_std = "CHOAVLDFg_calculated"
# WATERg = "WATERg"
# PROCNTg = "PROCNTg"
# FAT_g_standardised = "FAT_g_combined"
# CHOAVLg = "CHOAVLg"
# FIBTGg_std = "FIBTGg_std"
# ALCg = "ALCg"
# ASHg_std = "ASHg"
# CARTBEQmcg_std = "CARTBEQmcg_std"
# CARTBmcg = "CARTBmcg"
# CARTAmcg = "CARTAmcg"
# CRYPXBmcg = "CRYPXBmcg"
# VITA_RAEmcg_std = "VITA_RAEmcg_std"
# VITAmcg_std = "VITAmcg_std"
# RETOLmcg = "RETOLmcg"
# CARTBEQmcg = "CARTBEQmcg"
# THIAmg_std = "THIAmg_std"
# THIAmg = "THIAmg"
# THIAHCLmg = "THIAHCLmg"
# NIAEQmg = "NIAEQmg"
# TRPmg = "TRPmg"
# NIAmg = "NIAmg"
# comment = "comments"


Grp_Smrsr_row_update <- function(df1, group_ID_col, 
                                 
                                 ENERCKcal_std = "ENERCkcal_std", ENERCKj_std = "ENERCkj_std",
                                 SOP_std = "SOPg_calculated", 
                                 CHOAVLDFg_std = "CHOAVLDFg_calculated", WATERg = "WATERg", 
                                 PROCNTg = "PROCNTg", FAT_g_standardised = "FAT_g_combined", 
                                 CHOAVLg = "CHOAVLg", FIBTGg_std = "FIBTGg_std", ALCg = "ALCg", 
                                 ASHg_std = "ASHg", CARTBEQmcg_std = "CARTBEQmcg_std",
                                 CARTBmcg = "CARTBmcg", CARTAmcg = "CARTAmcg", CRYPXBmcg = "CRYPXBmcg", 
                                 VITA_RAEmcg_std = "VITA_RAEmcg_std", VITAmcg_std = "VITAmcg_std", 
                                 RETOLmcg = "RETOLmcg", CARTBEQmcg = "CARTBEQmcg", THIAmg_std = "THIAmg_std", 
                                 THIAmg = "THIAmg", THIAHCLmg = "THIAHCLmg", NIAEQmg = "NIAEQmg", TRPmg = "TRPmg",
                                 NIAmg = "NIAmg", comment = "comments"){
  
  summary_row_table <- subset(df1, grepl("SUMMARY ROW - ", df1[, group_ID_col]))
  old_summary_row_table <- summary_row_table
  
  
  
  #summary_row_table[, CARTBEQmcg_std] = (1 * as.numeric(summary_row_table[, CARTBmcg]) + 0.5 * as.numeric(summary_row_table[, CARTAmcg]) + 0.5 * as.numeric(summary_row_table[, CRYPXBmcg]))
  
  summary_row_table[, VITA_RAEmcg_std] = (as.numeric(summary_row_table[, RETOLmcg]) + (1 / 12 * as.numeric(summary_row_table[, CARTBEQmcg_std])))
  
  summary_row_table[, VITAmcg_std] = (as.numeric(summary_row_table[, RETOLmcg]) + (1 / 6 * as.numeric(summary_row_table[, CARTBEQmcg_std])))
  
  #summary_row_table[, THIAmg_std] = case_when(
  # !is.na(summary_row_table[, THIAmg]) ~ summary_row_table[, THIAmg],
  # is.na(summary_row_table[, THIAmg]) ~ summary_row_table[, THIAHCLmg]
  #)
  
  summary_row_table[, comment] =  ifelse(is.na(summary_row_table[, ALCg]), ifelse(is.na(summary_row_table[, comment]), 
                                                                                                   "ALC(g) standardised assumed 0",
                                                                                                   paste0(summary_row_table[, comment], ", ALC(g) standardised assumed 0")), NA)
  
  summary_row_table[, ALCg] =  ifelse(is.na(summary_row_table[, ALCg]), 0, summary_row_table[, ALCg])
  
  summary_row_table[, CHOAVLDFg_std] = (100 - (as.numeric(summary_row_table[, WATERg]) + as.numeric(summary_row_table[, PROCNTg]) + as.numeric(summary_row_table[, FAT_g_standardised]) + as.numeric(summary_row_table[, FIBTGg_std]) + as.numeric(summary_row_table[, ASHg_std]) + as.numeric(summary_row_table[, ALCg])))
  
  summary_row_table[, comment] =  ifelse(as.numeric(summary_row_table[, CHOAVLDFg_std]) <0, ifelse(is.na(summary_row_table[, comment]), 
                                                                                            "CHOAVLDF(g) standardised assumed 0",
                                                                                            paste0(summary_row_table[, comment], ", CHOAVLDF(g) standardised assumed 0")), NA)
  
  summary_row_table[, CHOAVLDFg_std] =  ifelse(as.numeric(summary_row_table[, CHOAVLDFg_std]) <0, 0, as.numeric(summary_row_table[, CHOAVLDFg_std]))
  
  summary_row_table[, ENERCKcal_std] <- as.numeric(summary_row_table[, PROCNTg])*4 + as.numeric(summary_row_table[, FAT_g_standardised])*9 + as.numeric(summary_row_table[, CHOAVLDFg_std])*4 + as.numeric(summary_row_table[, FIBTGg_std])*2 + as.numeric(summary_row_table[, ALCg])*7
  summary_row_table[, ENERCKj_std] <- as.numeric(summary_row_table[, PROCNTg])*17 + as.numeric(summary_row_table[, FAT_g_standardised])*37 + as.numeric(summary_row_table[, CHOAVLDFg_std])*17 + as.numeric(summary_row_table[, FIBTGg_std])*8 + as.numeric(summary_row_table[, ALCg])*29
  
  summary_row_table[, SOP_std] = as.numeric(summary_row_table[, WATERg]) +
    as.numeric(summary_row_table[, PROCNTg]) +
    as.numeric(summary_row_table[, FAT_g_standardised]) +
    as.numeric(summary_row_table[, CHOAVLDFg_std]) +
    as.numeric(summary_row_table[, FIBTGg_std]) +
    as.numeric(summary_row_table[, ALCg]) +
    as.numeric(summary_row_table[, ASHg_std])
  
 # summary_row_table[, nia_conversion_std] = case_when(
 #   !is.na(summary_row_table[, NIAmg]) ~ summary_row_table[, NIAmg],
 #   is.na(summary_row_table[, NIAmg]) ~ (as.numeric(summary_row_table[, NIAEQmg]) - (1 / 60 * as.numeric(summary_row_table[, TRPmg])))
 # )
  
  print(paste0("summary row table has nrow ", nrow(summary_row_table), " and ncol ", ncol(summary_row_table)))
  
  output_table <- df1
  
  for (i in 1:nrow(summary_row_table)){
    
    #print(paste0("got to here 3.", i))
    
    row_ID <- summary_row_table[i, group_ID_col]
    
    print(row_ID)
    
    message(paste0(row_ID, " - Previous SOP_std value: ", old_summary_row_table[i, SOP_std], " - new SOP_std value: ", summary_row_table[i, SOP_std]))
    message(paste0(row_ID, " - Previous VITA_RAEmcg_std value: ", old_summary_row_table[i, VITA_RAEmcg_std], " - new VITA_RAEmcg_std value: ", summary_row_table[i, VITA_RAEmcg_std]))
    message(paste0(row_ID, " - Previous VITAmcg_std value: ", old_summary_row_table[i, VITAmcg_std], " - new VITAmcg_std value: ", summary_row_table[i, VITAmcg_std]))
    message(paste0(row_ID, " - Previous CHOAVLDFg_std value: ", old_summary_row_table[i, CHOAVLDFg_std], " - new CHOAVLDFg_std value: ", summary_row_table[i, CHOAVLDFg_std]))
    message(paste0(row_ID, " - Previous ENERCKcal_std value: ", old_summary_row_table[i, ENERCKcal_std], " - new ENERCKcal_std value: ", summary_row_table[i, ENERCKcal_std]))
    message(paste0(row_ID, " - Previous ENERCKj_std value: ", old_summary_row_table[i, ENERCKj_std], " - new ENERCKj_std value: ", summary_row_table[i, ENERCKj_std]))
    
    output_table[output_table[, group_ID_col] == row_ID, ] <- summary_row_table[summary_row_table[, group_ID_col] == row_ID, ]
  }
  
  return(output_table)
}