

#Functions


# Data transformation - #Issue #40
## Variables recalculation

# Energy calculation (kcal, kj)  ----
# source(here::here("functions","Energy_Standardisation.R"))

# Sum of proximate ----
#RETOL - Issue  #46
# Carotene Eq.
#CARTBEQmcg (Carotene Eq.) Issue #44
# Vitamin A, retinol activity eq.
#Vitamin A, retinol eq.
#Thiamin
#Niacin (From niacin eq.)
source(here::here("functions","summary_table_functions.R"))

#SOP_std_creator()
#CHOAVLDFg_std_creator()
#CARTBEQmcg_std_creator()
#VITA_RAEmcg_std_creator()
#VITAmcg_std_creator()

#This function combine three Tagnames ----
source(here::here("functions","nutri_combiner.R"))

#Formatting - Nutritools
# source(here::here("functions","Group_Summariser.R"))
# source(here::here("functions", "Summarised_Row_Recalculator.R"))



#UNDER DEVELOPMENT ----


#Function to remove brackets and replace trace to zero
#The following f(x) removes [] and changing tr w/ 0

no_brackets_tr <- function(i){
  case_when(
    str_detect(i, 'tr|[tr]|Tr') ~ "0",
    str_detect(i, '\\[.*?\\]')  ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    TRUE ~ i)
}

#This function replace "trace" to zero (use w/ apply function for data.frames)
#Changing "Tr" to zero

TraceToZero <- function(x){
  x <- gsub("Trace|trace|[tr]|Tr|tr", "0", x)
  return(x) 
}






