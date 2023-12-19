

#Functions


# Data transformation - #Issue #40
## Variables recalculation

# Energy calculation (kcal, kj)  ----
# source(here::here("functions","Energy_Standardisation.R"))

# Summary table functions: ----
# Beta-carotene functions are now in NutritionTools package
# Sum of proximate
# RETOL - Issue  #46
# Vitamin A, retinol activity eq.
# Vitamin A, retinol eq.
# Thiamin
# Niacin (From niacin eq.)
source(here::here("functions","summary_table_functions.R"))

#SOP_std_creator()
#CHOAVLDFg_std_creator()
#VITA_RAEmcg_std_creator()
#VITAmcg_std_creator()

#This function combine three Tagnames ----
source(here::here("functions","nutri_combiner.R"))

#Formatting - Nutritools
# source(here::here("functions","Group_Summariser.R"))
# source(here::here("functions", "Summarised_Row_Recalculator.R"))




# Function that replace "trace" values to zero
## zero can be substituted to a different value

source(here::here("functions", "TraceToZero.R"))

#UNDER DEVELOPMENT ----


#Function to remove brackets
#The following f(x) removes [] 

no_brackets_tr <- function(i){
    case_when(
    str_detect(i, '\\[.*?\\]')  ~ str_extract(i, '(?<=\\[).*?(?=\\])'),
    TRUE ~ i)
}








