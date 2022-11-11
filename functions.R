

#Functions


# Data transformation - #Issue #40
## Variables recalculation

# Energy calculation (kcal, kj) 
source(here::here("functions","Energy_Standardisation.R"))
# ! Tom already generated this function so just incorporate it.
#source("Standardizer_test.R")

# Sum of proximate
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

#Formatting
source(here::here("functions","Group_Summariser.R"))
source(here::here("functions", "Summarised_Row_Recalculator.R"))



#UNDER DEVELOPMENT ----

#This function combine three Tagnames (haven't tested)

function(x, var1 = "", var2 = "", var3 = "", var4 = ""){
  
  x[ncol(x)+1] <- NA
  colnames(x)[ncol(x)+1] <- var4
  
  if (!is.na(x$var1[i])) {
    print(!is.na(x$var1[i]))
    x$var4[i] <- x$var1[i]
  }  
  if (is.na(x$var1[i])) { 
    x$var4[i] <- x$var2[i]
  } 
  if (is.na(x$var1[i]) & is.na(x$var2[i])) {
    x$var4[i] <- x$var3[i]
  }
  if (is.na(x$var1[i]) & is.na(x$var2[i]) & is.na(x$var3[i])) {
    x$var4[i] <- NA
  }
  print(x$var4[i])
}


Standardisation_fn <- function(df, var1, var2, var3, new_col_name = "standardised column"){
  
  df[ncol(df)+1] <- NA
  colnames(df)[ncol(df)+1] <- new_col_name
  
  if (!is.na(df$var1[i])) {
    print(!is.na(df$var1[i]))
    df$var4[i] <- df$var1[i]
  }  
  if (is.na(df$var1[i])) { 
    df$var4[i] <- df$var2[i]
  } 
  if (is.na(df$var1[i]) & is.na(df$var2[i])) {
    df$var4[i] <- df$var3[i]
  }
  if (is.na(df$var1[i]) & is.na(df$var2[i]) & is.na(df$var3[i])) {
    df$var4[i] <- NA
  }
  print(df$var4[i])
}

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






