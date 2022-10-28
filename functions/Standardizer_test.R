library(tidyverse)
library (docstring)


test_df <- data.frame (c(2, NA, 5, NA, NA),
                       c(NA, NA, NA, NA, 4),
                       c(NA, 7, 10, NA, 3))

colnames(test_df) <- c("fat1", "fat2", "fat3")



Standardisation_fn <- function(df, var1, var2, var3, new_col_name = "standardised column"){
  
  df[ncol(df)+1] <- NA
  colnames(df)[ncol(df)] <- new_col_name
  
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
