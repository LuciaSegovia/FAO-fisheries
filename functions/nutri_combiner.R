
## This function can be used to combine more than one Tagname of the 
## same nutrient. It adds information to the "comments" variable on the 
## Tagname used. 


nutri_combiner <-  function(data.df, var1, var2, var3, new_var){
  
  text <- paste0(new_var, " equals to ") # metadata in comments variable
  
  data.df[, new_var] <- NA
  
  # Loop that prioritise in the other of the variables defined above (1->3)
  for(i in 1:nrow(data.df)){
    print(i)
    if (!is.na(data.df[i, var1])) {
      print(!is.na(data.df[i, var1]))
      data.df[i, new_var] <- data.df[i, var1]
      data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]), 
                                       paste0(data.df[i, "comments"], ";", text, var1), 
                                       paste0(text, var1))
      
      
    }  else if (is.na(data.df[i, var1]) & !is.na(data.df[i, var2])) { 
      data.df[i, new_var] <- data.df[i, var2]
      data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]), 
                                       paste0(data.df[i, "comments"], ";", text, var2), 
                                       paste0(text, var2))
      
    } 
    if (is.na(data.df[i, var1]) & is.na(data.df[i, var2]) & !is.na(data.df[i, var3])) {
      data.df[i, new_var] <- data.df[i, var3]
      data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]), 
                                       paste0(data.df[i, "comments"], ";", text, var3), 
                                       paste0(text, var3))
      
    }
    if (is.na(data.df[i, var1]) & is.na(data.df[i, var2]) & is.na(data.df[i, var3])) {
      data.df[i, new_var] <- NA
    }
    print(data.df[i, new_var])
  }
  
  return(data.df)
  
}
