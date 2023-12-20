
# This function replace "trace" ('tr|[tr]|Tr') to zero 
# the string for trace "tr|[tr]|Tr" can be modified
# as well as the zero to any other value.
# TODO: change the string1 to "tr|[tr]|trace" & change ignore.case = TRUE 
### str_which(pull(dataset[, var1]), regex(string1, ignore.case = TRUE))

TraceToZero <- function(dataset, vars.column, string1 = 'tr|[tr]|Tr', string2 = "0"){
  
  if(sum(names(dataset)=="comments") == 0){
    dataset$comments <- NA
  }
  
  for(i in 1:length(vars.column)){
    var1 <- vars.column[i]
    
    if(grepl(string1, wafct[, var1]) == FALSE){next}
    
    else{
    #  text <- paste0(var1, " changed ", string1, " to ", string2) 
      text <- paste0("Changed trace value to ", string2, " in ", var1) 
      row1 <- str_which(pull(dataset[, var1]), string1)
      
      for(j in 1:length(row1)){
        dataset$comments[row1][j] <- ifelse(!is.na(dataset$comments[row1][j]), 
                                            paste0(dataset$comments[row1][j], ";", text), 
                                            paste0(text))
        
        dataset[row1[j], var1] <- gsub(string1, string2, dataset[row1[j], var1])
        
      }
      
    }
    
  }
  
  return(dataset)
}

