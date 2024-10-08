library(tidyverse)
library (docstring)

Group_Summariser <- function(df1, group_ID_col, secondary_sort_col, sep_row = F, seq_col = F, weighting_col = F){
  
  #' Insert summary rows between groups of rows in a data frame
  #' 
  #' @description This function analyses a data frame, sorting it based on the groups detailed in the group_ID_col,
  #' and inserts summary/mean rows in between each group. 
  #' 
  #' For this to work the data frame must be structured such that it has a group ID column of some sort, where the group
  #' of each item is listed. All the data columns that need to be averaged need to be numeric also. 
  #' 
  #' A secondary option is for the sorting of items within their groups, using the secondary_sort_col parameter. 
  #' 
  #' @param df1 Required - The data.frame that summary rows need to be inserted into.
  #' @param group_ID_col Required - The column name specifying the groups that summary rows are created for.
  #' @param secondary_sort_col Optional - Specify the column that the results should be sorted by after they're sorted into groups.
  #' @param sep_row Optional - if set to \code{TRUE}, The Summariser will insert an empty row after each summary row, to help reading and separation.
  #' @param seq_col Optional - if set to \code{TRUE}, The Summariser will insert a sequence column, numbering each item that goes into a summary row.
  #' @param weighting_col Optional - if set to \code{TRUE}, The Summariser will insert a weighting factor for each item that goes into a summary row.
  #' @return A data.frame that mirrors \code{df1}, but after each group a summary row is inserted, containing the mean of the data columns.
  
  
  # Data input checking ----
  
  #These checks are run on the inputs to make sure the data frames are data frames, and that the string input is just a string, and the string inputs are a legitimate column name
  
  numeric_cols <- select_if(df1, is.numeric)
  
  example_colname <- colnames(df1[1])
  
  stopifnot("df1 is not a data frame - please input a data frame" = is.data.frame(df1))
  stopifnot("df1 contains no numeric columns. Please ensure data columns are numeric" = length(numeric_cols) != 0)
  stopifnot("The group_ID_col is not a character or string - please input a character or string that is a column name in df1, e.g. 'column one'" = is.character(group_ID_col))
  stopifnot("The group_ID_col is not a column name in df1 - please input a string that is a column name in df1, e.g. 'column one'" = group_ID_col %in% colnames(df1))

  if(!missing(secondary_sort_col)){
    stopifnot("The secondary_sort_col is not a character or string - please input a character or string that is a column name in df1, e.g. 'column one'" = is.character(secondary_sort_col))
    stopifnot("The secondary_sort_col is not a column name in df1 - please input a string that is a column name in df1, e.g. 'column one'" = secondary_sort_col %in% colnames(df1))
  }
  
  
  # Table sorting ----
  
  #This makes sure the group ID column is a character string, finds the unique ID's and sorts by them
  
  df1[[group_ID_col]] <- as.character(df1[[group_ID_col]])
  group_IDs <- df1[[group_ID_col]]
  group_ID_list <- sort(unique(group_IDs))

  #This creates a new table, which will be the basis for the results table, using a subset of the main table only including the first group ID
  df1 <- as.data.frame(df1)
  sorted_table <- df1[df1[[group_ID_col]] == group_ID_list[1],]
  if(!missing(secondary_sort_col)){
    sorted_table <- sorted_table[order(sorted_table[, secondary_sort_col]),]
  }

  
  # Starting table setup ----
  
  #This creates a list for a new row, then iterates over the first subtable, generated averaged values where appropriate for the data columns.
  
  new_row <- c()

  for (i in 1:ncol(sorted_table)){
    new_row_entry <- NA
    unique_entries <- unique(sorted_table[[i]])
    if(length(unique_entries) == 1){
      if(paste(unique_entries) == "NA"){
        new_row_entry <- NA
      } else {
        new_row_entry <- paste(unique_entries)
        }
      } else if (is.numeric(sorted_table[[i]])) {
        new_row_entry <- mean(sorted_table[[i]], na.rm = TRUE)
      }
    new_row <- append(new_row, new_row_entry)
  }
  
  group_col_num <- which(colnames(sorted_table) == group_ID_col)
  group_ID_value <- new_row[group_col_num]
  new_row[group_col_num] <- paste0("SUMMARY ROW - ", group_ID_value)
  sorted_table <- rbind(sorted_table, new_row)
  
  if(weighting_col == T){
    sorted_table$Weighting_Factor <- c(replicate((nrow(sorted_table) - 1), round((1/(nrow(sorted_table) - 1)), 2)), "")
  }
  if(seq_col == T){
    sorted_table$Sequence <- c(1:(nrow(sorted_table) - 1), "")
  }
  if(sep_row == T){
    sorted_table[nrow(sorted_table) + 1,] <- ""
  }

  
  # Secondary tables setup ----
  
  #This repeats the process above, appending the results to the starter table.
  
  for (i in 2:length(group_ID_list)){
    
    secondary_table <- df1[df1[group_ID_col] == group_ID_list[i],]

    if(!missing(secondary_sort_col)){
      secondary_table <- secondary_table[order(secondary_table[secondary_sort_col]),]
    }
    
    
    new_row <- c()
    
    for (j in 1:ncol(secondary_table)){
      new_row_entry <- NA
      unique_entries <- unique(secondary_table[[j]])
      if(length(unique_entries) == 1){
        if(paste(unique_entries) == "NA"){
          new_row_entry <- NA
        } else {
          new_row_entry <- paste(unique_entries)
        }
      } else if (is.numeric(secondary_table[[j]])) {
        new_row_entry <- mean(secondary_table[[j]], na.rm = TRUE)
      }
      new_row <- append(new_row, new_row_entry)
    }
    group_col_num <- which(colnames(secondary_table) == group_ID_col)
    group_ID_value <- new_row[group_col_num]
    new_row[group_col_num] <- paste0("SUMMARY ROW - ", group_ID_value)
    
    secondary_table <- rbind(secondary_table, new_row)
    
    if(weighting_col == T){
      secondary_table$Weighting_Factor <- c(replicate((nrow(secondary_table) - 1), round((1/(nrow(secondary_table) - 1)), 2)), "")
    }
    if(seq_col == T){
      secondary_table$Sequence <- c(1:(nrow(secondary_table) - 1), "")
    }
    
    sorted_table <- rbind(sorted_table, secondary_table)
    
    if(sep_row == T){
      sorted_table[nrow(sorted_table) + 1,] <- ""
    }

  }
  
  return(sorted_table)

}