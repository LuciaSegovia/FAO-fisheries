
## Checking and loading updates

# FCTs that are not publicly available would not run w/ the files()
# Getting the folder/ script available for running
source_fct_name <- grep("^[[:upper:]]{2}[[:digit:]]{2}$", dir(), value = TRUE)

for(i in source_fct_name){
    source(paste0(i, "/", i, "_FCT_FAO_Tags.R"))

  }

