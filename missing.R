
##### Dealing with missing values ######

#Loading the data
source("variable_re-calculation.R")


# 7) SEmcg ---- 


#Fixing ""Other pelagic fish, cured"
#70*(100-14.6)/(100-62.1) 
  
  #Running Table: Data available (count) per fish category and nutrient in 
  #"example_visuals.R"
  #Bottom of the x1 table in Semcg. 
  
  ## Checking the data 
  
  #ICS FAOSTAT description of the missing values
  
  missing <- c("Cephalopods, cured", "Aquatic animals nei, cured", 
               "Aquatic animals nei, preparations nei", "Crustaceans, cured", 
               "Cephalopods, canned")
  
  # Checking the items included reporting other NVs for that categories
  fao_fish_fct %>% 
    filter(ics_faostat_sua_english_description %in% missing)  %>%
    select(food_desc, SEmcg, source_fct)
  
  #Snail - should be excl. form cephalopods?
  fao_fish_fct %>% 
    filter( fdc_id == "10291") 
  
  fao_fish_fct %>% 
    filter(fish_type %in% c("Cephalopods", 
                            "Aquatic animals nei", 
                            "Crustaceans")) %>%
    select(food_desc, SEmcg, source_fct)
  
  ##├├ Cephalopods, cured ---- 
  #squids
  
  fao_fish_fct %>% 
    filter(fish_type == "Cephalopods", 
           source_fct == "JA15",
           !is.na(SEmcg)) %>%
    select(fdc_id,food_desc, fish_prep, WATERg,  SEmcg)
  
  
  #Squid - calculating median water and Se
  
 # fao_fish_fct$comment <- NA
  
  squid <- c("10342", "10345")
  squid_fct <- unique(fao_fish_fct$source_fct[fao_fish_fct$fdc_id %in% squid])
  
  fao_fish_fct %>% 
    filter( fdc_id %in% squid) %>%
    select(fdc_id,food_desc, WATERg,  SEmcg) 
  
  x <- median(as.numeric(fao_fish_fct$SEmcg[fao_fish_fct$fdc_id %in% squid]))
  y <- median(as.numeric(fao_fish_fct$WATERg[fao_fish_fct$fdc_id %in% squid]))
  
  #checking values to be replaced
  subset(fao_fish_fct, fdc_id %in% c("10350", "10353", "10354", "10355"),
         select = c(food_desc, WATERg, SEmcg))
  
  #A loop that replace the NA value to the calculated SEmcg
  #based on the water conversion formulae
  
  ids <- c("10350", "10353", "10354", "10355")
  
  for(id in ids){
    
    i <- which(fao_fish_fct$fdc_id == id)
    
    fao_fish_fct$SEmcg[i] <- x*(100-as.numeric(fao_fish_fct$WATERg[i]))/(100-y)
    
    fao_fish_fct$comment[i] <- ifelse(is.na(fao_fish_fct$comment[i]), 
                                      paste0("SEmcg value from water adjusted, median values ", squid_fct, "(", toString(squid), ")"),
                                      paste0(fao_fish_fct$comment[i],", ",
                                             "SEmcg value from water adjusted, median values ", squid_fct, "(", toString(squid), ")"))
    
  }
  
  
  ##├├ Cephalopods, canned ----
  # Snail == 10291
  #Checking the items available
  fao_fish_fct %>% 
    filter(fish_type == "Cephalopods", fish_prep == "canned")
  
  fao_fish_fct %>% 
    filter(fish_type == "Cephalopods", 
           fish_prep %in% c("preparations nei"), 
           str_detect(food_desc, "boil|moist"), 
           #  source_fct == "JA15",
           !is.na(SEmcg)) %>%
    select(fdc_id,food_desc, WATERg,  SEmcg, source_fct) %>% 
    mutate_at(c("WATERg", "SEmcg"), as.numeric) %>% 
    mutate_if(is.numeric, ~round(., 2)) %>% 
    knitr::kable()
  
  dim(fao_fish_fct)
  
  fao_fish_fct <- fao_fish_fct %>%
    filter(fish_type == "Cephalopods", 
           fish_prep %in% c("preparations nei"), 
           str_detect(food_desc, "boil|moist"), 
           #  source_fct == "JA15",
           !is.na(SEmcg)) %>% 
    mutate(ics_faostat_sua_english_description = "Cephalopods, canned",
           ICS.FAOSTAT.SUA.Current.Code = "1573",
           fish_prep = "canned") %>% 
    rbind(., fao_fish_fct)
  
  dim(fao_fish_fct)
  
  
  ##├├ Aquatic animals nei ---- 
  #sea urchin, jelly fish,
  #sea cucumber, and sea squirt
  fao_fish_fct %>% 
    filter(fish_type == "Aquatic animals nei", 
           fish_prep %in% c("cured", "preparations nei"), 
           # source_fct == "JA15",
           #  !is.na(SEmcg)
    ) %>%
    select(fdc_id, food_desc, WATERg,  SEmcg, fish_prep) 
  
  #Aquatic animals nei - cured
  #Sea cucumber 
  
  #Aquatic animals nei - preparations nei
  
  #No info on aquatic animals, nei, preparation nei, hence calculating 
  #applying the Se concentration to all the other and water adjusting
  #We are doing this bc the type of aquatic animals are more similar to 
  #sea cucumber than to the croc, frog and turtle meat, and bc are from 
  #the same region. 
  
  #Sea cucumber
  #Storing Se (x) and Water (y) values of Sea cucumber
  x <- as.numeric(fao_fish_fct$SEmcg[fao_fish_fct$fdc_id == "10372"])
  y <- as.numeric(fao_fish_fct$WATERg[fao_fish_fct$fdc_id == "10372"])
  
  #Aquatic animals nei - preparations nei
  #Storing id no. (fdc_id) of the fish entries to be re-calculated
  ids <- fao_fish_fct %>% 
    filter(ics_faostat_sua_english_description %in% c("Aquatic animals nei, cured", 
                                                      "Aquatic animals nei, preparations nei")) %>% 
    pull(fdc_id)
  
  #ids[4] <- "10373" #adding sea cucumber (salted and fermented)
  
  #Loop to assing water-adjusted Se values to the above fish entries, and
  #a comment to record the source of Se values. 
  for(id in ids){
    
    i <- which(fao_fish_fct$fdc_id == id)
    
    fao_fish_fct$SEmcg[i] <- x*(100-as.numeric(fao_fish_fct$WATERg[i]))/(100-y)
    
    fao_fish_fct$comment[i] <- ifelse(is.na(fao_fish_fct$comment[i]), 
                                      paste0("SEmcg value from water adjusted value JA15(10372)"),
                                      paste0(fao_fish_fct$comment[i],",",
                                             "SEmcg value from water adjusted value JA15(10372)"))
    
    
  }
  
  ##├├ Crustaceans, cured ----
  #Identifying Se values to be used
  
  fao_fish_fct %>% 
    filter(fish_type == "Crustaceans", 
           fish_prep == "cured",
           #source_fct %in% c("JA15", "WA19", "UF16"),
           # !is.na(SEmcg),
           #  str_detect(food_desc, "Sakura|sakura|crayfish|whiteleg")
    ) %>%
    select(fdc_id, food_desc, WATERg,  SEmcg, source_fct, nutrient_data_source) 
  
  #Crustacean, Sakura shrimp dried
  #Identifying Se values to be used
  
  fao_fish_fct %>% 
    filter(fish_type == "Crustaceans", 
           #fish_prep == "cured",
           source_fct == "JA15",
           !is.na(SEmcg),
           str_detect(food_desc, "shrimp")
    ) %>%
    select(fdc_id, food_desc, WATERg,  SEmcg, source_fct, nutrient_data_source) 
  
  #Crustacean, whiteleg shrimp, raw
  #Storing Se (x) and Water (y) values of "Crustacean, whiteleg shrimp, raw"
  x <- median(as.numeric(fao_fish_fct$SEmcg[fao_fish_fct$fdc_id == "10415"]))
  y <- median(as.numeric(fao_fish_fct$WATERg[fao_fish_fct$fdc_id == "10415"]))
  
  #Identifying where the missing value is to be changed
  i <- which(fao_fish_fct$fdc_id == "10325")
  
  #Water-adjusting Se concentration for "Crustacean, Sakura shrimp, dried"
  fao_fish_fct$SEmcg[i] <- x*(100-as.numeric(fao_fish_fct$WATERg[i]))/(100-y)
  
  #Adding comment for source of Se for "Crustacean, Sakura shrimp, dried"
  fao_fish_fct$comment[i] <- ifelse(is.na(fao_fish_fct$comment[i]), 
                                    paste0("SEmcg value from water adjusted value 
                                       JA15 (10325)"),
                                    paste0(fao_fish_fct$comment[i],",",
                                           "SEmcg value from water adjusted value 
                                       JA15 (10325)"))
  
  
  #Shrimp (crayfish), whole, dried
  #Identifying values of Se to be used
  fao_fish_fct %>% 
    filter(fish_type == "Crustaceans", 
           fish_prep == "fresh",
           source_fct == "UF16" ,
           !is.na(SEmcg),
           str_detect(food_desc, "crayfish|Crayfish" )
    ) %>%
    select(fdc_id, food_desc, WATERg,  SEmcg, source_fct, nutrient_data_source) 
  
  #Shrimp (crayfish), whole, dried
  #Storing id no. (fdc_id) of the fish entries to be averaged for use as
  #Se value
  crayfish <- fao_fish_fct %>% 
    filter(fish_type == "Crustaceans", 
           fish_prep == "fresh",
           source_fct == "UF16" ,
           !is.na(SEmcg),
           str_detect(food_desc, "crayfish|Crayfish" )) %>% pull(fdc_id)
  
  crayfish[4] <- "092004" # Adding this from the ref. list
  #Storing FCT (source_fct)
  crayfish_fct <- unique(fao_fish_fct$source_fct[fao_fish_fct$fdc_id %in% crayfish])
  
  #Storing mean Se (x) and mean Water (y) values of the list above
  x <- median(as.numeric(fao_fish_fct$SEmcg[fao_fish_fct$fdc_id %in% crayfish]))
  y <- median(as.numeric(fao_fish_fct$WATERg[fao_fish_fct$fdc_id %in% crayfish]))
  
  #Identifying where the missing value is to be changed
  i <- which(fao_fish_fct$fdc_id == "09_057")
  
  #Water-adjusting Se concentration for "Shrimp (crayfish), whole, dried"
  fao_fish_fct$SEmcg[i] <- x*(100-as.numeric(fao_fish_fct$WATERg[i]))/(100-y)
  
  #Adding comment for source of Se for "Shrimp (crayfish), whole, dried"
  fao_fish_fct$comment[i] <- ifelse(is.na(fao_fish_fct$comment[i]), 
                                    paste0("SEmcg value from water adjusted, median values ",
                                           crayfish_fct, "(", 
                                           toString(crayfish) ,")"),
                                    paste0(fao_fish_fct$comment[i],",",
                                           "SEmcg value from water adjusted, median values ",
                                           crayfish_fct, "(",
                                           toString(crayfish) ,")"))
  