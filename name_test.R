


#├  FAO data - ISSCAAP codes and Scientific names ----

#reading excel
readxl::excel_sheets(here::here("data",
                                "ASFIS-production_FAO-NFISS.xlsx"))
#loading the data
isscaap <- readxl::read_excel(here::here("data",
                                         "ASFIS-production_FAO-NFISS.xlsx"),
                              sheet = 1) %>% 
  rename(alpha_code = "3A_CODE") #rename bc R doesn't like variable names 
#starting w/ a number
names(isscaap)

names(isscaap)[4] <- "scientific_name"

#1795

name_test <- fao_fish_fct %>% filter(!is.na(scientific_name)) %>%  
  select(fdc_id,food_desc, scientific_name, source_fct) %>% 
  left_join(., isscaap %>% select(1:5)) %>%
  relocate(any_of(c("scientific_name", "Ref", "ISSCAAP")),
           .after = food_desc) 

name_test %>% 
  filter(!is.na(ISSCAAP)) %>%  count()

name_test %>% filter(!is.na(scientific_name), is.na(ISSCAAP)) %>% 
  distinct(scientific_name) 
