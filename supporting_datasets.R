

## Producing supporting datasets

# Loading the data

#Original 122 columns
fao_fish <- read.csv(here::here("data", "FISHERIES-GlobalNCT_ForSharing_Feb2022.csv"))

# Checking the dataset
dim(fao_fish)
names(fao_fish)

names(fao_fish[, c(1:122)])
fao_fish[c(1:2), c(1:122)]
fao_fish[2, c(13:30)]


which(is.na(fao_fish[1, c(1:122)]))
which(fao_fish[1, c(1:122)] == "")

fao_fish[1, c(1:30)]

#Renaming variables

n <- which(fao_fish[1, c(1:122)] == "")

which(fao_fish[2, c(1:122)] == "Quality rating for food match")

fao_fish[1, n]
fao_fish[1, c(1:122)[!c(1:122) %in% n]]

#Renaming ICS codes variables
names(fao_fish)[1:6] <- fao_fish[2, c(1:6)]
#Renaming nutrients
names(fao_fish)[(7:122)[!c(7:122) %in% n]] <- fao_fish[1, c(7:122)[!c(7:122) %in% n]]
names(fao_fish)

# Getting info on variable: "Edible coefficient to be used" to be added to our dataset

which(fao_fish[2, c(1:122)] == "Edible coefficient to be used")

n <- which(fao_fish[, 27] != "")

#There is one duplicated - 1509
dim(fao_fish[n, c(1,27)])
dim(unique(fao_fish[n, c(1,27)]))

edible_ics <- unique(fao_fish[n, c(1,27)])

edible_ics[,1] <- paste("SUMMARY ROW -", edible_ics[,1])

# Save Edible coefficient to be used" to a R file - for formatting.
saveRDS(edible_ics[c(2:96),], 
        file = "data/edible_coefficient.rds")


# Getting the info for each ICS FAOSTAT category info (n=95)
#there seems to be duplicates (n=104)
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])

#Checking duplicated
duplicated(unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[1])
#Sequence of no. seems like a typo from excel
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[c(32:40),]
#Wrong code for one item
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[c(85:87),]

# Correcting column 5 () - 1518 == 15180 (were coded as seq. 15180:15190)
fao_fish[,5][fao_fish[,1] == "1518"] <- 15180
# Correcting column 5 () - 1557 == 15570 (was coded as 15530) 
fao_fish[,5][fao_fish[,1] == "1557"] <- 15570

# Checking duplicates w/i each category
#fao_fish %>% group_by(`ICS FAOSTAT SUA Current Code`) %>% 
#  count(Source, fdc_id) %>% 
#  filter(n>1) %>% 
#  arrange(desc(n))

#Save this one to complete the FAO table. 
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])
n <- nrow(unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)]))
unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[c(3:n),]


# Save ICS info  to a R file - for formatting.
saveRDS(unique(fao_fish[!is.na(fao_fish$`Food description`),c(1:6)])[c(3:n),], 
        file = "data/fao-ics-desc.rds")



