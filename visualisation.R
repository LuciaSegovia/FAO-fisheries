

# 0) Loading the data ----
#NOTE: For the images to be saved create a folder called "images"

# NCT harmnonisation ----

# If data is not loaded 
#source("merging.R")
source("merging_all.R")
# Loading libraries
library(gt) # pretty tables
library(gtExtras) # more pretty tables
library(dplyr) # data wrangling
library(tidyr) # data cleaning 
library(ggplot2) # Visualisation
library(ggridges) #ridges for ggplot
library(cowplot) # combining plots
library(ggpubr) 
library(patchwork) # combining plots



#Total count of the no. of food entries (not only fish)
# Harmonised per FCT

fct_cover %>% ggplot(aes(source_fct)) + 
  geom_bar() + 
  theme_light() +
  coord_flip() 

## Figure 3 - Supplementary (updated)----

# Checking fish included vs total (other food/fish) in FCTS

fish_fct %>% distinct(source_fct, fdc_id) %>%            # removing duplicated items (fish)
  group_by(source_fct) %>% count() %>% rename(Nfish = "n") %>%  # count of included foods (fish) by FCT & renaming variable
  left_join(., fct_cover %>% group_by(source_fct) %>% count()) %>%  #adding total food items in each FCT
  rename(Total = "n") %>%             #renaming variable
  mutate(Nothers = Total-Nfish) %>%   # calculating other foods (excluded items)
  pivot_longer(cols = c(Nfish, Nothers),  # combining counts variables (two variables into one (columns to rows))
               names_to = "Foods", 
               values_to = "counts") %>% 
  mutate(perc = (counts/Total*100)) %>%  # calculating perc. 
  select(!Total) %>%                    # excluding unnecessary variable
  arrange(source_fct, Foods) %>%  
  mutate(lab_ypos = cumsum(perc) - 0.5 * perc) %>% # generating the position of the labels
  ggplot(aes(x = source_fct, y = perc)) +            # visualisation of variables
  geom_col(aes(fill = Foods), position = position_stack(reverse = TRUE), width = 0.8) + # Changing colour fill, transparency and bin width
  geom_text(aes(y = lab_ypos, label = counts, group =Foods),
            color = "white", fontface = "bold", size = 4) +
  # scale_fill_brewer(palette = "Dark2") +
  scale_fill_manual(values = c( "#3D5A80", "#9B1D20")) +
  labs(y = "", x = "") +
  coord_flip() +
  theme_light() +
  # Reverse the order of a discrete-valued axis
  scale_x_discrete(limits=rev(levels(as.factor(fish_fct$source_fct)))) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(15, 10, 10, 15),
    axis.text.y = element_text(face = "bold"),
   panel.grid = element_blank()
)

## Suppl.Figure 3 (old version) ----

# Checking fish included vs total (other food/fish) in FCTS

stand_fct %>% distinct(source_fct, fdc_id) %>%            # removing duplicated items (fish)
  group_by(source_fct) %>% count() %>% rename(Nfish = "n") %>%  # count of included foods (fish) by FCT & renaming variable
  left_join(., fct_cover %>% group_by(source_fct) %>% count()) %>%  #adding total food items in each FCT
  rename(Total = "n") %>%             #renaming variable
  mutate(Nothers = Total-Nfish) %>%   # calculating other foods (excluded items)
  pivot_longer(cols = c(Nfish, Nothers),  # combining counts variables (two variables into one (columns to rows))
               names_to = "Foods", 
               values_to = "counts") %>% 
  mutate(perc = (counts/Total*100)) %>%  # calculating perc. 
  select(!Total) %>%                    # excluding unnecessary variable
  arrange(source_fct, desc(Foods)) %>%  
  mutate(lab_ypos = cumsum(perc) - 0.5 * perc) %>% # generating the position of the labels
  ggplot(aes(x = source_fct, y = perc)) +            # visualisation of variables
  geom_col(aes(fill = Foods), alpha =.7, width = 0.8) + # Changing colour fill, transparency and bin width
  geom_text(aes(y = lab_ypos, label = counts, group =Foods),
            color = "black", size = 4) +
  coord_flip() +
  theme_light()

# NCT compilation ----

#If data is not loaded 
source("variable_re-calculation.R")
library(gt)
#library(gtExtras)
colnames(fao_fish_fct)

fao_fish_fct %>% distinct(fdc_id, source_fct) %>% count()

# Basic piechart
fao_fish_fct %>% count(source_fct) %>% 
  ggplot( aes(x="", y=n, fill=source_fct)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#├ Figure 3: Plot (lollipop-flip) ----  
#Percentage of the fish items from each FCT

fao_fish_fct %>% count(source_fct) %>% 
  mutate(perc = n/sum(n)*100,
         fct_label = paste0(source_fct, " (",
                            n, ")")) %>% 
  arrange(perc) %>%  # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(fct_label=factor(fct_label, levels=fct_label)) %>%   # This trick update the factor levels
ggplot( aes(x=fct_label, y=perc)) +
  geom_segment( aes(xend=fct_label, yend=0), 
                #color="skyblue"
                ) +
  geom_point(aes(color=fct_label), size=4, # alpha=0.6
    ) +
  scale_colour_manual(values = c( rep("#9B1D20",2), rep("#3D5A80",7), rep("#009292", 3) )) +
# scale_color_brewer(palette = "Dark2") +
  theme_light() +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
 #   panel.grid = element_blank(),
    axis.text.y = element_text(face = "bold", size =12),
    axis.text.x = element_text(size =12),
    plot.margin = margin(15, 10, 10, 15)) +
  xlab("") +
  ylab("%") 

#├ List of components that we are interested in: ----
 
components <- c( "WATERg",
                 "F22D6N3g",
  "F20D5N3g",
  "VITB6Amg",
  "VITB6Cmg",
  "VITB6_mg",
  "VITB6_mg_standardised",
  "NIAmg_std",
  "NIAEQmg",
  "NIAmg",
  "NIATRPmg",
  "TRPmg",
  "VITB12mcg",
 # "VITDEQmcg",
 # "VITDmcg",
 # "CHOCALmcg",
 # "ERGCALmcg",
 # "CHOCALOHmcg",
 # "ERGCALOHmcg",
   "CUmg",
   "SEmcg",
 # "IDmcg",
   "VITA_RAEmcg", 
  "VITAmcg",
 "CARTBmcg",
 "CARTBEQmcg",
 "RETOLmcg")

components_longname <- c( "Water",
                 "Docosahexaenoic acid (DHA)",
                 "Eicosapentaenoic acid (EPA)",
                 "Vitamin B6 determined by analysis",
                 "Vitamin B6 determined by calculation",
                 "Vitamin B6 by unknown method",
                # "VITB6_mg_standardised",
                # "NIAmg_std",
                 "Niacin equivalents",
                 "Niacin, prefrormed",
                 "Niacin equivalents, from tryptophan",
                 "Tryptophan",
                 "Vitamin B12",
                 "Vitamin D calculated (eq)",
                 "Vitamin D calculated",
                 "Cholecalciferol (D3)",
                 "Ergocalciferol (D2)",
                 "25-hydroxycholecalciferol",
                 "25-hydroxyergocalciferol",
                 "Copper",
                 "Selenium",
                 "Iodine" , 
                "source_fct")

#2) Plots of overall component counts and missing values ----

#Plot: overall % of missing values

fao_fish_fct %>% select(components) %>% vis_miss(sort_miss = T) 

#├ Plot (heat map): % of missing values per FCT ----

#fao_fish_fct 
fct_cover %>% #select(components, source_fct) %>% 
# select(aa, source_fct) %>% 
    select(x[120:130], source_fct) %>% 
#  rename_all(., ~components_longname) %>%  
  naniar::gg_miss_fct(., fct = source_fct) +
 # geom_rect(aes(xmin = 0.5, ymin = -Inf, xmax = 1.5, ymax =Inf), #AU19
  #          linetype = "dotted",alpha = 0, colour = "red", size = 2.5) +
  # geom_rect(aes(xmin = 0.5, ymin = 3.5, xmax = Inf, ymax =5.5), #DHA & EPA
    #        linetype = "dotted",alpha = 0, colour = "red", size = 2.5) +
    labs( x= "", y= "",
        title = "Data Gaps: Percentage (%) of missing values of selected components in each FCT")

#Perfect for ppt (width = 12, height = 7)
# ggsave(here::here("images", "missing-values-fct.png"), width = 12, height = 7)

#├ Plot (lollipop-flip) ----
#Total count of the no. of reported values per component

#List of components for generating the plot

fao_fish_fct %>% select(1:10, components) %>% 
  mutate_at(components, as.numeric) %>% 
  pivot_longer(cols = all_of(components),
               names_to = "components", 
               values_to = "n") %>% 
  filter(!is.na(n)) %>% 
  group_by(components) %>% 
  summarise(total = n()) %>%
  ungroup() %>% 
  ggplot( aes(x=components, y=total)) +
  geom_segment( aes(x=components, xend=components, y=0, yend=total), 
                color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  xlab("") +
  ylab("Total count")

#Perfect for ppt (width = 12, height = 7)
#ggsave(here::here("images", "total-count_lolli-flip.png"), width = 10, height = 8)


#├ Plot (bar chart): Coverage ----
#Coverage: Percentage of reported values per component

fao_fish_fct %>% select(1:10, components) %>% 
  mutate_at(components, as.numeric) %>% 
  pivot_longer(cols = all_of(components),
               names_to = "NV", 
               values_to = "n") %>% 
  filter(!is.na(n)) %>% 
  group_by(NV) %>% 
  summarise(total = n()) %>%
  ungroup() %>% 
  mutate(perc = total/nrow(fao_fish_fct), 
         cat = ifelse(perc >.75, 
                      "high", 
                      ifelse(perc < 0.45, 
                             "poor", "medium" ))) %>% 
  ggplot(aes(x=reorder(NV, perc), y = perc*100, fill = as.factor(cat))) +
  geom_bar(stat = "identity") +
  theme_light() +
  coord_flip() +
  scale_fill_manual("Coverage",
  values=c("darkolivegreen3","lightgoldenrod2", "red4"), 
  labels = c("High (> 75%)", "Medium (75-45%)", "Poor (<45%)")) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  labs( x= "", y= "", title = "Availability of data: Percentage (%) of fish entries with values of selected components")
   # + 
  #guides(x = guide_axis(n.dodge = 2))

#Perfect for ppt (width = 12, height = 7)
#ggsave(here::here("images", "perc-coverage_bar-flip_reorder.png"), width = 12, height = 7)


#3) Plots by Fish type (new variable) ----- 


#├ Multiple plot (heat map): Missing values by fish prep ----

#List of fish type for generating the plots
#fish <- str_split_fixed(fao_fish_fct$ics_faostat_sua_english_description, ", " , n=2) %>% .[,1] %>% unique()
fish <- fao_fish_fct %>% distinct(fish_type) %>% pull()

#List of components for generating the plot

nv <- c("WATERg",
        "F22D6N3g",
        "F20D5N3g",
        "VITB6Amg",
        "VITB6Cmg",
        "VITB6_mg",
        "NIAmg_std", 
        "NIAEQmg",
        "NIAmg",
        "NIATRPmg",
        "TRPmg",
        "VITB12mcg",
        "VITDEQmcg",
        "VITDmcg",
        "CHOCALmcg",
        "ERGCALmcg",
        "CHOCALOHmcg",
        "ERGCALOHmcg",
        "CUmg",
        "SEmcg",
        "IDmcg")

#A empty list to store the plots
plot <- list()

#A loop to generate mutiple plots (one per fish type)

for(i in 1:length(fish)){
  
plot[[i]] <- fao_fish_fct %>% 
  filter(fish_type == fish[i]) %>% 
  select(all_of(nv), fish_prep) %>% 
  naniar::gg_miss_fct(., fct = fish_prep) +
  labs( x= "", y= "", title = paste0(i, ") ",  fish[i]))

print(plot[[i]])
ggsave(paste0("images/", gsub(" ", "_", fish[i]), ".png"), width = 5, height = 5)

}

#gridExtra::grid.arrange(plot[[1]], plot[[2]], nrow = 1)

#4) Plots by Fish preparation (new variable) ----- 

#├ Plot (lollipop flip): Count of compo per fish type faceted by prep. ----

#List of fish type for generating the plots
fish <- fao_fish_fct %>% distinct(fish_type) %>% pull()

#A empty list to store the plots
plot <- list()

#variables
col_names <- c("fdc_id",
               "food_desc",
               "food_group",
               "scientific_name",
               "source_fct",
               "nutrient_data_source",
               "Edible_factor_in_FCT",
               "ICS.FAOSTAT.SUA.Current.Code",
               "ics_faostat_sua_english_description",
               "fish_type",
               "fish_prep",
               "WATERg",
               "F22D6N3g",
               "F20D5N3g",
               "VITB6Amg",
               "VITB6Cmg",
               "VITB6_mg",
               "VITB6_mg_standardised",
               "NIAmg_std", 
               "NIAEQmg",
               "NIAmg",
               "NIATRPmg",
               "TRPmg",
               "VITB12mcg",
               "VITDEQmcg",
               "VITDmcg",
               "CHOCALmcg",
               "ERGCALmcg",
               "CHOCALOHmcg",
               "ERGCALOHmcg",
               "CUmg",
               "SEmcg",
               "IDmcg")

#A loop to generate mutiple plots (one per fish type)

for(i in 1:length(fish)){

  plot[[i]] <- fao_fish_fct %>% select(col_names) %>% 
  mutate_at(nv, as.numeric) %>% 
  pivot_longer(cols = all_of(nv),
               names_to = "components", 
               values_to = "n") %>% 
  filter(!is.na(n)) %>% 
  group_by(components, fish_type, fish_prep,  ics_faostat_sua_english_description,
           ) %>% 
  summarise(total = n()) %>%
  ungroup() %>% 
  filter(fish_type == fish[i]) %>% 
  ggplot( aes(x=components, y=total)) +
  geom_segment( aes(x=components, xend=components, y=0, yend=total), 
                color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  facet_grid(cols = vars(fish_prep)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  labs(x="", y = "", title = paste("No. of ", tolower(fish[i]), "in each category reporting selected componets"))

 print(plot[[i]])
#saving plot(s)
ggsave(paste0("images/count_", gsub(" ", "_", tolower(fish[i])),
              ".png"), width = 12, height = 7)
}


# 5) Table: Identifying missing values for each SUA fish category.


##├ Table: Data available (count) per fish category and nutrient ----

## ---- visual-tab4

final_nv <-  c("WATERg"   ,   "F22D6N3g",     "F20D5N3g" , 
               "VITB6_mg_standardised", 
               "NIAmg_std","VITB12mcg" , "CUmg", "SEmcg")


#Summary table

col_order <- fao_fish_fct %>% select(-NDB_number) %>% 
  mutate_at(final_nv, as.numeric) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise_at(final_nv,  
               funs(
                 n = sum(!is.na(.)),
                 mean = mean(., na.rm =T),
                 sd = sd(., na.rm =T))) %>% names() %>%
   sort(. , decreasing = T)

fao_fish_summary <- fao_fish_fct %>% select(-NDB_number) %>% 
  mutate_at(final_nv, as.numeric) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise_at(final_nv,  
               funs(
                 n = sum(!is.na(.)),
                 mean = mean(., na.rm =T),
                 sd = sd(., na.rm =T))) 

fao_fish_summary <- fao_fish_summary[, col_order]

## ---- end-tab-4

## GT table: Good visualisation in html, it can't render in pdf
fao_fish_summary <- fao_fish_fct %>% select(-NDB_number) %>% 
  mutate_at(final_nv, as.numeric) %>% 
  group_by(ics_faostat_sua_english_description) %>% 
  summarise_at(final_nv,  
               funs(
                    n = sum(!is.na(.))))

col_order <- sort(colnames(fao_fish_summary), decreasing = T)

fao_fish_summary <- fao_fish_summary[, col_order]

saveRDS(fao_fish_summary,
        here::here("..", "summary_fct_missingSe.rds"))

names(fao_fish_summary)

body_fct1 <- function(col, row){
  cells_body(
    columns = col,
    rows = {{row}} == 0
  )
}

body_fct2 <- function(col, row){
  cells_body(
    columns = col,
    rows = {{row}} == 1
  )
}


(tab_1 <- fao_fish_summary %>% 
  select(ics_faostat_sua_english_description, ends_with("_n")) %>% 
  gt() %>% 
  tab_spanner(
    label = "Data available (count) per fish category and nutrient",
    columns = c(2:9)
  ) %>% 
  cols_label(
    ics_faostat_sua_english_description = "Fish categories",
    WATERg_n = "Water",
    VITB6_mg_standardised_n = "Vitamin B6",
    VITB12mcg_n = "Vitamin B12", 
    SEmcg_n = "Selenium", 
    NIAmg_std_n = "Niacin", 
    F22D6N3g_n = "DHA",
    F20D5N3g_n = "EPA", 
    CUmg_n = "Copper"
  ) %>%
  
  tab_style(
    style = list(
      cell_fill(color = scales::alpha("red", 0.7)),
      cell_text(color = "white", weight = "bold")
    ),
    locations = list(
      body_fct1(2, WATERg_n),
      body_fct1(3, VITB6_mg_standardised_n),
      body_fct1(4, VITB12mcg_n),
      body_fct1(5, SEmcg_n),
      body_fct1(6, NIAmg_std_n),
      body_fct1(7, F22D6N3g_n),
      body_fct1(8, F20D5N3g_n),
      body_fct1(9, CUmg_n)
    )) %>%
  
  tab_style(
    style = list(
      cell_fill(color = scales::alpha("yellow", 0.7)),
      cell_text(color = "black", weight = "bold")
    ),
    locations = list(
      body_fct2(2, WATERg_n),
      body_fct2(3, VITB6_mg_standardised_n),
      body_fct2(4, VITB12mcg_n),
      body_fct2(5, SEmcg_n),
      body_fct2(6, NIAmg_std_n),
      body_fct2(7, F22D6N3g_n),
      body_fct2(8, F20D5N3g_n),
      body_fct2(9, CUmg_n)
    )) %>%  
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
  ))


# 6) Vis: by Fish preparation: Oils  ----- 

nv <- c("WATERg",
        "F22D6N3g",
        "F20D5N3g",
        "VITB6Amg",
        "VITB6Cmg",
        "VITB6_mg",
       # "NIAEQmg",
        "NIAmg",
      #  "NIATRPmg",
      #  "TRPmg",
        "VITB12mcg",
       # "VITDEQmcg",
      #  "VITDmcg",
      #  "CHOCALmcg",
      #  "ERGCALmcg",
      #  "CHOCALOHmcg",
      #  "ERGCALOHmcg",
      #  "IDmcg",
        "CUmg",
        "SEmcg")

#Values of the plot
fao_fish_fct %>% filter(str_detect(fish_prep, "oils")) %>% 
  select(nv, source_fct, fish_prep, fish_type) %>% 
  pivot_longer(cols = all_of(nv),
               names_to = "components", 
               values_to = "n") %>% 
  mutate_at("n", as.numeric) %>% 
  group_by(components, fish_prep, fish_type) %>% 
  summarise(total = median(n, na.rm = T)) %>%
  gt( rowname_col = "component")

              

#No. of unique items per FCT
fao_fish_fct %>% filter(str_detect(fish_prep, "oils")) %>%
  distinct(fdc_id, source_fct) %>% count(source_fct)

#Finding source of extreme values - all from DK19
fao_fish_fct %>% filter(str_detect(fish_prep, "oils")) %>%
  select(col_names) %>% 
  filter(IDmcg >300, CHOCALmcg>100, SEmcg >1) %>% gt()# %>% 
#gtsave(filename = "images/high-conc-oils.pdf")

#├ Plot (lollipop flip): Median concentration of compo in oils ----

fao_fish_fct %>% filter(str_detect(fish_prep, "oils")) %>% 
  select(all_of(nv, source_fct)) %>% 
  naniar::gg_miss_fct(., fct = source_fct)

fao_fish_fct %>% filter(str_detect(fish_prep, "oils")) %>% 
  select(nv, source_fct, fish_prep) %>% 
  pivot_longer(cols = all_of(nv),
               names_to = "components", 
               values_to = "n") %>% 
  mutate_at("n", as.numeric) %>%
  group_by(components, fish_prep) %>% 
  summarise(total = median(n, na.rm = T)) %>% ungroup() %>%
  filter(total < 300) %>%  #filtering out the outliers - I and CHOCAL from DK19
  ggplot( aes(x=components, y=total)) +
  geom_segment( aes(x=components, xend=components, y=0, yend=total), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  coord_flip() +
  facet_grid(vars(fish_prep)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Median concentration")


#ggsave(here::here("images", "median-conc-oils.png"), width = 12, height = 7)
ggsave(here::here("images", "median-conc-oils_wo-outliers.png"), width = 12,
       height = 7)


##├├ Plot: Missing values for nutients by ICS code ----
fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code <- as.factor(fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code)

fao_fish_fct[,c("CAmg", "SEmcg", "RETOLmcg", "ICS.FAOSTAT.SUA.Current.Code")] %>%  #selecting variables
  naniar::gg_miss_fct(., fct = ICS.FAOSTAT.SUA.Current.Code) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))


##├├ Plot: Missing values for nutients by ICS code ----
fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code <- as.factor(fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code)

fao_fish_fct[,c("FAT_g_standardised","F22D6N3g",
                "F20D5N3g", "ICS.FAOSTAT.SUA.Current.Code")] %>%  #selecting variables
  naniar::gg_miss_fct(., fct = ICS.FAOSTAT.SUA.Current.Code) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))


##├├ Plot: Missing values for nutients by ICS category ----
fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code <- as.factor(fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code)

fish <- unique(fao_fish_fct$fish_type)[1]

subset(fao_fish_fct, fish_type %in% fish, 
       select = c("FAT_g_standardised","F22D6N3g",
                "F20D5N3g", "ics_faostat_sua_english_description")) %>%  #selecting variables
  naniar::gg_miss_fct(., fct = ics_faostat_sua_english_description) +
  scale_fill_gradientn(colors = rainbow(5), limits = c(0, 100)) +
    coord_flip() #+
#  scale_x_discrete(guide = guide_axis(n.dodge = 3))

subset(fao_fish_fct, ics_faostat_sua_english_description == "Aquatic animals nei, fresh", 
       select = c("FAT_g_standardised","F22D6N3g",
                  "F20D5N3g", "ics_faostat_sua_english_description"))
       
# Perc. of missing values per ICS cat. 
var <- c("F22D6N3g",  "F20D5N3g")
per <- 60

fao_fish_fct %>%
 # group_by(fish_type) %>%
  naniar::miss_var_summary() %>%
  filter(variable %in% var) %>% 
  arrange(desc(pct_miss))

fao_fish_fct %>%
  group_by(ics_faostat_sua_english_description) %>%
  naniar::miss_var_summary() %>%
  filter(variable %in% var ,
         pct_miss >per) %>% 
  arrange(desc(pct_miss))# %>% pull(ics_faostat_sua_english_description)




## Table 4.

fao_fish_summary %>% relocate(ics_faostat_sua_english_description, 
                              .before = WATERg_n) %>%  arrange(desc(WATERg_n)) %>% 
  rename("Fish categories" = ics_faostat_sua_english_description,
         "Water" = WATERg_n,
         "Vitamin B6" = VITB6_mg_standardised_n,
         "Vitamin B12" = VITB12mcg_n, 
         "Selenium" = SEmcg_n, 
         "Niacin" = NIAmg_std_n, 
         "DHA" = F22D6N3g_n,
         "EPA" = F20D5N3g_n, 
         "Copper" = CUmg_n) %>% 
  
  knitr::kable()

missing <- c("Cephalopods, cured", "Aquatic animals nei, cured", 
             "Aquatic animals nei, preparations nei", "Crustaceans, cured", 
             "Cephalopods, canned")

# Count and perc. imputed values for 
# SE
# CARTBEQmcg_std imputed
# Ash
# CHOAVLDFg_std assumed zero
# Impausible value of CARTBEQmcg_std
subset(fao_fish_fct, grepl("Impausible value of CARTBEQmcg_std", comment)) %>% 
  count() /nrow(fao_fish_fct)*100
  
# Quality checks ----

harmo_fct <- readRDS(here::here("data", "FAO-fish-harmonised_v1.1.0.RDS"))

harmo_fct$ics_faostat_sua_english_description[harmo_fct$fish_prep == "frozen, fillets"]
harmo_fct$ics_faostat_sua_english_description[harmo_fct$fish_prep == "frozen fillets"]
harmo_fct$fish_prep[harmo_fct$fish_prep == "frozen, fillets"] <- "frozen fillets"
harmo_fct$fish_prep[harmo_fct$fish_prep == "frozen, whole"] <- "frozen whole"
harmo_fct$fish_prep[harmo_fct$fish_prep == "frozen fillets"]


## Proteins --------

harmo_fct %>% 
  #filter(grepl("fresh|frozen|raw", fish_prep)) %>% 
  filter(grepl("fish", ics_faostat_sua_english_description)) %>% 
  ggplot(aes(PROCNTg, fish_prep, fill = fish_prep)) + 
  geom_density_ridges(alpha = 0.8) +
 # scale_fill_manual(values = my_color) +
  theme_ridges() + 
  labs(x="", y = "")  +
  theme(legend.position = "none")

harmo_fct %>% 
  filter(grepl("fish", ics_faostat_sua_english_description)) %>% 
  mutate(fish_prep = factor(fish_prep,
        levels =c("fresh", "fresh fillets", "frozen whole", 
                  "frozen fillets","canned", "cured", "preparations nei", "body oils", "liver oils"))) %>% 
ggplot(aes(PROCNTg, after_stat(count), fill = fish_prep)) + 
  geom_density(alpha = 0.9) +
  facet_wrap(~fish_prep) +
  # labs(x="g of protein per 100g of fresh fish (EP)", y = "")  +
  labs(x="Protein \n (g/100g EP)", y = "")  +
  geom_vline(xintercept =  30, colour = "blue", linewidth = 1, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none") 

# Supplementary figures -----------

##├ Suppl. Fig X - Back-calculation of retinol ----

fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code <- as.factor(fao_fish_fct$ICS.FAOSTAT.SUA.Current.Code)

fao_fish_fct[,c("RETOLmcg", "VITA_RAEmcg", "VITAmcg", "CARTBEQmcg", "ICS.FAOSTAT.SUA.Current.Code")] %>%  #selecting variables
  naniar::gg_miss_fct(., fct = ICS.FAOSTAT.SUA.Current.Code) +
  coord_flip() +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))
