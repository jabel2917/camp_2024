library(dplyr)
library(ggplot2)

#Loading in Diet data set; change the name to the final sheet once your data collecting is complete
library(readxl)
PRELIMINARY_Camp_Diet_Data <- read_excel("PRELIMINARY Camp Diet Data.xlsx", 
                                         col_types = c("date", "text", "numeric", 
                                                       "numeric", "text", "numeric", "text", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "text"))
View(PRELIMINARY_Camp_Diet_Data)

#Creating two data sets for each basin
ref <- PRELIMINARY_Camp_Diet_Data %>%
  filter(Basin == "Ref")
View(ref)

trt <- PRELIMINARY_Camp_Diet_Data %>% 
  filter(Basin == "Trt")
View(trt)

#Two sample t-test comparing dry weights of fish between the two basins
t.test(ref$'FishDW', trt$'FishDW')

#Boxplot showing results from t-test above (DW's of fish)
ggplot(data=PRELIMINARY_Camp_Diet_Data, mapping=aes(x=Basin, y=FishDW, color=Basin)) +
  geom_boxplot()

#To make pie charts, will need to organize data similar to camp_diet_final.csv
#Instead, here's relationship between DW and WW for all fish
#First, need to add a column for difference between DW and WW for each weight of fish diet
PRELIMINARY_Camp_Diet_Data <- PRELIMINARY_Camp_Diet_Data %>% 
  mutate(WWDW_diff_fish = FishWW - FishDW, .before = Odonata)
View(PRELIMINARY_Camp_Diet_Data)

ggplot(PRELIMINARY_Camp_Diet_Data, aes(x=FishWW, y=WWDW_diff_fish)) +
  geom_point() 
  
#Filtering the data to exclude any diets that had a wet weight greater than 3 g
Small_Diet_Data <- PRELIMINARY_Camp_Diet_Data %>% 
  filter(FishWW <3)
View(Small_Diet_Data)

#Retrying that same graph (WWDW_Diff vs. WW) using the small diet subset, essentially removing outliers
ggplot(Small_Diet_Data, aes(x=FishWW, y=WWDW_diff_fish)) +
  geom_point() +
  geom_smooth(method = "lm")

lm_fit <- lm(WWDW_diff_fish~FishWW, data=Small_Diet_Data)
summary(lm_fit)

#Doing it with just WW and DW on the two axes
ggplot(Small_Diet_Data, aes(x=FishDW, y=FishWW)) +
  geom_point() +
  geom_smooth(method = "lm")

lm_fit <- lm(FishDW~FishWW, data=Small_Diet_Data)
summary(lm_fit)


####Diet 2024####
#Total weight per basin and proportions already calculated 

diets_2024_unfin <- read_csv("diets_2024_unfin.csv")
View(diets_2024_unfin)

#Stacked barplot - dry weight prop

diet_dw_sbp <-ggplot(diets_2024_unfin, aes(x = basin, y = prop_weight, fill = diet_item)) +
  geom_bar(stat = "identity") +
  labs(title = "Diet Proportions per Basin",
       x = "Basin",
       y = "Proportion of Total Weight") +
  theme_minimal()
diet_dw_sbp

#Weight pie chart 
new_labels <- c(
  "aquainv" = "Aquatic Invertebrate",
  "coleoptera" = "Coleoptera",
  "diptera" = "Diptera",
  "fish" = "Fish",
  "odonata" = "Odonata",
  "terinv" = "Terrestrial Invertebrate",
  "terver" = "Terrestrial Vertebrate",
  "trichoptera" = "Trichoptera")

facet_labels <- c(
  "reference" = "Simple",
  "treatment" = "Complex")

diet_dw_pie <- ggplot(diets_2024_unfin, aes(x = "", y = prop_weight, fill = diet_item)) +
  geom_bar(stat = "identity", width = ) +
  coord_polar("y") +
  facet_wrap(~ basin, labeller = labeller(basin = facet_labels)) +
  labs(x = NULL,
       y = NULL,
       fill = "Diet Items") +
  theme_void() + 
  theme(legend.position = "right",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        aspect.ratio = 1) +   
  scale_fill_manual(values = scales::hue_pal()(length(new_labels)), labels = new_labels)
diet_dw_pie
ggsave("diet_dw_pie.png", plot = diet_dw_pie, width = 10, height = 8, units = "in")

#Stacked barplot - number

diet_num_sbp <-ggplot(diets_2024_unfin, aes(x = basin, y = number, fill = diet_item)) +
  geom_bar(stat = "identity") +
  labs(title = "Diet Numbers per Basin",
       x = "Basin",
       y = "Raw Numbers") +
  theme_minimal()
diet_num_sbp
 