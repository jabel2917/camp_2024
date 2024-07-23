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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Attempting to make a stacked barchart

Barchart_ref <- ref %>% 
  group_by()
  mutate(
    = FishWW - FishDW, .before = Odonata)
View(PRELIMINARY_Camp_Diet_Data)
  
  
  
ggplot(PRELIMINARY_Camp_Diet_Data, aes(x = Basin, y = Value, fill = SubCategory)) +
  geom_bar() +
 