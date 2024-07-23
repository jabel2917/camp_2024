#Hello! 

library(tidyverse)
library(readr)

bmi_2024 <- read_csv("bmi_2024.csv")
View(bmi_2024)

####BMI 2024####
#Lets quick play around with BMI 

#Cleaning, omit NAs 
bmi_2024 <- na.omit(bmi_2024)

#Sum families by basin 
bmi_sum <- bmi_2024 %>%
  group_by(basin, family) %>%
  summarise(total_num = sum(number)) %>%
  ungroup()

#change that to a percentage and make a new data frame 
bmi_sum_percentage <- bmi_sum %>%
  group_by(basin) %>%
  mutate(percentage = total_num / sum(total_num) * 100) %>%
  ungroup()

#Plot families by basin by number 
bmi_sbp <- ggplot(bmi_sum, aes(x = basin, y = total_num, fill = family)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Each Family in Each Basin",
       x = "Basin",
       y = "Percentage") +
  theme_minimal()
bmi_sbp

#Plot families by basin by percentage 
bmi_sbp_percentage <- ggplot(bmi_sum_percentage, aes(x = basin, y = percentage, fill = family)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Each Family in Each Basin",
       x = "Basin",
       y = "Percentage") +
  theme_minimal()
bmi_sbp_percentage




