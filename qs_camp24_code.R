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
  labs(title = "Number of Families in Each Basin",
       x = "Basin",
       y = "Percentage") +
  theme_minimal()
bmi_sbp

#Plot families by basin by percentage 
bmi_sbp_percentage <- ggplot(bmi_sum_percentage, aes(x = basin, y = percentage, fill = family)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Families in Each Basin",
       x = "Basin",
       y = "Percentage") +
  theme_minimal()
bmi_sbp_percentage

####Preyfish 2024####

#Note - removed perch and minnowtraps from these data to avoid 0 inflation 
preyfish_2024 <- read_csv("preyfish_2024.csv")
View(preyfish_2024)

#Cleaning, omit NAs 
preyfish_2024 <- na.omit(preyfish_2024)

preyfish_bp <- ggplot(preyfish_2024, aes(x = basin, y = CPUE)) +
  geom_boxplot() +                       # Add boxplot
  geom_jitter(width = 0.2, alpha = 0.6) + # Add individual points with jitter for better visibility
  labs(title = "CPUE per Basin - All fish",
       x = "Basin",
       y = "CPUE") +
  theme_minimal()
preyfish_bp

#Omit LMB for giggles 
blg_preyfish <- preyfish_2024 %>%
  filter(species != 'LMB')

#plot CPUE without LMB 
blg_bp <- ggplot(blg_preyfish, aes(x = basin, y = CPUE)) +
  geom_boxplot() +                       
  geom_jitter(width = 0.1, alpha = 0.6, shape=15) + 
  labs(title = "CPUE per Basin - BLG",
       x = "Basin",
       y = "CPUE") +
  theme_minimal()
blg_bp

