#Hello! 

library(tidyverse)
library(readr)
library(RColorBrewer)

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

####JC BMI 2024####
#Exclude chironomids 
bmi_plot <- bmi_complete_2024 %>%
  filter(family != "chironomidae") %>%
  group_by(family, month, basin) %>%
  summarize(number = sum(number)) %>%
  ungroup()

ggplot(bmi_plot, aes(x = family, y = number, fill = month)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ basin) +
  labs(title = "Number of Family by Month in Each Basin",
       x = "Family",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(bmi_complete_2024, aes(x = family, y = number)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(basin~month) +
  labs(title = "Number of Each Bug Family by Month in Each Basin",
       x = "BMI Family",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


odonata_data <- bmi_complete_2024 %>%
  filter(family == "odonata")
odonata_data$month <- factor(odonata_data$month, levels = c("June", "July", "August"))


ggplot(odonata_data, aes(x = family, y = number, fill=month)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~site_id) +
  labs(title = "Number of Odonata by Month at each site",
       x = "BMI Family",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

odonata_basin_totals <- odonata_data %>%
  group_by(month, basin) %>%
  summarize(Total_Count = sum(number)) %>%
  ungroup()

ggplot(odonata_basin_totals, aes(x = month, y = Total_Count, fill = month)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ basin) +
  labs(title = "Total Number of Odonata by Month in Each Basin",
       x = "Month",
       y = "Total Count") +
  theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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

####LMB Length Weight 2024####
lmb_lw_2024 <- read_csv("lmb_lw_2024.csv")
View(lmb_lw_2024)

#Omit NAs in basin, and weights >1000 and <100
lmb_lw_2024<- lmb_lw_2024 %>%
  filter(!is.na(basin) & weight != 1000 & weight >= 100)

lmb_lw_2024 <- lmb_lw_2024 %>%
  mutate(lw_ration = length / weight)

lmb_bp <- ggplot(lmb_lw_2024, aes(x = basin, y = lw_ration)) +
  geom_boxplot() +                       
  geom_jitter(width = 0.1, alpha = 0.3, shape=15) + 
  labs(title = "Length-Weight Ration by Basin",
       x = "Basin",
       y = "Length-Weight Ration") +
  theme_minimal()
lmb_bp

#Test for significance 

# Split the data by basin
ref_t <- lmb_lw_2024 %>% filter(basin == 'reference') %>% pull(lw_ration)
trt_t <- lmb_lw_2024 %>% filter(basin == 'treatment') %>% pull(lw_ration)

# Run a t-test comparing the two basins
t_test <- t.test(ref_t, trt_t)

# Print the results of the t-test
print(t_test)

#Add size column 

lmb_lw_2024 <- lmb_lw_2024 %>%
  mutate(size = ifelse(length >= 305, 'large', 'small'))

#Facet wrap by size 
lmb_bp_size <- ggplot(lmb_lw_2024, aes(x = basin, y = lw_ration)) +
  geom_boxplot() +                       
  geom_jitter(width = 0.1, alpha = 0.3, shape=15) + 
  labs(title = "Length-Weight Ration by Basin and Size ",
       x = "Basin",
       y = "Length-Weight Ration") +
  facet_wrap(~size) +
  
  theme_minimal()
lmb_bp_size

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
diet_dw_pie <- ggplot(diets_2024_unfin, aes(x = "", y = prop_weight, fill = diet_item)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~ basin) +
  labs(title = "Diet Proportions per Basin",
       x = NULL,
       y = NULL) +
  theme_void() + 
  theme(legend.position = "right")
diet_dw_pie

#Stacked barplot - number

diet_num_sbp <-ggplot(diets_2024_unfin, aes(x = basin, y = number, fill = diet_item)) +
  geom_bar(stat = "identity") +
  labs(title = "Diet Numbers per Basin",
       x = "Basin",
       y = "Raw Numbers") +
  theme_minimal()
diet_num_sbp

ggplot(diets_2024_unfin %>% 
         filter(diet_item != "fish"),  # Filter out "fish"
       aes(x = basin, y = prop_number, fill = diet_item)) +
  geom_bar(stat = "identity") +
  labs(title = "Diet Numbers per Basin",
       x = "Basin",
       y = "Raw Numbers") +
  theme_minimal()



diet_dw_pie <- ggplot(diets_2024_unfin, aes(x = "", y = prop_weight, fill = diet_item)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~ basin, scales = "free", ncol = 2) +  # Adjusting the facets
  #labs(title = "Diet Proportions per Basin",
  #     x = NULL,
  #     y = NULL) +
  theme_void() + 
  theme(
    legend.position = "bottom",  # Moving labels to the bottom
    strip.text = element_text(size = 12),  # Increasing the size of facet labels
    #plot.title = element_text(size = 20),  # Increasing the size of the title
    plot.margin = margin(1, 1, 1, 1)  # Adjusting plot margins
  )

diet_dw_pie


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

diets_2024 <- read_csv("diets_2024.csv")

diet_dw_pie <- ggplot(diets_2024, aes(x = "", y = prop_weight, fill = diet_item)) +
  geom_bar(stat = "identity", width = ) +
  coord_polar("y") +
  facet_wrap(~ basin, labeller = labeller(basin = facet_labels)) +
  labs(x = NULL,
       y = NULL,
       fill = "Diet Items") +
  theme_void() + 
  theme(
    legend.position = "bottom",  # Moving labels to the bottom
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.title = element_text(size = 16), # Increasing the size of facet labels
    #plot.title = element_text(size = 20),  # Increasing the size of the title
    plot.margin = margin(1, 1, 1, 1)  # Adjusting plot margins
  )+
  scale_fill_manual(values = scales::hue_pal()(length(new_labels)), labels = new_labels)
diet_dw_pie

diets_2024$diet_item_coarse <- ifelse(diets_2024$diet_item == "fish", "Fish", "Other")

#New pie chart - broad categories 
aggregated_data <- diets_2024 %>%
  group_by(basin, diet_item_coarse) %>%
  summarise(total_weight = sum(prop_weight)) %>%
  ungroup()


diet_dw_pie_coarse <- ggplot(aggregated_data, aes(x = "", y = total_weight, fill = diet_item_coarse)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y") +
  facet_wrap(~ basin,labeller = labeller(basin = facet_labels)) +
  labs(x = NULL,
       y = NULL,
       fill = "Diet Items") +
  theme_void() + 
  theme(
    legend.position = "bottom",  # Moving labels to the bottom
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.title = element_text(size = 16), # Increasing the size of facet labels
    #plot.title = element_text(size = 20),  # Increasing the size of the title
    plot.margin = margin(1, 1, 1, 1)  # Adjusting plot margins
  ) +
  scale_fill_manual(values = c("Fish" = "deepskyblue4", "Other" = "tan3"), labels = c("Fish", "Other"))

diet_dw_pie_coarse




