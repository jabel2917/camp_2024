library(dplyr)
library(ggplot2)
glimpse(PRELIMINARY_Camp_Fish_Data)

#Conducting two-sample t-test; can insert any type of variable after the '$' in the t.test() function
ref <- PRELIMINARY_Camp_Fish_Data %>%
  filter(Basin == "Ref")
View(ref) 

trt <- PRELIMINARY_Camp_Fish_Data %>% 
  filter(Basin == "Trt")
View(trt)

t.test(ref$`LWRatio`, trt$`LWRatio`)

#Side-by-side boxplots of Length:Weight ratios between basins
#How do I choose which color to make the boxplots?
ggplot(data=PRELIMINARY_Camp_Fish_Data, mapping=aes(x=Basin, y=LWRatio, color=Basin)) +
  geom_boxplot()
ggsave(filename = "June2024CampLWRatiosBoxplot.png")

#Individual line graphs showing length:weight ratios in each basin
ggplot(PRELIMINARY_Camp_Fish_Data, aes(x=Weight, y=Length)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Basin)

#Combining those two graphs into one
ggplot(PRELIMINARY_Camp_Fish_Data, aes(x=Weight, y=Length, color=Basin)) +
  geom_point() +
  geom_smooth() +
  labs(caption="June 2024 Largemouth Bass Length:Weight Ratios in the Basins of Camp Lake")

#density plot; red dashed line indicates LW ratio of 1
ggplot(PRELIMINARY_Camp_Fish_Data, aes(x=LWRatio, color=Basin, fill=Basin)) +
  geom_density(alpha=0.25) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black")
ggsave(filename = "June2024CampLWRatiosDensity.png")


#Figuring out diet %'s from each basin
ref <- lmb_lw_2024 %>% 
  filter(basin == "reference")
view(ref)

trt <- lmb_lw_2024 %>% 
  filter(basin == "treatment")
view(trt)

ref %>% count(diet == "Y")
trt %>% count(diet == "Y")

#Figuring out avg. length in each basin
mean(ref$length)
mean(trt$length)

#Figuring out avg. weight in each basin
mean(ref$weight)
mean(trt$weight)



####Making a final boxplot for LWratios in each basin####

dark2_colors <- brewer.pal(n = 8, name = "Dark2")

# Assign selected colors to specific basins
selected_colors <- c(dark2_colors[1], dark2_colors[3], dark2_colors[5])  # Example: pick the 1st, 3rd, and 5th colors

# Ensure the number of selected colors matches the number of unique basins
unique_basins <- unique(lmb_lw_2024$Basin)
num_colors <- length(unique_basins)

# If there are more basins than colors specified, repeat the colors
if (num_colors > length(selected_colors)) {
  selected_colors <- rep(selected_colors, length.out = num_colors)
}

# Create a named vector of colors
color_map <- setNames(selected_colors[1:num_colors], unique_basins)

# Create the boxplot with specified colors from Dark2 palette
lmb_bp <- ggplot(lmb_lw_2024, aes(x = Basin, y = lw_ration, color = Basin)) +
  geom_boxplot() +                       
  geom_jitter(width = 0.1, alpha = 0.3, shape = 15) + 
  labs(x = "Basin",
       y = "Length-Weight Ratio") +
  scale_color_manual(values = color_map) +
  theme_classic()

# Display the plot
lmb_bp

# Changing the data set so that it says "simple" and "complex", not "reference" and "treatment"
lmb_lw_2024 <- lmb_lw_2024 %>%
  mutate(Basin = recode(Basin,
                        "Treatment" = "Complex",
                        "Reference" = "Simple"))
view(lmb_lw_2024)

#Recreating the boxplot
lmb_bp <- ggplot(lmb_lw_2024, aes(x = Basin, y = lw_ration, color = Basin)) +
  geom_boxplot() +                       
  geom_jitter(width = 0.1, alpha = 0.3, shape = 15) + 
  labs(x = "Basin",
       y = "Length-Weight Ratio") +
  scale_color_manual(values = color_map) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))  
lmb_bp

#Capitalizing the elements in the "Size" column
library(stringr)
lmb_lw_2024 <- lmb_lw_2024 %>%
  mutate(size = str_to_title(size))
view(lmb_lw_2024)

#Splitting up the boxplot by size structure
lmb_lw_2024 <- lmb_lw_2024 %>%
  mutate(size = factor(size, levels = c("Small", "Large")))

# Create the boxplot with facets in the specified order
lmb_bp_size <- ggplot(lmb_lw_2024, aes(x = Basin, y = lw_ration, color = Basin)) +
  geom_boxplot() +                       
  geom_jitter(width = 0.1, alpha = 0.3, shape=15) + 
  labs(x = "Basin",
       y = "Length-Weight Ratio") +
  facet_wrap(~size) +
  scale_color_manual(values = color_map) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
lmb_bp_size

# Display the plot
lmb_bp_size

my_palette <- c("royalblue4", "hotpink4")

# Create the boxplot with specified colors from Dark2 palette
lmb_bp <- ggplot(lmb_lw_2024, aes(x = Basin, y = lw_ration, color = Basin)) +
  geom_boxplot() +                       
  geom_jitter(width = 0.1, alpha = 0.3, shape = 15) + 
  labs(x = "Basin",
       y = "Length-Weight Ratio") +
  scale_color_manual(values = my_palette) +
  theme_classic()

# Display the plot
lmb_bp


