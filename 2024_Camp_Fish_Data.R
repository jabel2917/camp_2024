library(dplyr)
library(ggplot2)
glimpse(PRELIMINARY_Camp_Fish_Data)

library(readr)
lmb_lw_2024 <- read_csv("lmb_lw_2024.csv")
View(lmb_lw_2024)

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
library(dplyr)
library(ggplot2)

lmb_lw_2024 <- read_csv("lmb_lw_2024.csv")
View(lmb_lw_2024)

# Changing the data set so that it says "simple" and "complex", not "reference" and "treatment"
lmb_lw_2024 <- lmb_lw_2024 %>%
  mutate(basin = recode(basin,
                        "treatment" = "Complex",
                        "reference" = "Simple"))
View(lmb_lw_2024)

# Create the boxplot with facets in the specified order
lmb_bp_size <- ggplot(lmb_lw_2024, aes(x = basin, y = lw_ration, color = basin)) +
  geom_boxplot() + 
  scale_color_manual(values = c("Complex" = "#005239", "Simple" = "#7570b3")) + # Ensure jitter points match boxplot colors
  geom_jitter(width = 0.1, alpha = 0.3, shape=15) + 
  labs(x = "Basin",
       y = "Length-Weight Ratio") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
lmb_bp_size
