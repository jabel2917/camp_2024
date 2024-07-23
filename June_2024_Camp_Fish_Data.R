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