library(dplyr)
library(ggplot2)
library(tidyr)
#1.a
abalone = read.csv("abalone.csv")
str(abalone)
summary(abalone)

#1.b
abalone_subset = abalone %>% select(Length, Diameter, Height)
abalone_subset

#1.c
female_abalone = abalone %>% filter(Sex=="F")
female_abalone

#1.d
male_weights = abalone %>% filter(Sex == "M") %>% select(contains("Weight"))
male_weights

#1.e
mean_length_by_gender = abalone %>% 
  group_by(Sex) %>% 
  summarize(mean_length = mean(Length, na.rm = TRUE))
mean_length_by_gender

#2.a
library(GGally)
ggpairs(abalone)


#2.b
ggplot(abalone, aes(x = Diameter, y = Length)) +
  geom_point() +
  labs(title = "Diameter vs Length", x = "Diameter", y = "Length")

#2.c
ggplot(abalone, aes(x = Shell.weight, fill = Sex)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~Sex, ncol = 1) +
  labs(title = "Histograms of Shell-weight by Sex", x = "Shell Weight", y = "Count")

#2.d
ggplot(abalone, aes(x = Sex, y = Shell.weight, fill = Sex)) +
  geom_violin() +
  labs(title = "Violin Plot of Shell Weight by Sex", x = "Sex", y = "Shell Weight")

#2.e
ggplot(abalone, aes(x = Sex, y = Shell.weight, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Box Plot of Shell Weight by Sex", x = "Sex", y = "Shell Weight")

