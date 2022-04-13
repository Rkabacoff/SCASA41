##--------------------------------------------##
## Univariate graphs
## (c) 2022, Robert I. Kabacoff, Ph.D.
##--------------------------------------------##

library(ggplot2)
data(Marriage, package = "mosaicData")


## Bar Charts --------------------------------

# plot the frequency distribution of race
ggplot(Marriage, aes(x = race)) + 
  geom_bar()

# same as
ggplot(Marriage, aes(x = race, y = ..count..)) + 
  geom_bar()


# modified colors
ggplot(Marriage, aes(x = race)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black")

# sort categories

# create a plot dataset
library(dplyr)
plotdata <- Marriage %>%
  group_by(race) %>%
  summarize(N = n())

ggplot(plotdata, 
       aes(x = reorder(race, -N), y=N)) + 
  geom_bar(stat = "identity", 
           fill = "cornflowerblue", 
           color="black") +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by race")


# plot percents
plotdata$prop <- plotdata$Freq / sum(plotdata$Freq)
ggplot(plotdata, aes(x = race, y = prop)) + 
  geom_bar(stat = "identity", 
           fill = "cornflowerblue", 
           color="black") +
  scale_y_continuous(label = scales::percent) +
  labs(x = "Race", 
       y = "Percent", 
       title = "Participants by race")

# same as
ggplot(Marriage, 
       aes(x = race, 
           y = ..count.. / sum(..count..))) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  scale_y_continuous(label = scales::percent) +
  labs(x = "Race", 
       y = "Percent", 
       title = "Participants by race")


# sort bars by hand
ggplot(Marriage, aes(x = race)) + 
  geom_bar() + 
  scale_x_discrete(limits = c("American Indian", "Hispanic", 
                              "Black", "White"))

## Tree maps -----------------------------------------------

library(treemapify)

# create a tree map of marriage officials
plotdata <- Marriage %>%
  count(officialTitle)

ggplot(plotdata, 
       aes(fill = officialTitle, 
           area = n)) +
  geom_treemap() + 
  labs(title = "Marriages by officiate")

# create a tree map with tile labels
ggplot(plotdata, 
       aes(fill = officialTitle, 
           area = n, 
           label = officialTitle)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Marriages by officiate") +
  theme(legend.position = "none")

## Histograms ---------------------------------

# age distribution using a histogram
ggplot(Marriage, aes(x = age)) +
  geom_histogram() + 
  labs(title = "Participants by age",
       x = "Age")

# plot the histogram with blue bars and white borders
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white") + 
  labs(title="Participants by age",
       x = "Age")

# plot the histogram with 20 bins
ggplot(Marriage, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="Participants by age", 
       subtitle = "number of bins = 20",
       x = "Age")

# plot the histogram with percentages on the y-axis
library(scales)
ggplot(Marriage, 
       aes(x = age, 
           y= ..count.. / sum(..count..))) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="Participants by age", 
       y = "Percent",
       x = "Age") +
  scale_y_continuous(labels = percent)

## Kernel Density Plots ------------------------------------------

# kernel density plot of age
ggplot(Marriage, aes(x = age)) +
  geom_density() + 
  labs(title = "Participants by age")

# add a fill color
ggplot(Marriage, aes(x = age)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by age")

# default bandwidth for the age variable
bw.nrd0(Marriage$age)

# decrease bandwidth
ggplot(Marriage, aes(x = age)) +
  geom_density(fill = "indianred", bw = 1) + 
  labs(title = "Participants by age")
