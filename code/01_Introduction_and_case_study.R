##--------------------------------------------##
## ggplot2 introduction
## (c) 2022, Robert I. Kabacoff, Ph.D.
##--------------------------------------------##

## what is the relationship between experience and wages?

# load data
data(CPS85 , package = "mosaicData")
View(CPS85)

# specify dataset and mapping
library(ggplot2)
ggplot(CPS85, aes(x = exper, y = wage))

# add points
ggplot(CPS85, aes(x = exper, y = wage)) +
  geom_point()

# delete outlier
library(dplyr)
plotdata <- filter(CPS85, wage < 40)

# redraw scatterplot
ggplot(plotdata, aes(x = exper, y = wage)) +
  geom_point()

# make points blue, larger, and semi-transparent
ggplot(data = plotdata, aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue", alpha = .7, size = 2)

# add a line of best fit.
ggplot(plotdata, aes(x = exper, y = wage)) +
  geom_point(color = "cornflowerblue", alpha = .7, size = 2) +
  geom_smooth(method = "lm")

# indicate sex using color
ggplot(plotdata, aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1)

# modify the x and y axes and specify the colors to be used
ggplot(plotdata, aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue"))

# reproduce plot for each level of job sector
ggplot(plotdata, aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7, size = 1) +
  geom_smooth(method = "lm", size = 1, se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector)

# add informative labels
ggplot(plotdata, aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7, size = 1) +
  geom_smooth(method = "lm", size = 1, se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender")

# use a minimalist theme
ggplot(plotdata, aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7, size = 1) +
  geom_smooth(method = "lm", size = 1, se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender") +
  theme_minimal()

# remove construction and other
plotdata <-
  plotdata %>%
  filter(sector != "const", sector != "other")
ggplot(plotdata, aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = .7, size = 2) +
  geom_smooth(method = "lm", size = 1, se = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(breaks = seq(0, 30, 5), label = scales::dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) +
  facet_wrap(~sector) +
  labs(title = "Relationship between wages and experience",
       subtitle = "Current Population Survey",
       caption = "source: http://mosaic-web.org/",
       x = " Years of Experience",
       y = "Hourly Wage",
       color = "Gender") +
  theme_bw()
