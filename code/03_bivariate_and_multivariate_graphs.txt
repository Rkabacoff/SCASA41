##--------------------------------------------##
## Bivariate and multivariate graphs
## (c) 2022, Robert I. Kabacoff, Ph.D.
##--------------------------------------------##

library(ggplot2)

## Bar charts -------------------------------------

# stacked bar chart
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "stack")

# grouped bar plot
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "dodge")

# bar plot, with each bar representing 100%
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

## Scatter plot --------------------------------

data(Salaries, package="carData")

# simple scatterplot
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point()

# scatter plot with linear fit line
ggplot(Salaries,
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm")

# scatter plot with quadratic line of best fit
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

# scatter plot with loess smoothed line
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color= "steelblue") +
  geom_smooth(color = "tomato")

# scatter plot with loess smoothed line 
# and better labeling and color
ggplot(Salaries, 
       aes(x = yrs.since.phd, 
           y = salary)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha = .6) +
  geom_smooth(size = 1.5,
              color = "darkgrey") +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(50000, 250000)) +
  scale_x_continuous(breaks = seq(0, 60, 10), 
                     limits = c(0, 60)) + 
  labs(x = "Years Since PhD",
       y = "",
       title = "Experience vs. Salary",
       subtitle = "9-month salary for 2008-2009") +
  theme_minimal()

## Line plot ------------------------------------

data(gapminder, package="gapminder")

# Select US cases
library(dplyr)
plotdata <- filter(gapminder, 
                   country == "United States")

# simple line plot
ggplot(plotdata, 
       aes(x = year, 
           y = lifeExp)) +
  geom_line() 

# line plot with points
# and improved labeling
ggplot(plotdata, 
       aes(x = year, 
           y = lifeExp)) +
  geom_line(size = 1, 
            color = "lightgrey") +
  geom_point(size = 2, 
             color = "steelblue") +
  labs(y = "Life Expectancy (years)", 
       x = "Year",
       title = "Life expectancy changes over time",
       subtitle = "United States (1952-2007)",
       caption = "Source: http://www.gapminder.org/data/")

## Box plot ------------------------------------------

data(Salaries, package="carData")

# distribution of salaries by rank using boxplots
ggplot(Salaries, 
       aes(x = rank, 
           y = salary)) +
  geom_boxplot() +
  labs(title = "Salary distribution by rank")

# plot the distribution of salaries by rank using boxplots
ggplot(Salaries, aes(x = rank, 
                     y = salary)) +
  geom_boxplot(notch = TRUE, 
               fill = "cornflowerblue", 
               alpha = .7) +
  labs(title = "Salary distribution by rank")

## Violin plots ------------------------------

# distribution of salaries 
# by rank using violin plots
ggplot(Salaries, 
       aes(x = rank,
           y = salary)) +
  geom_violin() +
  labs(title = "Salary distribution by rank")

# distribution using violin and boxplots
ggplot(Salaries, 
       aes(x = rank, 
           y = salary)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .1, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) + 
  labs(title = "Salary distribution by rank")

## mean/standard error plots ----------------------------

library(dplyr)
plotdata <- Salaries %>%
  group_by(rank) %>%
  summarize(n = n(),
            mean = mean(salary),
            se = sd(salary) / sqrt(n))

# plot the means and standard errors
ggplot(plotdata, 
       aes(x = rank, 
           y = mean, 
           group = 1)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1)

## Grouped kernel density plot ------------------

# distribution of salaries by rank 
ggplot(Salaries, 
       aes(x = salary, 
           fill = rank)) +
  geom_density(alpha = 0.4) +
  labs(title = "Salary distribution by rank")

## Ridgeline plot ------------------
library(ggridges)
ggplot(Salaries, 
       aes(x = salary, 
           y = rank, 
           fill = rank)) +
  geom_density_ridges() + 
  theme_ridges() +
  theme(legend.position = "none")

# can handle many categories
ggplot(mpg, 
       aes(x = cty, 
           y = class, 
           fill = class)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs("Highway mileage by auto class") +
  theme(legend.position = "none")

## Strip plots ----------------------------------
# plot the distribution of salaries 
# by rank using strip plots
ggplot(Salaries, 
       aes(y = rank, 
           x = salary)) +
  geom_point() + 
  labs(title = "Salary distribution by rank")

# with jitter
ggplot(Salaries, 
       aes(y = rank, 
           x = salary)) +
  geom_jitter() + 
  labs(title = "Salary distribution by rank")

# add color and box plots
ggplot(Salaries, 
       aes(y = rank, 
           x = salary,
           color = rank)) +
  geom_jitter() + 
  geom_boxplot(alpha=0, color="black") +
  labs(title = "Salary distribution by rank") +
  theme(legend.position = "none")

## Beeswarm plots -----------------------------
# plot the distribution of salaries 
# by rank using beewarm-syle plots
library(ggbeeswarm)
ggplot(Salaries, 
       aes(x = rank, 
           y = salary, 
           color = rank)) +
  geom_quasirandom(alpha = 0.7,
                   size = 1.5) + 
  theme(legend.position = "none")


## Cleveland plot -------------------------------
data(gapminder, package="gapminder")

# subset Asian countries in 2007
library(dplyr)
plotdata <- gapminder %>%
  filter(continent == "Asia" & 
           year == 2007)

# basic Cleveland plot of life expectancy by country
ggplot(plotdata, 
       aes(x= lifeExp, y = country)) +
  geom_point()

# Sorted Cleveland plot
ggplot(plotdata, 
       aes(x=lifeExp, 
           y=reorder(country, lifeExp))) +
  geom_point()


## Bubble plot --------------------------------
data(mtcars)
ggplot(mtcars, aes(x = wt, y = mpg, size = hp)) +
  geom_point()

# create a bubble plot with modifications
ggplot(mtcars, aes(x = wt, y = mpg, size = hp)) +
  geom_point(alpha = .5, 
             fill="cornflowerblue", 
             color="black", 
             shape=21) +
  scale_size_continuous(range = c(1, 10))


## Correlation plot -------------------------------
data(SaratogaHouses, package="mosaicData")

# select numeric variables
df <- dplyr::select_if(SaratogaHouses, is.numeric)

# calulate the correlations
r <- cor(df, use="complete.obs")
library(ggcorrplot)
ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

## Mosaic plot --------------------------------
library(vcd)
mosaic(~Survived + Class, data=Titanic, shade=TRUE)
mosaic(~Survived + Class + Sex, data=Titanic, shade=TRUE)

## 3D Scatter plot ------------------------------
#static
library(scatterplot3d)
with(mtcars, scatterplot3d(x=hp, y=mpg, z=wt))
with(mtcars, scatterplot3d(x=hp, y=mpg, z=wt,
                           type="h", pch=16, highlight.3d = TRUE))
#interactive
library(car)
with(mtcars, scatter3d(x=hp, y=mpg, z=wt))
with(mtcars, scatter3d(x=hp, y=mpg, z=wt, 
                       groups=factor(am), surface=FALSE))

