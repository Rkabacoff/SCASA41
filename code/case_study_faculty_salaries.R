##--------------------------------------------##
## Case Study: Professor Salaries
## (c) 2022 Robert I. Kabacoff, Ph.D.
##--------------------------------------------##
library(ggplot2)
library(scales)
library(plotly)
data(Salaries, package="carData")

Salaries$discipline <- factor(Salaries$discipline,
                              levels = c("A", "B"),
                              labels = c("Theoretical", "Applied"))


# plot sex vs. salary vs. discipline vs. rank
p <- ggplot(Salaries, aes(x=sex, y=salary, color=sex, label=yrs.since.phd)) +
  geom_jitter(width=.2, alpha=.5, size=2) +
  scale_color_manual(values=c("brown", "steelblue")) +
  facet_grid(rank ~ discipline, scales="free") +
    theme_bw() +
  theme(legend.position="none")
ggplotly(p)


# add medians to plot

library(dplyr)
plotdata <- Salaries %>%
  group_by(sex, rank, discipline) %>%
  summarize(salary = median(salary),
            yrs.service = median(yrs.service))

p <- ggplot(Salaries, aes(x=sex, y=salary, color=sex, label=yrs.service)) +
  geom_jitter(width=.2, alpha=.4, size=2) +
  geom_point(data=plotdata, aes(x=sex, y=salary), color="black",shape=3, size=2) +
  scale_color_manual(values=c("brown", "steelblue")) +
  scale_y_continuous(label=dollar) +
  facet_grid(rank ~ discipline, scales="free") +
  theme_bw() +
  theme(legend.position="none") +
  labs(title = "Nine Month College Salaries", y="")
ggplotly(p)
