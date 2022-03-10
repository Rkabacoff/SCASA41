# -----------------------------------------------------
# IT Help Desk
# source: 
# https://www.ibm.com/communities/analytics/watson-analytics-blog/it-help-desk/
# -----------------------------------------------------

library(tidyverse)
library(scales)
df <- read_csv("ITHelpDesk.csv")
glimpse(df)

cnames <- c("RequestorSeniority", "FiledAgainst", "TicketType", "Severity",
            "Priority", "Satisfaction")


sapply(df[cnames], table, useNA = "always")

df <- mutate(df,
             RequestorSeniority = factor(RequestorSeniority, 
                  levels = c(1, 2, 3, 4),
                  labels = c("Junior", "Regular", "Senior", "Management"),
                  ordered = TRUE),
             FiledAgainst = factor(FiledAgainst),
             ITOwner = factor(ITOwner),
             TicketType = factor(TicketType,
                  levels = c(1, 2),
                  labels = c("Issue", "Request")),
             Severity = factor(Severity, 
                  levels = c(1, 2, 3, 4),
                  labels = c("Minor", "Normal", "Major", "Critical"),
                  ordered = TRUE),
             Priority = factor(Priority, 
                  levels = c(1, 2, 3),
                  labels = c("Low", "Medium", "High"),
                  ordered = TRUE),
             Satisfaction = factor(Satisfaction, 
                  levels = c(1, 2, 3),
                  labels = c("Unsatisfied", "Satisfied", "Highly Satisfied"),
                  ordered = TRUE))


# base R
summary(df)

# Hmisc
Hmisc::describe(df)

# psych
psych::describe(df)
save(df, file="ITtickets.Rdata")

# skimr
skimr::skim(df)
summary(df)

# summarytools
summarytools::dfSummary(df)
        
# are ticket numbers unique?
length(unique(df$ticket)) == nrow(df)

# how many requesters are there
length(unique(df$requestor))

# who are the most requesters
df %>% 
  group_by(requestor) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n=12)

# use by seniority
df %>%
  group_by(RequestorSeniority) %>%
  summarise(count = n()) %>%
  mutate(freq = count /sum(count))

# alternative
# table(df$RequestorSeniority)
# prop.table(table(df$RequestorSeniority))

library(scales)
plotdata <- df %>%
  group_by(RequestorSeniority) %>%
  summarise(count = n()) %>%
  mutate(pct = count /sum(count))

ggplot(data=plotdata, aes(x = RequestorSeniority, y=freq)) +
  geom_bar(fill="maroon", stat="identity") + 
  geom_text(aes(label=paste("N =", comma(count)), vjust=1), color="white") +
  scale_y_continuous(labels=percent) +
  labs(x= "Requestor Seniority", y = "Percent", title="IT Tickets by Seniority")

ggplot(data=df, aes(x = RequestorSeniority,
                    y = ..count../sum(..count..))) +
  geom_bar() +
  scale_y_continuous(labels = percent) +
  labs(y = "Percent", x = "Requestor Seniority")

# how many IT helpers are there and how is the work divided?
length(unique(df$ITOwner))
df %>%
  group_by(ITOwner) %>%
  summarise(count = n()) %>%
  arrange(count) -> ITStaff

summary(ITStaff$count)

ggplot(data=ITStaff, aes(x=count, y=as.factor(ITOwner))) +
  geom_point()

ggplot(data=ITStaff, aes(y=count, x=1)) +
  geom_boxplot()

# what are the types of problems?
df %>%
  group_by(FiledAgainst) %>%
  summarise(count = n()) %>%
  mutate(freq = count /sum(count))

ggplot(data=df, aes(x = FiledAgainst)) +
  geom_bar()

# what are the request types?
df %>%
  group_by(TicketType) %>%
  summarise(count = n()) %>%
  mutate(freq = count /sum(count))

ggplot(data=df, aes(x = TicketType)) +
  geom_bar()

# what priorities are assigned by IT staff?
df %>%
  group_by(Priority) %>%
  summarise(count = n()) %>%
  mutate(freq = count /sum(count))

ggplot(data=df, aes(x = Priority)) +
  geom_bar()

# how long does it take to close a ticket
summary(df$daysOpen)
ggplot(data=df, aes(x = daysOpen)) +
  geom_histogram()

ggplot(data=df, aes(x = daysOpen)) +
  geom_density(fill="lightblue")

df %>%
  group_by(daysOpen) %>%
  summarise(count = n()) %>%
  mutate(cumpct = cumsum(count)/sum(count)) -> days

print(days, n=Inf)

ggplot(days, aes(x=daysOpen, y=cumpct)) +
  geom_point() + 
  geom_line()

# how satisfied are users?
df %>%
  group_by(Satisfaction) %>%
  summarise(count = n()) %>%
  mutate(freq = count /sum(count))

ggplot(data=df, aes(x = Satisfaction)) +
  geom_bar()

