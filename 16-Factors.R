# library(tidyverse)
# Library forcats is part of the core tidyverse

gss_cat |> 
  count(race)

# Exercises #1
## 1. Explore the distribution of rincome (reported income).
### What makes the default bar chart hard to understand? How could you improve the plot?
ggplot(gss_cat, aes(x = rincome)) +
  geom_bar()
## A: the default bar chart is hard to understand because:
### (1): The x value labels overlap with each other
### (2): It includes non-numerical values such as: Non Applicable, I don't know...
### We could improve the plot by plotting rincome in the y axis and ommitting the non-numerical values
gss_cat |> 
  filter(str_detect(rincome,"\\$")) |> 
  ggplot(aes(y = rincome)) +
  geom_bar()


## 2. What is the most common relig in this survey? What’s the most common partyid?
gss_cat |> 
  count(relig)
gss_cat |> 
  count(partyid)
## A: The most common relig is Protestant, and the partyid is Independent


## 3. Which relig does denom (denomination) apply to? How can you find out with a table?
### How can you find out with a visualization?
gss_cat |> 
  count(relig, denom) |> 
  print(n = 47)

ggplot(gss_cat, aes(x = relig, y = denom)) +
  geom_count()
## A: There are definitely cleaner ways of doing this, but pretty obvious they denom applies to Protestant

by_age <- gss_cat |>
  filter(!is.na(age)) |>
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )
ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")

