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


# Exercises #2
## 1. There are some suspiciously high numbers in `tvhours`. Is the mean a good summary?
### Let's look at the distribution
ggplot(gss_cat, aes(x = tvhours)) +
  geom_histogram(binwidth = 1)
### There are a few values, but it given the (small) difference in scale and low
### low numbers, this should make the mean a pretty good summary.
### The distribution resembles more a height dist than a wealth dist

## 2. For each factor in `gss_cat` identify whether the order of the levels is arbitrary or principled
factor_columns <- gss_cat |> 
  select(where(is.factor)) |> 
  glimpse()
### There are 6 factors:
#### marital: arbitrary (?)
#### race: arbitrary
#### rincome: principled
#### partyid: hybrid (?)
#### relig: arbitrary
#### denom: arbitrary

## 3. Why did moving "Not applicable" to the front of the levels move it to the bottom of the plot?
### Because the bottom of the plot represents the "smallest" values, therefore the first ones to appear in asc order

# Exercises #3
## 1. How have th proportions of peopñe identifying as Democrat, Republican, and Independent changed over time?
### We start by collapsing the relevant groups
### And then plotting their evolution over time
gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
       "other" = c("No answer", "Don't know", "Other party"),
       "rep" = c("Strong republican", "Not str republican"),
       "ind" = c("Independent", "Ind,near rep", "Ind,near dem"),
       "dem" = c("Strong democrat", "Not str democrat")
    )
  ) |>
  count(year, partyid) |> 
  group_by(year) |> 
  mutate(prop = n / sum(n)) |> 
  ggplot(aes(x = year, y = prop, colour = partyid)) +
  geom_line()

## 2. How could you collapse 'rincome' into a small set of categories?
### Collapse non-numeric values into one category
### Use $5,000 increments
gss_cat |> 
  mutate(rincome =
    fct_collapse(rincome,
      "Unknown" = c("No answer", "Don't know","Refused", "Not applicable"),
      "Less than $5000" = c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999"),
      "$5000 - $9999" = c("$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999")
    )
  ) |> 
  ggplot(aes(y = rincome)) +
  geom_bar()

## 3. Notice there are 9 groups (excluding other) in the `fct_lump` example above.
### Why not 10?
### Because the default is "Other", and "Other" already exists as a level so all levels from the 11th level get lumped in "Other"
