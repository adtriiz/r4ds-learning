library(tidyverse)
library(nycflights13)

## Understanding how to full_seq works in practice
stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2022, 2022, 2022),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks |> 
  complete(year = full_seq(year,1), qtr)

## Understanding manual method with full_join() in practice
manual <- tibble(
  year = c(2020, 2020, 2020, 2020, 2021, 2021, 2021, 2021, 2022, 2022, 2022, 2022),
  qtr  = c(   1,    2,    3,    4,    1,    2,    3,    4,    1,    2,    3,    4)
)
manual |> 
  full_join(stocks)
### So full_join() takes the first arguments (x)' rows first, and then returns any unmachted rows from the second (y)

# Exercise #1
## 1. Can you find any relationship between the carrier and the rows that appear to be missing from planes?
### Let's check to which carriers do the missing tailnums belong to
missing_tailnum <- flights |> 
  distinct(tailnum) |> 
  anti_join(planes)

flights |> 
  distinct(carrier, tailnum) |>
  mutate(missing = tailnum %in% missing_tailnum$tailnum) |> 
  group_by(carrier) |> 
  summarise(
    total_planes = n(),
    missing_planes = sum(missing),
    prop_missing = missing_planes / total_planes
  ) |> 
  arrange(desc(prop_missing))
### A: Most of the planes missing from the planes dataset belong to either MQ or AA;
### In fact, most of their planes (98% and 71% respectively) are missing from the planes dataset for some reason
