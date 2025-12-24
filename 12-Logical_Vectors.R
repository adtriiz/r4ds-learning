library(tidyverse)
library(nycflights13)

# How are the missing values in dep_time, sched_dep_time and dep_delay connected?
flights |> 
  mutate(
    miss_dep_time = is.na(dep_time),
    miss_sched_dep_time = is.na(sched_dep_time),
    miss_dep_delay = is.na(dep_delay)
  ) |> 
  count(miss_dep_time, miss_sched_dep_time, miss_dep_delay)
# There are no cases of missing sched_dep_time, but if dep_time is missing, then dep_delay is also missing

# All flights where arr_delay is missing but dep_delay isn't
flights |> 
  filter(is.na(arr_delay) & !is.na(dep_delay)) |> 
  relocate(any_of(c("arr_delay","dep_delay")),.before = 1)

# All flights where neither arr_time nor sched_arr_time are missing, but arr_delay is
flights |> 
  filter(!is.na(arr_time) & !is.na(sched_arr_time) & is.na(arr_delay)) |> 
  relocate(any_of(contains("arr")), .before = 1)

# Flights with missing dep_time
flights |> 
  filter(is.na(dep_time))
# 8255 flights with missing dep_time - also missing dep_delay, arr_time and arr_delay. They are cancelled flights

# Distribution of cancelled flights per day
daily_summary <- flights |> 
  mutate(
    year_month_day = as.Date(paste(year, month, day, sep = "-"),"%Y-%m-%d"),
    cancelled = is.na(dep_time)
    ) |>
  group_by(year_month_day) |> 
  summarise(
    n_flights = n(),
    n_cancelled = sum(cancelled),
    prop_cancelled = mean(cancelled),
    avg_delay_non_cancelled = mean(dep_delay[!cancelled], na.rm = TRUE)
  )

# Smooth distribution of proportion of cancelled flights per day
ggplot(daily_summary, aes(x = year_month_day)) +
  geom_smooth(aes(y = prop_cancelled), se = FALSE, colour = "blue")

# Relationship between proportion of cancellations and avg delay of non cancelled
ggplot(daily_summary, aes(x = avg_delay_non_cancelled, y = prop_cancelled)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",se = FALSE, colour = "red")

# if_else() and case_when() exercises

# 1. Determine whether each number between 0 and 20 is even or odd:
x <- 0:20
if_else(
  x %% 2 == 0,
  "even",
  "odd"
)

# 2. Weekend or weekdays?
x <- c("Monday", "Saturday", "Wednesday")
if_else(
  x == "Saturday" | x == "Sunday",
  "Weekend",
  "Weekday"
)

# 3. Absolute value of a numeric vector called x
x <- runif(50, min = -10, max = 10)
if_else(
  x < 0,
  -x,
  x
)

# 4. Label a selection of important US holidays
#  (1) Create a logical column that is either TRUE or FALSE
#  (2) Create a character column that either gies the name of the holiday or NA

flights |> 
  mutate(
    major_holiday = case_when(
      month == 1 & day == 1 ~ TRUE,
      month == 7 & day == 4 ~ TRUE,
      month == 11 & day == 28 ~ TRUE,
      month == 12 & day == 25 ~ TRUE
    ),
    holiday_label = case_when(
      month == 1 & day == 1 ~ "New Years Days",
      month == 7 & day == 4 ~ "4th of July",
      month == 11 & day == 28 ~ "Thanksgiving",
      month == 12 & day == 25 ~ "Christmas"
    )
  )
