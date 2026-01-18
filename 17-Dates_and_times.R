library(tidyverse)
library(nycflights13)

# Exercises 1

# 1. What happens if you parse a string that contains invalid dates?
## Example:
ymd(c("2010-10-10", "bananas"))
## A: You a vector with the correct parsing and NA.
## Does that number in the warning messsage count the number of strings that contain invalid dates?
ymd(c("2010-10-10", "bananas", "tomato"))
### Yes it does

# 2. What does the tzone argument to today() do? Why is it important?
## It lets us specify the relevant time zone, defaulting to the user's computer system timezone.
## It is important especially if you are collaborating with other in different timezones if consistency is important.
## It may also be important if computing boolean operations that are sensitive to time differences

# 3. For each of the following date-times, show how you`d parse it using a readr column specification and a lubridate function
d1 <- "January 1, 2010"
d1_csv <- "
  date
  January 1, 2010
"
read_csv(d1_csv, col_types = cols(date = col_date("%B %d, %Y")))
mdy(d1)

d2 <- "2015-Mar-07"
d2_csv <- "
  date
  2015-Mar-07
"
read_csv(d2_csv, col_types = cols(date = col_date("%Y-%b-%d")))
ymd(d2)

d3 <- "06-Jun-2017"
d3_csv <- "
  date
  06-Jun-2017
"
read_csv(d3_csv, col_types = cols(date = col_date("%d-%b-%Y")))
dmy(d3)

d4 <- c("August 19 (2015)", "July 1 (2015)")
d4_csv <- "
  date
  August 19 (2015)
  July 1 (2015)
"
read_csv(d4_csv, col_types = cols(date = col_date("%B %d (%Y)")))
mdy(d4)

d5 <- "12/30/14" # Dec 30, 2014
d5_csv <- "
  date
  12/30/14
"
read_csv(d5_csv, col_types = cols(date = col_date("%m/%d/%y")))
mdy(d5)

t1 <- "1705"
t1_csv <-"
  time
  1705
"
read_csv(t1_csv, col_types = cols(time = col_time("%H%M")))
hms(t1)
## Note that I am not sure if this refers to 5:05pm or number of seconds since 00:00:00

t2 <- "11:15:10.12 PM"
t2_csv <- "
  time
  11:15:10.12 PM
"
read_csv(t2_csv, col_types = cols(time = col_time("%I:%M:%OS %p")))
hms(t2)
## Seems like Lubridate helpers have limitations;
### Might help with simple date-times, but more complex one will require something extra

# Constructing the flights_dt tibble
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))

# VERY USEFUL: Show the distribution of flights across the course of a day
flights_dt |>
  mutate(dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day"))) |>
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

# Set of Exercises #2

# 1.How does the distribution of flight times within a day change over the course of the year?
## We have some options to observe the distribution. Let's start by comparing months
flights_dt |> 
  mutate(
    dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day")),
    dep_month = month(dep_time)
  ) |> 
  ggplot(aes(x = dep_hour)) + 
  geom_freqpoly(aes(colour = as_factor(dep_month)), binwidth = 60 * 30) +
  facet_wrap(vars(as_factor(dep_month))) +
  theme(legend.position = "none")
## Hard to discern anything... Let's compare the median flight time by day
flights_dt |> 
  mutate(
    dep_day = as_date(dep_time),
    dep_time_1 = hms::as_hms(dep_time),
    dep_month = month(dep_time)
  ) |> 
  group_by(dep_day, dep_month) |> 
  summarise(day_median = median(dep_time_1)) |> 
  ungroup() |> 
  ggplot(aes(x = dep_day, y = day_median)) +
  geom_line(aes(colour = as.factor(dep_month)))
## Again, nothing really to discern here

# 2. Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.
## Compare whether the difference between both is equal to dep_delay
flights_dt |> 
  mutate(
    delay = (dep_time - sched_dep_time) / dminutes(1),
    same = delay == dep_delay,
    difference = delay - dep_delay
  ) |>
  filter(abs(difference) > 0)
## A: Mostly consistent, but for the ones that aren't, the difference is 1440 minutes, or 24 hours between all of them, which means that the departure date should have been pushed forward

# 3. Compare air_time with the duration between the departure and arrival. Explain your findings. (Hint: consider the location of the airport.)
flights_dt |> 
  mutate(
    computed_air_time = (arr_time - dep_time) / dminutes(1),
    same = computed_air_time == air_time,
    difference = computed_air_time - air_time
  ) |> 
  filter(abs(difference) > 0)
## They will be very different due to timezone differences. However, I would expect them to cluster around every 60 minutes, but that does not seem to be the case at first glance.
flights_dt |> 
  mutate(
    computed_air_time = (arr_time - dep_time) / dminutes(1),
    difference = abs(computed_air_time - air_time)
  ) |>
  filter(abs(difference) < 500) |> # discard too high of values
  ggplot(aes(x = difference)) +
  geom_histogram(binwidth = 5)
## There is a bit of clustering, but it is still too oddly distributed

# 4. How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time? Why?
## We should probably use sched_dep_time so that we know, given a scheduled departing time, what's its average delay
flights_dt |> 
  mutate(
    dep_hour = hms::as_hms(floor_date(sched_dep_time, "15 minutes"))
  ) |> 
  group_by(dep_hour) |> 
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) |> 
  ggplot(aes(x = dep_hour, y = avg_delay)) +
  geom_line()
# It gets worse as the day progresses - probably early delays compound on later flights

# 5. On what day of the week should you leave if you want to minimise the chance of a delay?
## We are talking chance, so let's see both the probability & delay time
flights_dt |> 
  mutate(
    dep_weekday = wday(sched_dep_time, label = TRUE)
  ) |> 
  group_by(dep_weekday) |> 
  summarise(
    prop_delay = sum(dep_delay > 0) / n()
  )
## Least likely day is Saturday. But what about median delay?
flights_dt |> 
  mutate(
    dep_weekday = wday(sched_dep_time, label = TRUE)
  ) |> 
  group_by(dep_weekday) |> 
  summarise(
    median_delay = median(dep_delay, na.rm = TRUE),
    mean_delay = mean(dep_delay, na.rm = TRUE)
  )
# Same picture here - especially avoid Thursdays!

# 6. What makes the distribution of diamonds$carat and flights$sched_dep_time similar?
p1 <- ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

p2 <- ggplot(flights_dt, aes(x = hms::as_hms(sched_dep_time))) +
  geom_histogram(bins = 24*6)

p1 + p2
## Both have discontinuities and peaks

# 7. Confirm our hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early. Hint: create a binary variable that tells you whether or not a flight was delayed.
flights_dt |> 
  mutate(
    minute = minute(dep_time),
    is_delayed = dep_delay > 0
  ) |> 
  group_by(minute) |> 
  summarise(
    prop_delayed = mean(is_delayed, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = minute, y = prop_delayed)) +
  geom_point() +
  geom_line()
## I think we can confirm the hypothesis


# Set of Exercises #3

# 1. Explain days(!overnight) and days(overnight) to someone who has just started learning R. What is the key fact you need to know?
## A. overnight is a binary variable, where a value of means that the condition (arr_time < dep_time) is true, and 0 is false
## Thus, given that days() returns a period corresponding to the number of days inputted, days(overnight) will return a 1 day period for the overnight flights
## Adding the exclamation mark (!) in front inverts the condition, so it would return 1 if the condition is false
  
# 2. Create a vector of dates giving the first day of every month in 2015. Create a vector of dates giving the first day of every month in the current year.
## We start with the initial point
start_2015 <- ymd("2015-01-01")
## Then create a vector, starting adding 0 until 11
first_day_months_2015 <- start_2015 + months(0:11)

## To get the first day of the current year:
start_current <- today() - (day(today())+1) + (month(today())+1)
## Then repeat what we did above
first_day_months_current <- start_current + months(0:11)


# 3. Write a function that given your birthday (as a date), returns how old you are in years.
## First we register the birthday
birthday <- ymd("1998-04-23")
## Then create an interval between today and that day
age_interval <- birthday %--% today()
## And finally transform it into years
age_years <- age_interval %/% years(1)
age_years
