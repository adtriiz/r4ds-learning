library(tidyverse)
library(nycflights13)

View(flights)

flights |> count(dest, sort = TRUE) |> View()

# Number of rows with a missing value for dep_time
flights |> count(dest, wt = is.na(dep_time))
flights |> count(is.na(dep_time))

# Using group_by(), summarise(), and arrange():
# 1. flights |> count(dest, sort = TRUE)
flights |> 
  group_by(dest) |> 
  summarise(count = n()) |> 
  arrange(desc(count))

# 2. flights |> count(tailnum, wt = distance)
flights |> 
  group_by(tailnum) |> 
  summarise(miles = sum(distance))

# Explain in words each line of code for:

# Choose dataset
flights |> 
  # Groups by transformed variable from sched_dep_time which extracts only the hour
  group_by(hour = sched_dep_time %/% 100) |> 
  # For each hour, finds the proportion of cancelled flights, and the total number of flights
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
  # Filters out an outlier (see below)
  filter(hour > 1) |> 
  # Sets up the x and y aesthetics for the plot
  ggplot(aes(x = hour, y = prop_cancelled)) +
  # Creates a line graph for the x and y that unites all (x,y) observations
  geom_line(color = "grey50") + 
  # Displays how many flights each hour has based on bubble size
  geom_point(aes(size = n))

flights |> 
  mutate(hour = sched_dep_time %/% 100) |> 
  filter(hour == 1) |> View()

# Convert dep_time & sched_dep_time to a more truthful representation of time
flights |> 
  filter(month == 1, day == 1) |> 
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +
  geom_point()

# In fractional hours
flights |> 
  mutate(
    frac_dep_time = dep_time %/% 100 + (dep_time %% 100 / 60),
    frac_sched_dep_time = sched_dep_time %/% 100 + (sched_dep_time %% 100 / 60)
  ) |>
  filter(month == 1, day == 1) |> 
  ggplot(aes(x = frac_sched_dep_time, y = dep_delay)) +
  geom_point()
  
# In minutes since midnight
flights |> 
  mutate(
    mid_dep_time = (dep_time %/% 100 + (dep_time %% 100 / 60)) * 60,
    mid_sched_dep_time = (sched_dep_time %/% 100 + (sched_dep_time %% 100 / 60)) * 60
  ) |>
  filter(month == 1, day == 1) |> 
  ggplot(aes(x = mid_sched_dep_time, y = dep_delay)) +
  geom_point()

# Round dep_time to nearest 5 minute
flights |> 
  mutate(
    rounded_dep_time = round(dep_time / 5) * 5,
    rounded_arr_time = round(arr_time / 5) * 5
  )


# Exercises on General Transformations, using ranks, offsets and consecutive identifiers

# Find 10 most delayed flights
flights |> 
  mutate(
    delay_rank = min_rank(-pick(arr_delay, dep_delay))
  ) |>
  filter(delay_rank < 11) |> View()

# Plane with the worst on-time record
flights |> 
  group_by(tailnum) |> 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) |> 
  filter(min_rank(desc(avg_arr_delay)) == 1) |> View()

# Time of day to avoid delays as much as possible:
flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarise(prop_cancelled = mean(is.na(dep_time), na.rm = TRUE)) |> 
  filter(min_rank(prop_cancelled) == 1) |> View()
# At 5am

# For each destination,compute the total minutes of delay
flights |> 
  group_by(dest) |> 
  summarise(sum(dep_delay, na.rm = TRUE)) |> View()

# For each flight, compute the proportion of total delay for its destination
flights |> 
  group_by(dest) |> 
  mutate(prop_delay = dep_delay / sum(dep_delay, na.rm = TRUE)) |> View()


# How does the average flight delay for an hour is related to the average delay for the previous hour
flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  filter(n > 5) |> 
  mutate(change = dep_delay - lag(dep_delay)) |> 
  ggplot(aes(x = dep_delay, y = change)) +
    geom_point()
# There seems to be a positive correlation between dep_delay and change

# Suspiciously fast flights
flights |> 
  group_by(dest) |> 
  mutate(
    speed = distance / (air_time / 60)
  ) |> 
  ggplot(aes(x = speed)) +
    geom_histogram()
# Seems like anything above 600 would be suspicious. I'll try creating two groups:
flights |> 
  group_by(dest) |> 
  mutate(
    speed = distance / (air_time / 60)
  ) |>
  filter(speed > 600) |> View()
# There are (at least) 4 suspiciously fast flights

# Another approach is for each destination, ordering them from fastest to slowest, and checking the lag between the fastest and second fastest
flights |>
  mutate(speed = distance / (air_time / 60)) |>
  group_by(dest) |>
  arrange(dest, desc(speed)) |> 
  mutate(
    diff = speed - lead(speed)
  ) |> 
  slice_head(n = 1) |> arrange(desc(diff)) |> View()
# An additional flight has emerged

# Compute the air time of a flight relative to the shortest flight to that destination
flights |> 
  group_by(dest) |> 
  mutate(
    prop_air_min = air_time / min(air_time, na.rm = TRUE)
  ) |> View()

# Which flights were most delayed in the air?
flights |> 
  mutate(
    delayed_on_air = (arr_delay - dep_delay) / air_time,
    rank = min_rank(desc(delayed_on_air))
  ) |> arrange(rank) |> slice_head(n = 10) |> View()
# Most delayed per minute on air - all the values seem a bit implausible, though
# air_time < (arr_delay-dep_delay)

# Alternative approach comparing each flight to the median air_time for their destination
flights |> 
  filter(!is.na(air_time)) |> 
  group_by(dest) |> 
  mutate(
    median_air_time = median(air_time, na.rm = TRUE),
    air_dev = air_time - median_air_time,
    air_dev_minute = air_dev / median_air_time
  ) |> 
  ungroup() |> 
  arrange(desc(air_dev)) |> 
  slice_head(n = 10) |> View()
# Depending on whether we use air_dev_minute or air_dev in the arrange, we will get different flights

# Destination flown by at least two carriers
flights |> 
  group_by(dest) |> 
  arrange(carrier) |> 
  mutate(
    carrier_dest_id = consecutive_id(carrier) 
  ) |> 
  filter(max(carrier_dest_id) >= 2)
# Overcomplicated way, but worked :) 76 destinations served by two or more carriers

# Use those destinations to come up with a relative ranking of the carriers based on their performance for the same destination.
flights |> 
  group_by(dest) |> 
  arrange(carrier) |> 
  mutate(
    carrier_dest_id = consecutive_id(carrier) 
  ) |> 
  filter(max(carrier_dest_id) >= 2) |>
  ungroup() |> 
  group_by(dest, carrier) |> 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  group_by(dest) |> 
  mutate(
    carrier_rank = min_rank(avg_delay)
  ) |>
  arrange(dest, carrier_rank)

# According to ChatGPT, the cleanest way would be:
# Step 1: compute carrier performance per destination
carrier_perf <- flights |> 
  group_by(dest, carrier) |> 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop")

# Step 2: keep destinations served by at least two carriers
multi_carrier_dest <- carrier_perf |> 
  group_by(dest) |> 
  filter(n() >= 2) |> 
  mutate(carrier_rank = min_rank(avg_delay)) |> 
  arrange(dest, carrier_rank)


# Reproducing Figure 13.3:
# This computes the overall distribution of departure delays.
library(patchwork)
plot1 <- ggplot(flights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 5)
plot2 <- flights |>
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay)) +
  geom_histogram(binwidth = 5)

plot1 | plot2

# Which destinations show the greatest variation in air speed?
flights |> 
  group_by(dest) |> 
  summarise(
    air_time_iqr = IQR(air_time, na.rm = TRUE),
    n = n()
  ) |> arrange(desc(air_time_iqr))
# HNL, HOU, SNA, ABQ, BUR

# Using sd
flights |> 
  group_by(dest) |> 
  summarise(
    air_time_sd = sd(air_time, na.rm = TRUE),
    n = n()
  ) |> arrange(desc(air_time_sd))
# HNL, SNA, OKC, ABQ, SAN - so slightly different

# What about doing the actual speed calculation?
flights |> 
  mutate(speed = distance / (air_time / 60)) |> 
  group_by(dest) |> 
  summarise(
    speed_iqr = IQR(speed, na.rm = TRUE),
    n = n()
  ) |> arrange(desc(speed_iqr))

# HOU, MSY, IAH, OKC, CLT - quite different, probably more correct

# What is going on with EGE, where the distance is not always the same?
flights |>
  group_by(origin, dest) |>
  summarise(
    distance_iqr = IQR(distance),
    n = n(),
    .groups = "drop"
  ) |>
  filter(distance_iqr > 0)

# Distance distribution for EGE
flights |> 
  filter(dest == "EGE") |> 
  ggplot(aes(x = distance)) +
  geom_histogram(binwidth = 1) +
  facet_grid(vars(origin))
# Seems like it moved by 1 KM

# Was there a temporal moment when distance was altered?
flights |> 
  filter(dest == "EGE", origin == "EWR") |>
  arrange(year, month, day) |> 
  mutate(
    diff = distance - lag(distance, default = first(distance)),
    is_different = diff != 0,
    group = cumsum(is_different)
  )

flights |> 
  filter(dest == "EGE", origin == "JFK") |>
  arrange(year, month, day) |> 
  mutate(
    diff = distance - lag(distance, default = first(distance)),
    is_different = diff != 0,
    group = cumsum(is_different)
  ) |> View()

# For both airports of origin, the discontinuity occurs on the same date, Mar 1st
# So it is likely that something happened that date

# ChatGPT suggested code
flights |>
  group_by(origin, dest) |>
  arrange(year, month, day) |>
  mutate(
    old_distance = lag(distance),
    new_distance = distance,
    date = as.Date(paste(year, month, day, sep = "-"), "%Y-%m-%d")
  ) |>
  filter(new_distance != old_distance) |>
  summarise(
    change_date = first(date),
    old_distance = first(old_distance),
    new_distance = first(new_distance),
    .groups = "drop"
  )


