library(tidyverse)

# Distributions of x, y and z
ggplot(diamonds, aes( x = x)) + 
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0,12))
# x is bimodal (4 & 7), with most observations between 3 and 9.

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0,12))
# y pretty much similar distribution

ggplot(diamonds, aes(x = z)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(xlim = c(0,12))
# z is smaller on average, ranging from 2 to 7, with most observations around 3 and 4

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 50)

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(0.95, 1.05))

diamonds |> 
  filter(carat == 0.99) |> 
  count()

diamonds |> 
  filter(carat == 1) |> 
  count()

diamonds |> 
  filter(carat == 0.99) |> 
  summarise(mean = mean(price, na.rm = TRUE))

# Using facet
nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min/60
  ) |>
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), bindwidth = 1/4) +
  facet_grid(vars(cancelled), scales = "free_y")

# Using density
nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min/60
  ) |>
  ggplot(aes(x = sched_dep_time, y = after_stat(density))) +
    geom_freqpoly(aes(color = cancelled), binwidth = 1, linewidth = 0.75)

# Using EDA to explore the var in the diamonds dataset that predicts
# the price of the diamond better
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()

ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()
# Price is strongly correlated with carat, but larger carats are also associated with lower quality cuts.

ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot() +
  coord_flip()

ggplot(mpg, aes(x = hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()

# Experimenting with lvplot as an "improvement" over boxplots for large datasets
library(lvplot)
ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_lv()

# Diamond prices vs categorical var visualisation
# 1. Using violin
ggplot(diamonds, aes(x = price, y = color)) +
  geom_violin()

# 2. Faceted histogram
ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_grid(rows = vars(color))

# 3. Coloured Freqpoly
ggplot(diamonds, aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(colour = color), binwidth = 500, linewidth = 0.25)

# 4. Coloured Density
ggplot(diamonds, aes(x = price)) +
  geom_density(aes(colour = color))

# Testing the varwidth argument in boxplot
smaller <- diamonds |> 
  filter(carat < 3)

ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)), varwidth = TRUE)


# Rescaling tile for better visualisation of two categorical variables
# By color
diamonds |>
  count(color, cut) |>
  group_by(color) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = prop))

# By cut
diamonds |>
  count(color, cut) |>
  group_by(cut) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = prop))

# Bar chart showing cut distribution for each color (absolute)
diamonds |> 
  ggplot(aes(color)) +
  geom_bar(aes(fill = cut))

# How do average flight departure delays vary by destination and month of year?
View(nycflights13::flights)

nycflights13::flights |> 
  group_by(dest, month) |> 
  summarise(mean = mean(dep_delay, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = dest, y = month)) +
    geom_tile(aes(fill = mean))

# Make it better by grouping by destination? NO
nycflights13::flights |>
  group_by(dest, month) |>
  summarise(mean_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") |>
  group_by(month) |>
  mutate(prop = mean_delay / sum(mean_delay, na.rm = TRUE)) |>
  ggplot(aes(x = dest, y = month)) +
  geom_tile(aes(fill = prop))

# Make it better by ordering by avg dep_delay? MAYBE
nycflights13::flights |>
  group_by(dest, month) |>
  summarise(prop = mean(dep_delay, na.rm = TRUE), .groups = "drop") |>
  mutate(dest = reorder(dest, prop, mean)) |>
  ggplot(aes(x = dest, y = month)) +
  geom_tile(aes(fill = prop))

# Summarise conditional distribution with frequency polygon:
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

ggplot(smaller, aes(x = price)) +
  geom_freqpoly(aes(colour = carat, group = cut_width(carat, 0.1)))
# Not sure what is happening here to be honest

# Distribution of carat partitioned by price
ggplot(smaller, aes(x = carat, y = after_stat(density) , colour = cut_width(price, 5000))) +
  geom_freqpoly()

# Distribution of smaller vs larger diamonds
very_small <- diamonds |> 
  filter(carat < 1)
very_large <- diamonds |> 
  filter(carat > 3)
ggplot(very_small, aes(x = price, y = after_stat(density))) +
  geom_histogram(aes(fill = "red")) +
  geom_histogram(data = very_large, aes(fill = "blue"))

# Combine two techniques to visualise the combined distribution of cut, carat
# and price
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(colour = cut)) +
  facet_grid(rows = vars(cut))

# Why is a scatterplot better than a binned plot for this case?
diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_bin2d() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

# cut_width vs cut_number - Adv vs Disadv
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_number(carat, 20)))

ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))
