# install.packages("scales")
# install.packages("ggrepel")
# install.packages("patchwork")

library(tidyverse)

# Creating a plot on the fuel economy data with customised labels
View(mpg)

# Mapping distribution of highway mpg depending on vehicle class
ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot(aes(colour = as.factor(year))) +
  labs(
    title = "Larger vehicles are less fuel efficient",
    subtitle = "All cars became slightly more efficient over time except subcompacts",
    caption = "Date from fueleconomy.gov",
    x = "Car type",
    y = "Highway fuel economy (mpg)",
    color = "year"
  )

# Recreate a plot
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(aes(shape = drv, colour = drv)) +
  labs(
    x = "City MPG",
    y = "Highway MPG",
    colour = "Type of drive train",
    shape = "Type of drive train"
  )

# Place text at the four corners of the plot

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  geom_text(
    data = data.frame(
      x = c(-Inf,  Inf, -Inf,  Inf),
      y = c(-Inf, -Inf,  Inf,  Inf),
      label = c("Bottom-left", "Bottom-right", "Top-left", "Top-right")
    ),
    aes(x = x, y = y, label = label),
    vjust = c(-0.5, -0.5, 1, 1),
    hjust = c(-0.5, 1, -0.5, 1)
  )

# Add a point geom in the middle with annotate()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  geom_text(
    data = data.frame(
      x = c(-Inf,  Inf, -Inf,  Inf),
      y = c(-Inf, -Inf,  Inf,  Inf),
      label = c("Bottom-left", "Bottom-right", "Top-left", "Top-right")
    ),
    aes(x = x, y = y, label = label),
    vjust = c(-0.5, -0.5, 1, 1),
    hjust = c(-0.5, 1, -0.5, 1)
  ) +
  annotate(
    geom = "point",
    x = 22.5, y = 30, colour = "red", size = 3
  )

# Faceting and geom_text
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  facet_grid(rows = vars(drv)) +
  geom_text(aes(x, y, label = lab),
    data = data.frame(x = -Inf, y = Inf, lab = c("", "", "Amazing"), drv = c("4","f","r")),
    vjust = 1,
    hjust = "left"
  )

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)))

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d()
ggplot(diamonds, aes(x = log10(carat), y = log10(price))) +
  geom_bin2d()

# Why doesn`t this code override the default scale?
library(scales)

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_color_gradient(low = "white", high = "red") +
  coord_fixed()
# Should be scale_fill_gradient instead

# Change the display of the presidential terms by:
# 1. Combining the two variants that customize colors and x axis breaks.
# 2. Improving the display of the y axis.
# 3. Labelling each term with the name of the president.
# 4. Adding informative plot labels.
# 5. Placing breaks every 4 years (this is trickier than it seems!).
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(
    name = NULL, 
    # breaks = presidential$start,
    date_breaks = "4 years",
    date_labels = "'%y",
    minor_breaks = NULL
  ) +
  scale_color_manual(values = c(Republican = "#E81B23", Democratic = "#00AEF3")) +
  scale_y_continuous(
    name = "President #",
    labels = label_ordinal()
  ) +
  geom_text(
    aes(x = start, y = id, label = name),
    size = 3,
    nudge_y = 0.5,
    hjust = "left",
    show.legend = FALSE
  ) +
  labs(
    title = "US Presidents since 1953"
  )

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
  

# Using a ggthemes theme for last generated graph
library(ggthemes)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_solarized(light = FALSE)

# Make the axis labels blue and bolded

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(
    axis.text = element_text(color = "blue", face = "bold")
  )

library(patchwork)

p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 1")

p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) +
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 2")

p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 3")

p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 4")

p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~drv) +
  labs(title = "Plot 5")

(guide_area() / (p1 + p2) / (p3 + p4) / p5) +
  plot_annotation(
    title = "City and highway mileage for cars with different drive trains",
    caption = "Source: https://fueleconomy.gov."
  ) +
  plot_layout(
    guides = "collect",
    heights = c(1, 3, 2, 4)
  ) &
  theme(legend.position = "top")

# testing parenthesis in patchwork
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) + 
  geom_boxplot() + 
  labs(title = "Plot 2")
p3 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 3")

(p1 | p2) / p3
# Answer: / has precedence over | (or +)

p1 / (p2 | p3)

