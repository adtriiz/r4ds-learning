library(tidyverse)

diamonds |> 
  group_by(cut) |> 
  summarise(
    ymin = min(depth),
    y = median(depth),
    ymax = max(depth),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = cut, y = y, ymin = ymin, ymax = ymax)) +
  geom_pointrange()

ggplot(diamonds, aes(x = cut, y = after_stat(prop))) + 
  geom_bar()

View(mpg)

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_jitter()

ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot(varwidth = TRUE)

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline()
