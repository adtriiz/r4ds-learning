library(tidyverse)

# Scatterplot of hwy vs displ with pink triangle points
ggplot(mpg, aes(x = hwy, y = displ)) +
  geom_point(shape = 24, fill = "pink")

# Stroke test
ggplot(mpg, aes(x = hwy, y = displ)) +
  geom_point(shape = 2, stroke = 2)

# Map one aesthetic to something other than a variable name
ggplot(mpg, aes(x = hwy, y = displ, color = displ < 5)) +
  geom_point()


# Recreate the R code necessary to generate the following graphs.
# Note that wherever a categorical variable is used in the plot, it’s drv.
# Graph 1
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)
  
# Graph 2
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv), se = FALSE)

# Graph 3
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Graph 4
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(se = FALSE)

# Graph 5
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(aes(linetype = drv), se = FALSE)

# Graph 6 - Interesting case to play around with and without aes() inside the geoms
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_point(shape = "circle open", size = 3, color = "white")
