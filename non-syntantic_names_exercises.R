library(tidyverse)
read_csv("x,y\n1,'a,b'")

# Practice referring to non-syntactic names in the following data frame by:
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

## 1. Extracting the variable called `1`.
annoying |> 
  select(`1`)

## 2. Plotting a scatterplot of `1` vs. `2`.
ggplot(annoying, aes(x = `2`, y = `1`)) +
  geom_point()
  
## 3. Creating a new column called `3`, which is `2` divided by `1`.
annoying |> 
  mutate(`3` = `2`/ `1`)

## 4. Renaming the columns to `one`, `two`, and `three`.
annoying |> 
  mutate(`3` = `2`/ `1`) |>
  rename(
    "one" = `1`,
    "two" = `2`,
    "three" = `3`
  )
