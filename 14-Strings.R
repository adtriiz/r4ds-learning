library(tidyverse)
library(babynames)

# Strings that contain certain values:
s1 <- 'He said "That\'s amazing!"'
s2 <- "\\a\\b\\c\\d"
s3 <- "\\\\\\\\\\\\"
str_view(s1)
str_view(s2)
str_view(s3)

# String with special character
x <- "This\u00a0is\u00a0tricky"
str_view(x)

str_c("hi ", NA)
paste0("hi ", NA)

str_c(letters[1:2], letters[1:3])
paste0(letters[1:2], letters[1:3])

# Convert following expressions bw str_c and str_glue
str_c("The price of ", food, " is ", price)

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, 2,state = 2)
  )

View(babynames)

# Extract middle letter from each name
babynames |> 
  mutate(
    middle = str_sub(name, (str_length(name) + 1) / 2, (str_length(name) + 1) / 2)
  )

# Trends in length
babynames |>
  mutate(length = str_length(name)) |>
  group_by(year) |> 
  summarise(
    weighte_avg_length = weighted.mean(length, n)
  ) |> 
  ggplot(aes(x = year, y = weighte_avg_length)) +
    geom_line()
# Went up and recently started going down

# Trends in popularity of first and last letters
babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  ) |> 
  group_by(year, first) |> 
  summarise(
    n_first = sum(n),
    .groups = "drop_last"
  ) |> 
  mutate(
    prop_first = n_first / sum(n_first)
  ) |> 
  ggplot(aes(x = year, y = prop_first, colour = first)) +
    geom_line()


# 15. Regular Expressions

# What baby names has the most vowels?
babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aieou]")
  ) |> 
  arrange(desc(vowels)) |> 
  slice_head(n = 10)
# mariaguadalupe

# Highest proportion of vowels
babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aieou]"),
    len_name = str_length(name),
    prop_vowels = vowels / len_name
  ) |> 
  arrange(desc(prop_vowels)) |> 
  slice_head(n = 10)
# Ai

# Replace all forward slashes in "a/b/c/d/e" with backslashes
str <- "a/b/c/d/e"
rep_str <- str_replace_all(str, "/", "\\\\")
str_view(rep_str)

# Attempt to reverse
rev_str <- str_replace_all(rep_str, "\\\\", "/")
str_view(rev_str)

# Simple version of str_to_lower() with str_replace_all(
str <- c("Ad", "DZ", "DeA")
str_lower <- str_replace_all(str, "[A-Z]", tolower)
str_view(str_lower)

# regular expression that will match telephone numbers in Spain
str_detect("674 37 65 39","\\d{3}\\s\\d{2}\\s\\d{2}\\s\\d{2}")

# Match literal strings "'\ and $^$
str <- c("\"'\\", "$^$")
str_view(str, r"{\"'\\}")
str_view(str, r"{\$\^\$}")

# Using words dataset, create regex that find all words that:
## Start with "y"
str_view(words, "^y")

## Don`t start with "y"
str_view(words, "^[^y]")

## End with "x"
str_view(words, "x$")

## Are exactly three letters long
str_view(words, "\\b\\w{3}\\b")

## Have seven letters or more
str_view(words, "\\b\\w{7,}\\b")

## Contain a vowel-consonant pair
### Expansive understanding (could be more than one)
str_view(words, "[aeiou][^aeiou]")
### Explicit understanding (only one)
str_view(words, "^[^aeiou]*[aeiou][^aeiou]+$")
#### There is probably an easier solution using count

## Contain at least two vowel-consonant pairs in a row
str_view(words, "([aeiou][^aeiou]){2,}")

## Only consist of repeated vowel-consonant pairs
str_view(words, "\\b([aeiou][^aeiou]){2,}\\b")

# Regex that match British and American spelling for:
## airplane/aeroplane
str <- c("airplane", "aeroplane")
str_view(str, "a(ir|ero)plane")

## aluminum/aluminium
str <- c("aluminum", "aluminium")
str_view(str, "alumin(i)?um")


## analog/analogue
str <- c("analog", "analogue")
str_view(str, "analog(ue)?")

## I think I got it pretty ok, let's move on

# Switch the first and last letters in words. Which one are still words?
words_alt <- str_replace(words, "(^.)(.*)(.$)", "\\3\\2\\1")


# Solve using both a single regular expression and combination of str_detect() calls
## All words that start or end with "x"
str_view(words, "^x|x$")
words[str_detect(words, "^x") | str_detect(words, "x$")]

## All words that start with a vowel and end with a consonant
str_view(words, "(^[aieou])(.+)([^aeiou]$)")
words[str_detect(words, "^[aeiou]") & !str_detect(words, "[aeiou]$")]

## Words that contain at least one of each vowel
words[
  str_detect(words, "a") &
  str_detect(words, "e") &
  str_detect(words, "i") &
  str_detect(words, "o") &
  str_detect(words, "u")
]

## Evidence for and against: "i before e except after c"
str_view(words, ".*[^c]ie.*")
str_view(words, ".*cie.*")

# Automatically identify modifiers
colours <- tibble(name = colors())

colours |> 
  filter(str_detect(name, str_c(colors(),collapse = "|")))

# Regex that finds any base R dataset.


