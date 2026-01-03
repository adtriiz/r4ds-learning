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


