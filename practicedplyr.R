library(nycflights13)
library(tidyverse)

# flights
# 
# glimpse(flights)

flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

# filter()

# When you run filter() dplyr executes the filtering operation, creating a new data 
# frame, and then prints it. It doesn’t modify the existing flights dataset because 
# dplyr functions never modify their inputs. To save the result, you need to use the 
# assignment operator, <-
  
flights |> 
  filter(dep_delay > 120)

# Flights that departed on January 1
flights |> 
  filter(month == 1 & day == 1)

# Flights that departed in January or February
flights |> 
  filter(month == 1 | month == 2)

# A shorter way to select flights that departed in January or February
flights |> 
  filter(month %in% c(1, 2))

jan1 <- flights |> 
  filter(month == 1 & day == 1)

# arrange()

# arrange() changes the order of the rows based on the value of the columns. It 
# takes a data frame and a set of column names (or more complicated expressions) 
# to order by. If you provide more than one column name, each additional column 
# will be used to break ties in the values of preceding columns. 

flights |> 
  arrange(year, month, day, dep_time)

# re-order the data frame based on that column in descending
flights |> 
  arrange(desc(dep_delay))

# distinct()
# distinct() finds all the unique rows in a dataset, so in a technical sense, it 
# primarily operates on the rows.

# Remove duplicate rows, if any
flights |> 
  distinct()

# Find all unique origin and destination pairs
flights |> 
  distinct(origin, dest)

flights |> 
  distinct(origin, dest, .keep_all = TRUE)

# If you want to find the number of occurrences instead, you’re better off swapping 
# distinct() for count(), and with the sort = TRUE argument you can arrange them in 
# descending order of number of occurrences
flights |>
  count(origin, dest, sort = TRUE)


# mutate()
