library(tidyverse)

cars2020 <- read.csv("data/cars2020.csv")

## Using filter 

manual <- filter(cars2020, transmission == "Manual")

cyl_4 <- filter(cars2020, cyl >= 4)

manual_cyl4 <- filter(cars2020, transmission == "Manual",
                      cyl >= 4)

experiment <- filter(cars2020, transmission == "Manual", 
                     cyl >= 4, drive == "RWD")

auto_l6 <- filter(cars2020, transmission == "Automatic", 
                  gears < 6)


### Using select 

cars_narrow <- select(cars2020, model, mpg, transmission, cyl)

cars_alt <- select(cars2020, -startStop, -aspiration)

### using arrange (to sort)

cars_sorted <- arrange(cars2020, mpg)

cars_desc <- arrange(cars2020, desc(mpg))

cars_tmpg <- arrange(cars2020, transmission, desc(mpg))


## evaluation: 1) restrict to cvt 2) remove sidi, aspiration, startStop,
# 3) sort by mpg (descending) and then by model

df1 <- filter(cars2020, transmission == "CVT")
df2 <- select(df1, -sidi, -aspiration, -startStop)
df3 <- arrange(df2, desc(mpg), model)

## alternatively:

df1 <- filter(cars2020, transmission == "CVT")
df1 <- select(df1, -sidi, -aspiration, -startStop)
df1 <- arrange(df1, desc(mpg), model)


## another alternative (using the pipe |>)

df <- filter(cars2020, transmission == "CVT") |> 
  select(-sidi, -aspiration, -startStop) |>
  arrange(desc(mpg), model)

df2 <- cars2020 |> filter(transmission == "CVT") |>
  select(-sidi, -aspiration, -startStop) |>
  arrange(desc(mpg), model)


## Using mutate to add columns


cars_30plus <- mutate(cars2020, 
                      above30 = if_else(mpg >= 30, TRUE, FALSE))

head(cars_30plus)



cars30_alt <- cars2020 |> mutate(cars2020, above30 = if_else(mpg >= 30, TRUE, FALSE))


## example of a scatterplot in base R 


plot(mpg~disp, data = cars2020)


## using summarise 

report <- cars2020 |> summarise(num_cars = n(), 
                                avg_mpg = mean(mpg, na.rm = TRUE))

report2 <- cars2020 |> 
  group_by(make) |> 
  summarise(num_cars = n(), avg_mpg = mean(mpg, na.rm = TRUE))

report3 <- cars2020 |> 
  group_by(make, transmission) |>
  summarise(num_cars = n(), avg_mpg = mean(mpg, na.rm = TRUE),
            .groups = "drop")

## grouping behaviour can be tricky if you are using multiple summarise calls



temp <- group_by(cars2020, make)

















hist(cars2020$cyl)
