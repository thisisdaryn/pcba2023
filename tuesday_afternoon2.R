library(tidyverse)

cars2020 <- read_csv("data/cars2020.csv")

## can you plot a scatterplot of mpg (y-axis) vs disp (x-axis)?

ggplot(data = cars2020,
       mapping = aes(x = disp,
                     y = mpg)) + 
  geom_point() + geom_smooth(method = "lm")


ggplot(data = cars2020,
       mapping = aes(x = mpg, 
                     fill = transmission)) + 
  geom_histogram(color = "black") + 
  facet_wrap(~transmission, ncol = 1, scales = "free_y") 


ggplot(data = cars2020,
       mapping = aes(x = mpg, y = transmission)) + 
  geom_boxplot()
