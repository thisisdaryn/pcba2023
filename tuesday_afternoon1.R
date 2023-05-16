library(tidyverse) # contains ggplot2 

penguins <- read_csv("data/penguins.csv", 
                     show_col_types = FALSE)

# show_col_types = FALSE suppresses extra information 

count(penguins, species)

count(penguins, island)


## In base R 

plot(flipper_length_mm ~ bill_length_mm, data = penguins, 
     main = "My scatterplot", pch = 25)


ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, 
                     y = flipper_length_mm)) + 
  geom_point()


ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = flipper_length_mm)) + 
  geom_point() 


## add an aesthetic to the mapping 

ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = flipper_length_mm,
                     colour = species)) + 
  geom_point() + theme_bw()


## using shape for different species


ggplot(data = penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = flipper_length_mm,
                     color = species,
                     shape = sex)) + 
  geom_point()


### Histograms


### in base R 

hist(penguins$bill_length_mm,breaks = 50, col = "#FE2EFE",
     main = "Distribution of bill lengths for Antarctic Penguins")

ggplot(data = penguins,
       aes(x = bill_length_mm)) + 
  geom_histogram(fill = "#FE2EFE", color = "black") + theme_bw()


ggplot(data = penguins,
       aes(x = bill_length_mm,
           fill = species)) + 
  geom_histogram(color = "black")


## Using a facets to show sub groups 

ggplot(data = penguins,
       aes(x = bill_length_mm)) + 
  geom_histogram(fill = "royalblue", color = "black") + 
  facet_wrap(~species, ncol = 1) + 
  labs(title = "My big title", 
       subtitle = "Blah blah blah penguins etc",
       x = "Bill length in mm", 
       y = "number of penguins",
       caption = "data from scientists on the internet")

### Using boxplots in ggplot

ggplot(data = penguins,
       aes(y = species, 
           x = bill_length_mm)) + 
  geom_boxplot()

ggplot(data = penguins,
       aes(y = species,
           x = bill_length_mm, 
           color = sex)) + 
  geom_boxplot() + facet_wrap(~island)

ggplot(data = penguins,
       aes(x = bill_length_mm)) +
  geom_boxplot() + 
  facet_grid(island~species)

## violin plot


ggplot(data = penguins,
       aes(x = body_mass_g,
           y = species)) + geom_violin()


### beeswarm plot

library(ggbeeswarm)

ggplot(data = penguins, 
       aes(y = species, 
           x = body_mass_g,
           color = sex)) + 
  geom_beeswarm()

























