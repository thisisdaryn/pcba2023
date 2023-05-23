library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample)

cars2020 <- read_csv("data/cars2020.csv", 
                     show_col_types = FALSE)

set.seed(1729)
cars_split <- initial_split(cars2020, prop = 0.8)

cars_training <- training(cars_split)
cars_testing <- testing(cars_split)

model1 <- lm(mpg ~ disp + transmission + atvType, 
             data = cars_training)
model1

# two different ways to append the prediction

cars_testing_copy <- cars_testing

cars_testing$model1 <- predict(model1, newdata = cars_testing)

cars_testing_copy <- cars_testing_copy |> 
  mutate(model1 = predict(model1, newdata = cars_testing))

ggplot(data = cars_testing,
       aes(x = mpg, y = model1)) + 
  geom_abline(intercept = 0, slope = 1, 
              colour = "orange", linetype = "dashed"
              ) + 
  geom_point() + xlim(c(0, 60)) + 
  ylim(c(0, 60))

## Question: can you try different combinations of 4 input variables to come up
## with your own mpg prediction models?

model2 <- lm(mpg~disp+fuel+aspiration, data = cars_training)

model3 <- lm(mpg~disp+fuel+aspiration+atvType, data = cars_training)

model4 <- rpart(mpg~cyl+disp, data = cars_training)

cars_testing$model2 <- predict(model2, newdata = cars_testing)
cars_testing$model3 <- predict(model3, newdata = cars_testing)
cars_testing$model4 <- predict(model4, newdata = cars_testing)

cars_testing_pivot <- cars_testing |>
  pivot_longer(model1:model4,
               names_to = "pred_model", 
               values_to = "prediction")

ggplot(cars_testing_pivot,
       aes(x = mpg, y = prediction)) + 
  geom_abline(intercept = 0, slope = 1,
              colour = "orange", linetype = "dashed") + 
  geom_point() + facet_wrap(~pred_model) + 
  xlim(c(0,60)) + ylim(c(0,60))
