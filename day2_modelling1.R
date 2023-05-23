library(tidyverse) 
library(rpart)
library(rpart.plot)

cars2020 <- read_csv("data/cars2020.csv",
                     show_col_types = FALSE)

model1 <- rpart(mpg~transmission+disp, data = cars2020)

rpart.plot(model1)

model2 <- rpart(mpg~disp+class+atvType, data = cars2020)

rpart.plot(model2)

## to actually assess the model

cars2020$model1 <- predict(model1, newdata = cars2020)

ggplot(data = cars2020) + 
  geom_point(aes(x = mpg, y = model1),
             alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "orange") + 
  xlim(c(0,70)) + ylim(c(0,70))

### checking model1 model1 vs mpg

ggplot(data = cars2020) + 
  geom_point(aes(x = mpg, y = model1),
             alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "orange") + 
  xlim(c(0,70)) + ylim(c(0,70))


## checking model2 vs mpg
cars2020$model2 <- predict(model2, newdata = cars2020)

ggplot(data = cars2020) + 
  geom_point(aes(x = mpg, y = model2),
             alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "orange") + 
  xlim(c(0,70)) + ylim(c(0,70))

## Linear regression model using only displacement

lm1 <- lm(mpg~disp, data = cars2020)

cars2020$lm1 <- predict(lm1, newdata = cars2020) 

## Linear regression model using only transmission

lm2 <- lm(mpg~transmission, data = cars2020)

cars2020$lm2 <- predict(lm2, newdata = cars2020)

lm2

ggplot(data = cars2020) + 
  geom_point(aes(x = mpg, y = lm2),
             alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "orange") + 
  xlim(c(0,70)) + ylim(c(0,70))


## linear model using disp and transmission

lm3 <- lm(mpg~disp+transmission, data = cars2020)

cars2020$lm3 <- predict(lm3, newdata = cars2020)

ggplot(data = cars2020) + 
  geom_point(aes(x = mpg, y = lm3),
             alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, 
              color = "orange") + 
  xlim(c(0,70)) + ylim(c(0,70))









