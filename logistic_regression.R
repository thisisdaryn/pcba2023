library(tidyverse)
library(rsample)
library(ggbeeswarm)

so <- read_csv("data/stackoverflow.csv",
               show_col_types = FALSE) |>
  mutate(remote2 = if_else(remote == "Not remote", 0, 1)) #|> 
  #select(country, years_coded_job, open_source, remote, remote2)

set.seed(1234)

count(so, remote)

so_split <- initial_split(so, prop = 0.8)
so_training <- training(so_split)
so_testing <- testing(so_split)

## using country, years_coded_job and open_source

remote_model1 <- glm(remote2~country+years_coded_job, data = so_training,
                     family = binomial(link = "logit"))

so_testing$model_pred <- predict(remote_model1, newdata = so_testing, 
                                 type = "response")

hist(so_testing$model_pred)

ggplot(data = so_testing,
       aes(x = remote, y = model_pred)) + 
  geom_boxplot()

### Now to do exercise with balanced training data

so_remote <- so |> 
  filter(remote == "Remote")
so_notremote <- so |> 
  filter(remote == "Not remote")

## going to 1) take 50% of remote to use in training data and 
## 2) take a comparable number from not remote

remote_split <- initial_split(so_remote, prop = 0.5)
remote_training <- training(remote_split)
remote_testing <- testing(remote_split)


### now take a split of the not remote
notremote_split <- initial_split(so_notremote, prop = 0.06)
notremote_training <- training(notremote_split)
notremote_testing <- testing(notremote_split)

new_training <- bind_rows(remote_training, notremote_training)
new_testing <- bind_rows(remote_testing, notremote_testing)

remote_model2 <- glm(remote2~country+years_coded_job+open_source,
                     data = new_training)

new_testing$pred_model <- predict(remote_model2, 
                                  newdata = new_testing)

hist(new_testing$pred_model)

ggplot(data = new_testing,
       aes(x = remote, y = pred_model)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 0.5, linetype = "dashed")

ggplot(data = new_testing,
       aes(x = pred_model)) + 
  geom_histogram() + 
  facet_wrap(~remote, ncol = 1, 
             scales = "free_y")



