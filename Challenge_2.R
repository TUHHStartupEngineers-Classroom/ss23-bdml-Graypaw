# Standard
library(tidyverse)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)
library(workflows)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

bike_orderlines_tbl <- readRDS(".\\..\\Business Decisions with Machine Learning\\bike_orderlines.rds")
bike_orderlines_tbl <- bike_orderlines_tbl %>% select(!c(total_price, quantity, order_date, order_id, order_line))

split <- initial_split(bike_orderlines_tbl, prop = 0.7)
train_data <- training(split)
test_data <- testing(split)

data_recipe <- recipe(price ~ ., data = train_data)
data_recipe <- data_recipe %>% step_scale(all_numeric_predictors())

model_linear <- linear_reg(mode = "regression") %>% 
  set_engine("lm")

workflow <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(model_linear)
workflow_fit <- workflow %>% fit(data = train_data)

to_predict <- "price"
prediction <- predict(workflow_fit, new_data = test_data)
truth <- test_data[[to_predict]]

rmse_value <- rmse_vec(truth, prediction$.pred)
print(rmse_value)