#-----------------------------------------------
# Helping a Colleague 
#-----------------------------------------------
#       A colleague has called you over to their
#       desk and says they can't get this script 
#       to run. First remove any errors you find
#       in the R code, then once you get the 
#       script to run without any errors, help 
#       them interpret the linear regression model 
#       and understand the problem they are trying 
#       to solve! 
#-----------------------------------------------
#Your colleague needs to make a scatterplot 
# showing how sales of an item relates to 
# an item's visiblity in their ranking system. 
# They then want to try to predict sales with 
# a linear regression model to try to explain
# as much of the data as possible. Coach
# them through why some of their modeling
# choices might not be a good idea and 
# suggest a clearer route forward. 
#-----------------------------------------------
library(tidyverse)
library(ggplot2)

df <- read_csv("big_mart.csv",    )

View(df)

df %>%
  ggplot(aes(x = Item_Outlet_Sales, 
             y = Item_Visibility,
             color = Item_Type)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sales Predict Item Visibility",
       x = "Sales",
       y = "Item Visibility") +
  theme_minimal()

df %>%
  ggplot(mapping = aes(x = Item_Outlet_Sales,
             y = Item_Visibility,
             color = Item_Type)) + 
  geom_point() %>%
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sales Predict Item Visibility",
       x = "Sales",
       y = "Item Visibility") +
  theme_minimal() +
  facet_wrap(~Outlet_Size)


#------------------------------------------------
# Linear Regression 

predict_model <- lm(Item_Outlet_Sales ~ ., data = df)


residuals_df <- predict_model$residuals %>% as_tibble()

residuals_df %>%
  ggplot(mapping = aes(x = residuals)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Why Do I Have To Make This Plot?")

plot(predict_model)

summary(predict_model)
