# Basic data cleaning and visualization packages
library(tidyverse)
library(ggplot2)
# Package for multinomial logistic regression
library(nnet)
# To transpose output results
library(broom)
# To create html tables
library(knitr)
library(kableExtra)
# To exponentiate results to get RRR
library(gtsummary)
# To plot predicted values
library(ggeffects)
# To get average marginal effect
library(marginaleffects)
# For the Wald Test
library(car)
# Read excel
library(readxl)
library(readxl)


trips_per_user <- read_excel("C:/Users/jeanm/Desktop/dataset_trip_per_user.xlsx")
trips_per_user <- trips_per_user %>%
  mutate(
    prefered_mode = factor(prefered_mode,
                           labels = c('Car', 'Bike', 'Foot'),
                           levels = c('Bike', 'Car', 'Foot')),
    
    prefered_day = factor(prefered_day, 
                          labels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'),
                          levels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')),
    
    Gender = factor(Gender,
                    labels = c('Male', 'Female')),
    
    City_density = factor(City_density,
                          labels = c('High_density', 'Low_density', 'Medium_density'),
                          levels = c('Low_density', 'Medium_density', 'High_density')),
    
    House_composition = factor(House_composition,
                               labels = c('With_young_children', 'Adult_household', 'With_adolescent_children', 'Single')),
    
    Education_level = factor(Education_level,
                             labels = c('Bachelor_education_or_above', 'High_school', 'Middle_school_or_below', 'Unknow'),
                             levels = c('Middle_school_or_below', 'High_school', 'Bachelor_education_or_above', 'Unknow')),
    
    Profession = factor(Profession,
                        labels = c('Employed', 'Retired', 'Entrepreneur', 'Incapacitated', 'At_home', 'Unemployed', 'Governmental', 'Studying', 'Unknow')),
    
    Car_ownership = factor(Car_ownership,
                           labels = c('Yes', 'No', 'Unknwon')),
    
    Anual_income = factor(Anual_income,
                          labels = c('High_income', 'Modal', 'Low_income', 'Unknown'))
  )


multinom <- multinom(prefered_mode ~ weighted_avg_distance + weighted_avg_shopping_time + Age + Household_size + prefered_day + Gender + City_density + House_composition + Education_level + Profession + Car_ownership + Anual_income, data = trips_per_user)

pprob_pct_ctry <- ggeffect(fit_full, terms = "Car_ownership")




# Plotting marginal effects of independent variables
ggplot(data = pprob_pct_ctry, aes(x = x, y = predicted,
                                  color = response.level, group = response.level)) +
  # Add line layer
  geom_line() +
  # Add point layer
  geom_point() +
  # Add confidence intervals as error bars
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high,
                    color = response.level,
                    group = response.level),
                width = 0.5) +
  # Change the color of the lines, remove legend title, change the
  # labels of the lines in the legend.
  scale_color_brewer(palette = "Dark2",
                     name = "",
                     labels = c("Car",
                                "Bike",
                                "Foot")) +
  # Add titles to the x and y axis
  labs(
    x = "Car ownership",
    y = "Probability"
  ) +
  # Set the theme
  theme_minimal() +
  theme(
    legend.position = "bottom", # move legend to the bottom
    axis.title = element_text(size = 14) # increase axis title size
  )



