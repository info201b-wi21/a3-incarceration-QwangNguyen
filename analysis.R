#This is the file where the code for assignment 3 will go.
library(tidyverse)
library(dplyr)

incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

current_pop <- incarceration_df %>%
  filter(year == max(year)) %>%
  summarise(year = max(year),
            jail = sum(total_jail_pop, na.rm = TRUE), 
            prison = sum(total_jail_from_prison, na.rm = TRUE), 
            population = sum(total_pop, na.rm = TRUE))

data_description <- list(nrow(incarceration_df), range(incarceration_df$year, na.rm = TRUE), current_pop)
names(data_description) <- c("Number of observations", "Range of Years", "Most Recent Pop Totals")

