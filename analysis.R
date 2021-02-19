#This is the file where the code for assignment 3 will go.
library(tidyverse)
library(dplyr)

incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

################################################################################################################
##Part 1########################################################################################################

current_pop <- incarceration_df %>%
  drop_na(total_prison_pop) %>%
  filter(year == max(year)) %>%
  summarise(year = max(year),
            jail = sum(total_jail_pop, na.rm = TRUE), 
            prison = sum(total_prison_pop, na.rm = TRUE), 
            population = sum(total_pop, na.rm = TRUE))

data_description <- list(nrow(incarceration_df), range(incarceration_df$year, na.rm = TRUE), current_pop)
names(data_description) <- c("Number of observations", "Range of Years", "Most Recent Pop Totals")

################################################################################################################
##Part 2########################################################################################################

incarceration_time_series_df <- incarceration_df %>%
  drop_na(total_prison_pop) %>%
  group_by(year) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(Total = total_jail_pop + total_prison_pop, 
         White = white_jail_pop + white_prison_pop,
         Black = black_jail_pop + black_prison_pop,
         Latinx = latinx_jail_pop + latinx_prison_pop,
         AAPI = aapi_jail_pop + aapi_prison_pop,
         Native = native_jail_pop + native_prison_pop)%>%
  summarise(across(Total : Native, sum)) %>%
  pivot_longer(!year, names_to = "race", values_to = "value")

incarceration_over_time_plot <- 
  ggplot(incarceration_time_series_df, mapping = aes(year, value, colour = race)) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "Incarcerated Population Over Time",
    x = "Year",
    y = "Population",
    color = "Population Types")
 
################################################################################################################
##Part 3########################################################################################################

top_10_black_incarceration_states_df <- incarceration_df %>%
  filter(year == 2016) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(state) %>%
  summarise(black_incarceration_rate = (sum(black_jail_pop, black_prison_pop) / sum(black_pop_15to64)) * 100,
         total_incarcerated_rate = (sum(total_jail_pop + total_prison_pop) / sum(total_pop_15to64)) * 100, 
         .groups = "drop") %>%
  slice_max(black_incarceration_rate, n = 10) %>%
  pivot_longer(!state, names_to = "name", values_to = "value")

top_10_black_incarceration_plot <-
  ggplot(top_10_black_incarceration_states_df, aes(x = value, y = factor(state, levels = rev(unique(state))))) +
  geom_col(
    aes(fill = name),
    position = position_dodge2(reverse = TRUE)
  ) +
  scale_fill_manual(name = "", labels=c("Black", "Total"), values=c("Black", "firebrick4")) +
  labs(title = "States with Highest Rate of Black Incarceration",
      x = "Percent Incarcerated",
      y = "State"
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1))
  
################################################################################################################
##Part 4########################################################################################################
  

  
