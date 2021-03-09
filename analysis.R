#This is the file where the code for assignment 3 will go.
library(tidyverse)
library(dplyr)
library(maps)

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
current_pop <-as.list(current_pop)

data_description <- list(nrow(incarceration_df), range(incarceration_df$year, na.rm = TRUE), current_pop)
names(data_description) <- c("Number_of_observations", "Range_of_Years", "Most_Recent_Pop_Totals")

################################################################################################################
##Part 2########################################################################################################

incarceration_time_series_df <- incarceration_df %>%
  drop_na(total_prison_pop) %>%
  group_by(year) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  drop_na(total_prison_pop) %>%
  mutate(Total = total_jail_pop + total_prison_pop, 
         White = white_jail_pop + white_prison_pop,
         Black = black_jail_pop + black_prison_pop,
         Latinx = latinx_jail_pop + latinx_prison_pop,
         AAPI = aapi_jail_pop + aapi_prison_pop,
         Native = native_jail_pop + native_prison_pop) %>%
  summarise(across(Total : Native, sum)) %>%
  pivot_longer(!year, names_to = "race", values_to = "value") %>%
  filter(year >= 1980)

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

racial_incarceration_rates_df <- incarceration_df %>%
  filter(year == 2016) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  filter(str_detect(state, "WA")) %>%
  mutate(black_incarceration = (black_jail_pop + black_prison_pop) / black_pop_15to64,
         white_incarceration = (white_jail_pop + white_prison_pop) / white_pop_15to64,
         incarceration_ratio = black_incarceration / white_incarceration) %>%
  select(fips, incarceration_ratio)
  
 
washington <- map_data("county", region = "Washington") %>%
  mutate(polyname = paste(region, subregion, sep= ","))

fips <- filter(county.fips, str_detect(polyname, "washington,")) %>%
  add_row(fips = 53055, polyname = "washington,san juan") %>%
  add_row(fips = 53053, polyname = "washington,pierce")
  
washington <- left_join(washington, fips, by = "polyname") 
washington <- left_join(washington, racial_incarceration_rates_df, by = "fips")

black_white_ratio_plot <-
  ggplot(data = washington) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = incarceration_ratio), color = "gray") +
  coord_quickmap() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  labs(title = "Discrepancies between racial incarceration rates in WA",
       x = "",
       y = "",
       fill = "ratio of black:white",
       caption = "Displays the ratio of black incarceration rate to white incarceration rate. A ration of 2.0
       means that black people are twice as likely to be incarcerated as white people"
  ) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank())

################################################################################################################
##Part 5########################################################################################################

white_asian_incarceration <- incarceration_df %>%
  filter(year == 2016) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  filter(str_detect(state, "CA")) %>%
  mutate(aapi_incarceration = (aapi_jail_pop + aapi_prison_pop) / aapi_pop_15to64,
         white_incarceration = (white_jail_pop + white_prison_pop) / white_pop_15to64,
         incarceration_ratio = aapi_incarceration / white_incarceration) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  select(fips, incarceration_ratio)


california <- map_data("county", region = "california") %>%
  mutate(polyname = paste(region, subregion, sep= ","))

fips_california <- filter(county.fips, str_detect(polyname, "california,"))

california <- left_join(california, fips_california, by = "polyname") 
california <- left_join(california, white_asian_incarceration, by = "fips")

AAPI_white_ratio_map <-
  ggplot(data = california) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = incarceration_ratio), color = "gray") +
  coord_quickmap() +
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "Discrepancies between racial incarceration rates in CA",
       x = "",
       y = "",
       fill = "ratio of AAPI:white",
       caption = "Displays the ratio of AAPI incarceration rate to white incarceration rate. A ration of 2.0
       means that AAPI people are twice as likely to be incarcerated as white people"
  ) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank())





  
