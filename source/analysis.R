library(tidyverse)
library(ggplot2)
library(dplyr)
library(usmap)
library(leaflet)
# The functions might be useful for A4
source("../source/a4-helpers.R")
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)
## Section 2  ---- 
#----------------------------------------------------------------------------#
# Average female jails across the nation
get_average_female_jail_pop <- function() {
  average_female_jail_pop <- data %>%
    summarise(female_jail_pop == mean(female_jail_pop, na.rm = TRUE)) %>%
  return(female_jail_pop)
}

# Years with the most population of 15 to 64
year_max_total_pop_15_to_64 <- data %>%
  filter(total_pop_15to64 == max(total_pop_15to64, na.rm = TRUE)) %>%
  pull(year)

# Average male jail population in the state of Washington in 2001
wa_average_jail_pop <- data %>%
  filter(state == "WA")%>%
  filter(year == 2001) %>%
  summarise(male_jail_pop = sum(male_jail_pop, na.rm = TRUE) / n()) %>%
  pull(male_jail_pop)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
get_year_jail_pop <- function() {
  year_jail_pop <- data %>%
    group_by(year) %>%
    summarise(year_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    select(year, year_jail_pop)
  return(year_jail_pop)
}

year_jail_pop <- get_year_jail_pop()

plot_jail_pop_for_us <- function()  {
  jail_pop_chart <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = year_jail_pop)) +
    labs(x = "Year", y = "Population of Jails", title = "Growth of Prison Population from 1970-2018", 
         caption = "Growth of Prison population")
  return(jail_pop_chart)   
} 

jail_pop_chart <- plot_jail_pop_for_us()
jail_pop_chart

#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
get_jail_pop_by_states <- function(states) {
  growth_by_state <- data %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(year_jail_pop = sum(total_pop, na.rm = TRUE))
  return(growth_by_state)
}

state_vector <- c("WA", "ID", "MT")

plot_jail_pop_by_states <- function(states) {
  jail_pop_chart <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = year_jail_pop, color = state)) +
    labs(x = "Year", y = "Population of Jails", title = "Growth of Jail Population", 
    caption = "Growth of Jail Population from 1970-2018 in Washington, Idaho and Montana")
  return(jail_pop_chart)
}

plot_jail_pop_by_states(state_vector)

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Incarceration: Patterns of Inequality
inequality_graph <- function() {
  inequality_graph <- data %>%
    select(year, black_jail_pop, white_jail_pop) %>%
    replace(is.na(.), 0) %>%
    mutate(black_sum = black_jail_pop, white_sum = white_jail_pop) %>%
    group_by(year) %>%
    summarise(black_sum = sum(black_sum), white_sum = sum(white_sum))
  return (inequality_graph)
}

race_incarceration_plot <- function() {
  race_diff_graph <- ggplot(inequality_graph()) + 
    geom_line(mapping = aes(x = year, y = black_sum, color = "black")) +
    geom_line(mapping = aes(x = year, y = white_sum, color = "white"))
    labs(x = "Year", y = "Jail population", title = "Difference in Growth of Black and White Jail Pop")
  return(race_diff_graph)
}
race_incarceration_plot()


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>

inequality_geo <- data %>%
  select(state, year, native_jail_pop, total_jail_pop) %>%
  filter(year == 2010) %>%
  mutate(native_rate = native_jail_pop / total_jail_pop) %>%
  mutate(native_rate = native_jail_pop * 100) 

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(inequality_geo)

inequality_geo_map <- function() {
  inequality_map <- ggplot(state_shape) + 
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = native_rate),
      color = "black",
      size = 0.1
    ) + 
    coord_map() + 
    scale_fill_continuous(na.value = "grey", low = "white", high = "red") + 
    labs(fill = "Native Population in 2010") + 
    ggtitle("Native Jail Population Across Nation in 2010") + 
    theme_bw() + 
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank() 
    )
  return(inequality_map)
}
inequality_geo_map()

#----------------------------------------------------------------------------#