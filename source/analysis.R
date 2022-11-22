library(tidyverse)
library(ggplot2)
library(dplyr)
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
# <variable comparison that reveals potential patterns of inequality>



#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>


#----------------------------------------------------------------------------#

## Load data frame ---- 


