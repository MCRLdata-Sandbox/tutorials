## This tutorial assumes you have R/RStudio installed, and understand the basics
## If not, please check out the tutorial before this in the scripts folder!
##
## This script is a tutorial for with L1 data created and stored in the data_prep
## repo. We will use the tide gauge  and data, demonstrating how to pull data 
## directly from Github into R, merge data, and start exploring with a simple plot 
##
## 2025-07-07
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## The first step is setting up your environment. First, let's run through some
## useful setup steps to make it easier to interact with the data we want to look
## at. 

## Clean your environment - this step removes everything, so R is starting fresh
## with nothing in its memory
rm(list = ls())

## Load packages we may want to use
require(pacman)
p_load(tidyverse, # keep your data workflows tidy
       ncdf4, # interact with netCDF files
       parsedate, # robust handling of datetimes
       cowplot, # arrange multiple plots
       janitor,  # clean_names()
       hms) # as_hms()

## Set ggplot theme - this is personal preference, you can find your own favorite
## theme at https://ggplot2.tidyverse.org/reference/ggtheme.html
theme_set(theme_bw())

# A function is a reusable block of code that performs a specific task.
# Functions allow you to automate repetitive tasks and organize your code more effectively.
# To create a function, use the `function` keyword and define input arguments within parentheses.
# The body of the function contains the operations, and the `return()` function is used to specify output.
# Here's a basic example of how to write a function in R:

# Example: A function to add two numbers
# add_numbers <- function(x, y) {
#   sum <- x + y       # Perform the addition
#   return(sum)        # Return the result
# }

# You can call this function like so:
# result <- add_numbers(3, 5)
# print(result)   # Output: 8

# Functions make your code modular, easy to debug, and reusable!
## Since seasons and seasonality are one of the more important things we want to
## examine in many of our datasets, let's write a function to help us with this: 
assign_season <- function(data){
  data %>% 
    mutate(date = as_date(time_pst)) %>% 
    mutate(month = month(date, label = T), 
           month_num = month(date, label = F)) %>% 
    mutate(season = case_when(month_num %in% c(3, 4, 5) ~ "1. Spring", 
                              month_num %in% c(6, 7, 8) ~ "2. Summer", 
                              month_num %in% c(9, 10, 11) ~ "3. Fall", 
                              month_num %in% c(12, 1, 2) ~ "4. Winter"))
}

## These steps are based on https://github.com/MCRLdata-Sandbox/data_prep/blob/main/scripts/0_setup.R
## and there's an easier way to do all this, so that you're not typing the code 
## above into every single script, which would get old! We can use the raw code
## that's stored on Github to do this:

## Set up environment - read and run 0_setup.R
source("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/scripts/0_setup.R")

## So, in summary, you can write all the lines of code between 20 and 65, or you
## can just run line 73! 


# 2. Read L1 data from Github --------------------------------------------------

## We can do something really cool: instead of downloading a dataset then reading
## from your local download location, you can read that data directly from Github.
## As an example, let's pull in water level from the tide gauge and velocity
## from the ADCP: 

## Read in data - note: you MUST read in the raw link (Click "Raw" button when 
## viewing a dataset on Github) 
tide_gauge <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250515_tidegauge_L1.csv")
adcp <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250703_adcp_velocity_L1.csv")

## First, let's look at each dataset. Prior to joining them, we want to make sure
## that the column we are using to join (time_pst) matches. The scripts that make
## L1 data are careful to convert to a consistent timezone and round timestamps
## to a consistent interval. These look good.
tide_gauge
adcp

## One quick note on timestamps in R. R will assume by default that datetime data
## are in UTC. However, if you make a timestamp, it will assign the timezone of your
## computer. As a quick example: 

## When we pull the first value from time_st, we see "UTC" at the end. As a quick
## note, these times are in PST (set when creating L1 data). R doesn't know that,
## so it assumes it is in UTC. Since it is making the same assumption for all data
## we read in, and all data are set to PST by L1 processing, that's okay. However,
## if you are workin with datasets in different timezones, it is good practice to
## set your timezone when you read data in. force_tz() and with_tz() are useful
## lubridate functions for this.
time1 <- tide_gauge$time_pst[[1]]

## If you make a timestamp and don't define the timezone, it will assume your
## local timezone is correct. 
time2 <- as_datetime("2021-09-01 06:05:00")
time2

## We can see these aren't equivalent...
time1 == time2

## If we wanted to change that, we could define our timezone
time3 <- as_datetime("2021-09-01 06:05:00", tz = "UTC")
time3

## Now they match!
time1 == time3


# 3. Join ADCP and tide gauge data into a single dataset -----------------------

## I want to make a plot that includes information from both tide_gauge and 
## adcp datasets. The way I prefer to do this is using *_join commands. There are
## several: inner_join only includes rows that are present in the two datasets, 
## full_join includes all rows, left_join includes all rows in the first dataset
## and only ones in the second dataset that match the first dataset, and right_join
## does the opposite. There are other options too, including semi_join and anti_join.
## I only want rows that include information on water level and velocity, so let's 
## use inner_join. The important argument is what column to join by (by = )

## Join datasets
df <- inner_join(tide_gauge, 
                 adcp, 
                 by = "time_pst") %>% 
  dplyr::select(-contains("tide")) # I'm not going to use the two columns called tide & tide2

## Let's check how that merger changed our dataset
nrow(tide_gauge)
nrow(adcp)
nrow(df)

## We can see that adcp and df have the same number of rows, while tide gauge
## lost more than two thirds of its rows

## Let's visualize this: It looks like there are some periods missing WL, but 
## more often there appear to be little gaps. That's interesting, and potentially
## something we should look into if we were wanting to use these data further 
ggplot() + 
  geom_line(data = tide_gauge, aes(time_pst, water_level_m_navd88), color = "gray") + 
  geom_line(data = df, aes(time_pst, water_level_m_navd88), color = "blue", alpha = 0.2)

## For now, we'll stick with basics, and create a plot. Let's say we want to look
## at how velocities relate to tidal water levels. First, let's pick a period
## with good data. It looks like mid-2024 is relatively clean. First, let's subset
## the chunk we'd like to use and look at it. 

## Let's pick a random month:
start_date = as_datetime("2024-02-01", tz = "UTC")

df_subset <- df %>% 
  filter(time_pst > start_date & 
           time_pst < start_date + months(1))

## This is our initial plot. It's not super informative, and it's not pretty
ggplot(df_subset, aes(time_pst, y = water_level_m_navd88, color = max_velocity_m_s)) + 
  geom_line()

## Let's edit our axes and change our color scheme. It looks better but doesn't
## really give us a sense for the relationship between velocity and tides
ggplot(df_subset, aes(time_pst, 
                      y = water_level_m_navd88, 
                      color = max_velocity_m_s)) + 
  geom_line() + 
  labs(x = "Datetime", 
       y = "Water level (NAVD88)", 
       color = "Velocity \n (m/s)") + 
  scale_color_viridis_c(option = "a")

## Let's zoom in a bit more to see if that helps, and flip our color scheme
df_subset %>% 
  filter(time_pst > start_date + weeks(2) & 
           time_pst < start_date + weeks(3)) %>% 
  ggplot(aes(time_pst, 
                      y = water_level_m_navd88, 
                      color = max_velocity_m_s)) + 
  geom_line(lwd = 1) + 
  labs(x = "Datetime", 
       y = "Water level (NAVD88)", 
       color = "Velocity \n (m/s)") + 
  scale_color_viridis_c()














