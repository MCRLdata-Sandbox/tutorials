## This tutorial assumes you have R/RStudio installed, and understand the basics
## If not, please check out the tutorial before this in the scripts folder!
##
## This script is a tutorial for with L1 data created and stored in the data_prep
## repo. We will use the tide gauge data, demonstrating how to pull data directly
## from Github into R. 
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
## 






