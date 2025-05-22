## Everyone has their own way of coding, and their favorite ways to do things. 
## One of the great things about R is that you can code however you want, as long
## as it works! There are of course "best practices" but, in my opinion, whatever works for
## you is what is what you should do. This script is designed to introduce you to the 
## anatomy of R code, and how I like to set my scripts up.

## First, this is a comment. Comments are anything that follows a '#' character, 
## and R doesn't try to evaluate (run) comments. To run a line of code, highlight it
## and press the "Run" button in the upper right of this window. You can also type
## your command into the Console window:

## R runs on libraries, the standard way to install a library is
install.packages("ggplot2")

## I do something different, that I think is simpler, and uses the 'pacman' library

## First, install/load pacman using the 'require' function
require(pacman)

## Now, you can use the p_load function (from the pacman library) to load a library
p_load(ggplot2)

## You can also do this for multiple packages
p_load(ggplot2, dplyr)

## Those are the basics, I'm going to write a full (simple) script below to illustrate
## how I like to set things up. Like I said above, you can write code however you
## like, and there are all sorts of cool tips and tricks out there, if you ever
## get stuck, you can always ask me, but Google will be your best friend as you
## learn to code and troubleshoot, places like stackexchange have answers to 
## virtually any R code question you can think up.

# ------------------------------------------------------------------------------


## This script is an introduction to some basic R code that will be useful for 
## understanding the data you'll be collecting, and for completing the 
## research you'll present in your SULI deliverables.
## I start my codes off with a header (composed of comments) that explains what
## what this code is supposed to do. 
##
## Peter Regier 
## 2022-05-31
##
# ########## #
# ########## #

# 1. Setup environment ---------------------------------------------------------

## Load pacman
require(pacman)

## Load useful packages
p_load(tidyverse,  # Contains many different tidy packages, including ggplot2 
       lubridate, # Useful for manipulating date data
       hms) # Useful for manipulating time data

# 2. Read in some data ---------------------------------------------------------

## We'll be using datasets that are built-in to R to keep things simple. If you're
## loading data, you'll need to tell R where to find it. For this example, we'll
## be using a dataset called 'beaver1' which is the body temperature of a beaver.

## Because these data are built-in, you can call it to without any other code:
beaver1

## This is a dataframe, which is the standard way R stores data. We can check this
## by asking R what the structure of the dataset is, which gives us 'data.frame': 
str(beaver1)

## A key tool is the help section of R, which lets you look up any function. So,
## for instance, we just called the str() function. If you want to look it up, 
## just put a ? in front of it.
?str()

## I like to use tidyverse, which keeps things "tidy". The tidyverse alternative 
## to a dataframe is called a 'tibble', so let's convert beaver1 to a tibble: 
tibble(beaver1)

## See how much nicer that looks? Tibbles are great because they don't print a 
## lot of data, format the data nicely, and tell you what type each column of data
## is. For instance, all of these columns are type double (<dbl>) which means 
## it's numeric and R evaluates it like a number. Other types we'll use include 
## characters, factors and datetimes. We'll get to those later. For now, we'd like
## to save the beaver1 dataframe as a tibble. To do that, we use the '<-' operator
df <- tibble(beaver1)

## You'll see now that we've created an objected called 'df' that's a tibble holding
## our data. 
df

## You can explore your dataset in a number of ways. Some useful ones are: 

## what are the column names? 
colnames(df)

## how many rows does df have? 
nrow(df)

## how many columns does df have?
ncol(df)

## What is the data structure (we've already used this one)
str(df)

## If you want to only look at a single column, you can reference it using '$'
df$day

## You can also reference using its index, which accepts row and column position
## in the format if [row, column]. For instance, if we want the first row of the 
## the first column (day), we would use
df[1, 1]

## if we want the second row of the third column (temp), we would use
df[2, 3]


# 3. Let's clean up our dataset ------------------------------------------------

## There are some columns here we need and some we do. To select which columns 
## you want to keep, you can use select

## First, to figure out how select works and what arguments it takes, run this:
?select

## One of the most important things with tidyverse is the '%>%' operator, which 
##is called a pipe operator, because it pipes data from one function to the next

## For instance, if I wanted to select just one column from our data I would do this: 
df %>% 
  select(day)

## Or, what if you wanted to remove a single column? Just put '-' in front
df %>% 
  select(-day)

## We can create new columns using the "mutate" function. For example, let's create
## a new column called 'date_time' where we paste together two columns.
df %>% 
  mutate(date_time = paste(day, time))

## You'll note that the 'date_time' column is still a character (<chr>). That means
## R won't interpret any order. We know that day (day of year) and time (time of day)
## have specific meanings, but R doesn't know that....yet. So, let's touch on 
## some light date-time manipulation to tell R that these columns should be interpreted
## as datetimes and not just random vectors. To illustrate this point, let's create
## our first graph. I, and most of the rest of the R worlk,  use ggplot, which is 
## a package for making graphs that is loaded by the tidyverse package. ggplot is
## a little confusing at first, but once you're used to it, it's wonderful. For an
## exhaustive intro, check out https://ggplot2-book.org/introduction.html or any of
## the many other wonderful ggplot tutorials out there. Here's our first plot:

## data is the dataset we're plotting, x is the column for the x-axis, and y is the
## column for the y-axis. geom_point() plots each data-point as a point (ie dot)
ggplot(data = df, aes(x = day, y = temp)) + 
  geom_point()
  
## This graph is super uninformative because ggplot doesn't know that there are multiple
## measurements made through time (we didn't feed 'time' to ggplot). So let's do 
## that instead:
ggplot(data = df, aes(x = time, y = temp)) + 
  geom_point()

## This is a little more useful, but we know from the previous plot that we have
## data for two days. Let's add day as color to see that: 
ggplot(data = df, aes(x = time, y = temp, color = day)) + 
  geom_point()

## That's mildly more useful, but notice that, because data were collected starting
## on day 346 and continued to day 347, we're actually mis-representing the chronology
## here. To fix this, we'll need to edit our dataset. To do this, we need to save
## the combination of date and time in a format that R will understand to the dataset.

## First, the operations above haven't been changing 'df', because we haven't been
## saving them. The way to save our changes to a new object is using the '<-' operator.
## For instance, if we wanted to save the 'date_time' column we made above to a 
## dataframe called 'df1', we'd do this:
df1 <- df %>% 
  mutate(date_time = paste(day, time))

## df looks the same as it did, but df1 now has an additional column:
df
df1

## Back to making plots: let's first manipulate our 'df' dataset to include date
## and time saved in a format that R understands. Note that we're creating an 
## object 'df1' that will overwrite the previous 'df1' we just created (and date_time
## is going to disappear).
df1 <- df %>% 
  mutate(date = lubridate::as_date(day)) %>% 
  mutate(time = as_hms(time))

## Let's look at the new dataset we created. Note that there's a new 'date' column
## that's of type <date>, and we overwrote the 'time' column, which now looks different
## and is of type <time>. Because we didn't tell R what year the 'day' column data
## are from, it assumes 1970.
df1

## Next, let's do the same thing, but create a third column that combines date and time
## into a single column. Note that we can create multiple columns for a single
## mutate() call by separating with commas. 
df1 <- df %>% 
  mutate(date = lubridate::as_date(day),
         time = as_hms(time), 
         date_time = as_datetime(paste(date, time)))

## Now, there's an issue here: time was saved as hhmm, where h = hour and m = minute.
## However, if we look at date_time, we can see that datetime in df1 is wrong: 
## the as_hms() function we called interpreted our data in hhmm as mmss (s = seconds).
## To fix this, we'll need to go to help and figure out how to tell the function
## that we aren't giving it seconds. Don't worry about this right now, but we'll 
## create a function called as_time(), and use it to convert time correctly:
as_time <- function(x)
  as_hms(strptime(if_else(nchar(x) == 3, paste0("0", x), as.character(x)), "%H%M"))

## Note that I didn't know what to do with this time issue, so I started Googling 
## around and found this post which led me to this conclusion. Almost always,
## someone else has had whatever problem you run into, and the internet's already solved it: 
## https://stackoverflow.com/questions/60328880/convert-hmm-hhmm-time-column-to-timestamp-in-r

df1 <- df %>% 
  mutate(date = lubridate::as_date(day),
         time = as_time(time), 
         date_time = as_datetime(paste(date, time)))

## Now that we have a nice date_time column that R will understand, let's make a 
## plot and see if things are making more sense: 
ggplot(data = df1, aes(x = date_time, y = temp)) + 
  geom_point()
  

## Let's make a plot where we look at how temperature ('temp') and activity ('activ') relate
ggplot(data = df1, aes(x = temp, y = activ)) + 
  geom_point()

## It looks like temp is generally higher when active (duh), so let's color our
## time-series plot to visualize that: 
ggplot(data = df1, aes(x = date_time, y = temp, color = activ)) + 
  geom_point()

## Now, let's change the axis labels to be more descriptive
ggplot(data = df1, aes(x = date_time, y = temp, color = activ)) + 
  geom_point() + 
  labs(x = "Datetime", y = "Temperature (C)")

## There are all types of different things you can do with ggplot. So for instance,
## let's connect the points with a line, change the color scheme, add a 
## title, and change the plot theme: 
ggplot(data = df1, aes(x = date_time, y = temp, color = activ)) + 
  geom_line() +
  geom_point() + 
  scale_color_viridis_c() + 
  labs(x = "Datetime", y = "Temperature (C)", title = "Beaver temperature thorugh time") + 
  theme_bw()

## that starting to look nicer, let's make a couple more tweaks, including only
## applying color to the dots, and adding another layer to highlight each point
## a little more (notice the changes to aes()): 
ggplot(data = df1, aes(x = date_time, y = temp)) + 
  geom_line(color = "gray") +
  geom_point(size = 3, alpha = 0.5) +
  geom_point(aes(color = as.factor(activ)), size = 2) + 
  scale_color_manual(values = c("lightblue", "red")) + 
  labs(x = "Datetime", y = "Temperature (C)", 
       color = "Activity",
       title = "Beaver temperature thorugh time") + 
  theme_bw()



