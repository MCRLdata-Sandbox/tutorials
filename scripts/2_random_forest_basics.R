## This script provides several examples of constructing Random Forests, from 
## simple to more complex. This is intended to be a low-level practical introduction
## to start you on your ML journey!
##
## I strongly suggest checking around for basic information on Random Forests to
## understand the principles behind it before running through this script. A personal
## favorite of mine is StatQuest: https://www.youtube.com/watch?v=J4Wdy0Wc_xQ
##
## If you are new to R, please run through 0_R_basics.R before using this script
##
## 2025-05-29
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## First, load all the packages you will need using pacman
require(pacman)

p_load(tidyverse, ## Keep things tidy
       tidymodels, ## An extension of the tidyverse for modeling
       janitor, ## clean_names()
       pdp, ## partial dependency plots
       ranger)

## set ggplot theme - I like theme_bw, feel free to pick your favorite!
## examples near bottom: https://ggplot2.tidyverse.org/reference/ggtheme.html
theme_set(theme_bw())


# 2. Load dataset --------------------------------------------------------------

## For the first model, we'll use a simple dataset built into R. Let's convert
## it to a tibble, which is the dataframe style preferred by tidyverse
df <- as_tibble(iris) %>% 
  clean_names() ## I like this function to make it easier to write column names

## Let's look at this dataset: we have 4 continuous variables and one factor
df


# 3. My first random forest ----------------------------------------------------

## Random Forests are a machine learning technique that can be thought of as somewhat 
## similar to a linear regression model. In a linear regression model, you use
## one variable to predict another via a function, i.e., your y = mx + b from algebra.
## A Random Forest model can do the exact same thing, but because it is designed 
## using randomness, and isn't bounded by the same assumptions as a linear regression
## model, it can find more complex, and potentially better-fitting relationships

## Let's build a linear regression model, and then a Random Forest model and 
## compare them

## Linear regression models can be built using the lm() function in R. A quick
## reminder that if you ever want to know more about a function, you can type it 
## in the console preceded by a ?: 
?lm()

## Let's start with the relationships between petal length and sepal length. 
## We can fit a linear regression line, which does a pretty good job here, and
## this is a good initial indicator of what you expect: the slope looks close to 
## 0.5, and the intercept may be somewhere near 4.
ggplot(df, aes(petal_length, sepal_length)) + 
  geom_point() + 
  geom_smooth(method = "lm")

## If we want to fit a y = mx + b linear model, where we use petal_length as x 
## to predict sepal_length as y, we use this formula. This gives us two pieces
## of information: the call, or formula, and then the coefficients. These are our
## slope and y-intercept, which R labels somewhat confusingly. We can see that 
## these match our guesses above: the intercept is 4.3 and the slope is 0.4, 
## pretty close to 4 and 0.5 that I guessed based on the plot.
lm(sepal_length~petal_length, data = df)

## It's important to also know that there is a lot more information below the
## surface here. If we save the model, we can interrogate that object to get
## out other things we might be interested in: 
linear_model <- lm(sepal_length~petal_length, data = df)

## You can access these directly using a $ like this (here's how to pull the slope)
linear_model$coefficients[[1]]

## You can also use the summary function
summary(linear_model)

## This is really useful for pulling R2 values, for instance
summary(linear_model)[[9]]

## and if you wanted to save these values for use later, you can do that too
lm_r2 = round(summary(linear_model)[[9]], 2)


## Now that we understand how to use one variable to predict another (and please
## note this is NOT all that's involved, a proper model requires testing assumptions
## on your dataset and other potential steps). Let's do the same thing but using
## Random Forest instead of linear regression: 

## I like the ranger package, it's an easy-to-use implementation of Random Forests
rf0 <- ranger(sepal_length~petal_length, 
              data = df, 
              importance = "impurity")

## This has some similarities to our lm call above: the model reports the call, and 
## then some information about the model. Unlike lm, the Random Forest is looking 
## for the best relationship it can find based on it's structure, and is not limited
## to a standard linear structure, so it doesn't report coefficients. It does report
## and R2, which is very similar to the linear regression model. 
rf0

## Something cool we can do with this model is approximate the relationship 
## between these variables. partial() relabels our dependent variable (sepal_length) 
##to yhat, so let's rename that. 
pdp0 <- partial(rf0, pred.var = "petal_length", train = df) %>% 
  rename("sepal_length_rf" = yhat)

## Let's format and plot: 
ggplot(pdp0, aes(petal_length, sepal_length_rf)) + 
  geom_path() + 
  geom_smooth()

## Now let's combine these two ways of understanding the relationship between
## these two variables and compare. 
ggplot() + 
  geom_point(data = df, aes(x = petal_length, y = sepal_length), color = "gray") + 
  geom_smooth(data = df, aes(x = petal_length, y = sepal_length),
                  method = "lm", color = "blue", se = F) + 
  geom_smooth(data = pdp0, aes(x = petal_length, y = sepal_length_rf),
              color = "red", se = F) + 
  annotate(geom = "text", x = 3, y = 7.5, label = "Linear", color = "blue") + 
  annotate(geom = "text", x = 3, y = 7, label = "Random Forest", color = "red")
 

# 2. Multivariate RF models ----------------------------------------------------

## The true power of Random Forests are their ability to use many different 
## predictors, both categorical and continuous, to build a predictive model.
rf1 <- ranger(sepal_length~petal_length + sepal_width + petal_width, 
              data = df, 
              importance = "impurity")

## We see a small increase in the R2, which is interesting because we are giving
## the model a lot more inputs to use for predicting, so we might expect that 
## value to increase dramatically. However, it's important to note that petal_length
## clearly already contains a lot of useful information for predicting sepal_length, 
## where a simple single-variable linear regression model can explain ~75% of the 
## variance in the sepal_length dataset. So let's take a look at how important 
## each of the three input (predictor) variables are to this model. 

## Set vectors for renaming stuff
var_names <- rf1$variable.importance
col_names <- c("predictor", "raw_fi")

## Convert feature importance to a tibble with variables as a column
fi1 <- as.data.frame(var_names) %>% 
  tibble::rownames_to_column() %>% 
  as_tibble() 

## Rename columns
colnames(fi1) = col_names

## This plot gives us the percentage of the Random Forest's predictive power
## contributed by each variable. 
fi1 %>% 
  mutate(fi = raw_fi / sum(raw_fi)) %>% 
  ggplot(aes(fi * 100, 
               reorder(predictor, fi), fill = predictor)) + 
  geom_col(alpha = 0.8, show.legend = F, width = 0.7) + 
  labs(x = "Feature Importance (%)", 
       y = "", fill = "")












