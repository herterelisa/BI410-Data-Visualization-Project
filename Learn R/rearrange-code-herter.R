################################################################################
### TRANSFORM: Rearranging data                                              ### 
################################################################################

## OBJECTIVES:
## To learn how manipulate data into a form useable for analysis and graphs.
## To do this in a way that each step is traceable and reproducible.
## To this end we'll be using the dplyr package.

## dplyr is in the tidyverse:
library(tidyverse)

########################
##1) Reading in the data
########################

## We will use a dataset of water temperature in Calispell Creek and its tributories  from eastern Washington State.
## These type of data are ripe for for scripted analysis because their formats remain constant 
## but graphs frequently need to be updated to reflect new data.

## Remember to set your working directory to where the file is!!! 

rawdat <- read.csv("CalispellCreekandTributaryTemperatures.csv", stringsAsFactors = FALSE) 

## QUESTION TO PONDER (EXTRA): What does stringsAsFactors mean? Why would we want to make it false?
#Answer: We set stringsAsFactors = FALSE so text like dates and times stay as plain text, so its  easier to clean and work with later.

## Let's assign more useable column names
names(rawdat) <- c("date", "time", "calispell_temp", "smalle_temp", "winchester_temp")


#################################
## 2) dplyr tool number 0: the tibble
#################################


## The first step of working with data in dplyr is to load the data in what the package authors call
## a 'tibble'
## Use this code to create a new tibble called wtemp.
## Tibbles are similar to data frames but with some useful features: https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html
wtemp <- as_tibble(rawdat)

## One of the best features is the printing
## Let’s see what is meant by this 
wtemp

## REVIEW QUESTION AND PLAY (EXTRA): What class is wtemp? How many rows does wtemp have? How many columns?
# wtemp is a tibble with 61,100 rows and 5 columns.

dim(wtemp)

## To reinforce how nice this is, print rawdat instead:
rawdat

## Ophf! To never see that again, let's remove rawdat from the workspace
rm(rawdat)

## Another way to get a tibble when you upload is to use the readr package, also in the tidyverse
rawdat_alt <- read_csv("CalispellCreekandTributaryTemperatures.csv") 

# EXTRA QUESTION TO PONDER: why did we not need stringsAsFactors for this? 
#Answer: read_csv() automatically keeps text columns as characters instead of turning them into factors.
# the earlier function rawdat needed it, the newer tidyverse function rawdat_alt does not.


#################################
## 3) dplyr tool number 1: select
#################################

## Let's imagine that we are only intested in the temperature at the Calispell site
## select helps us to reduce the dataframe to just columns of interesting
select(wtemp, calispell_temp, date, time)

## QUESTION: Are the columns in the same order as wtemp?
#            No, the columns are not in the same order as in wtemp. In wtemp, the columns are date, time,
#            calispell_temp, smalle_temp, and winchester_temp. In the selected tibble, the columns have been 
#            reordered and only three are kept: calispell_temp, date, and time.

## NOTE: We didn't have to type wtemp$date etc as we would outside of the tidyverse
## the select() function knows we are referring to wtemp.

## Recall that in R, the : operator is a compact way to create a sequence of numbers. For example:
5:20

## Normally this notation is just for numbers, but select() allows you to specify a sequence of columns this way.
## This can save a bunch of typing! 

## TASK: Select date, time and calispell_temp using this notation
select(wtemp, date:calispell_temp)
## Print the entire tibble again, to remember what it looks like.
## We can also specify the columns that we want to discard. Let's remove smalle_temp, winchester_temp that way:
select(wtemp, -smalle_temp, -winchester_temp)

## TASK: Get that result a third way, by removing all columns from smalle_temp:winchester_temp.
## Be careful! select(wtemp, -smalle_temp:winchester_temp) doesn't do it...
select(wtemp, -(smalle_temp:winchester_temp))

#################################
## 3) dplyr tool number 2: filter
#################################

#Now that you know how to select a subset of columns using select(), 
#a natural next question is “How do I select a subset of rows?” 
#That’s where the filter() function comes in.

## I might be worried about high water temperatures. 
## Let's filter the the dataframe table to only include data with temperature equal or greater than 15 C
filter(wtemp, calispell_temp >= 15)

## QUESTION: How many rows match this condition?
#Answer: There are 7,703 rows where the Calispell Creek water temperature is greater than 15 °C.

## We can also filter based on multiple conditions. 
## For example, did the water get hot on the 4th of July, 2013? I want both conditions to be true:
filter(wtemp, calispell_temp >= 15, date == "7/4/13")

##And I can filter based on "or" - if any condition is true. 
## For example, was water temp >=15 at any site?
filter(wtemp, calispell_temp >= 15 | smalle_temp >= 15 | winchester_temp >= 15)


##QUESTION: How many rows match this condition? For the 7/4 filter only 12 match the condition. 
# Answer: There are 8,093 rows where the water temperature is ≥ 15 °C at at least one of the three sites (Calispell, Smalle, or Winchester).

## Finally, we might want to only get the row which do not have missing data
## We can detect missing values with the is.na() function
## Try it out:
is.na(c(3,5, NA, 6))

## Now put an exclamation point (!) before is.na() to change all of the TRUEs to FALSEs and FALSEs to TRUEs
## This tells us what is NOT NA:
!is.na(c(3,5, NA, 6))


## NOTE: To see all possible unique values in a column, use the unique function:
unique(wtemp$calispell_temp)

## TASK: Time to put this all together. Please filter all of the rows of wtemp 
## for which the value of calispell_temp is not NA.
filter(wtemp, !is.na(calispell_temp))
## How many rows match this condition?
nrow(filter(wtemp, !is.na(calispell_temp)))
# Answer: There are 52,330 rows

## TASK: Please filter all the values of calispell_temp where the temp is greater or equal to 15, or is na
filter(wtemp, calispell_temp >= 15 | is.na(calispell_temp))
# there are  16,473 rows that maatch this condition
##################################
## 4) dplyr tool number 3: arrange
##################################

## Sometimes we want to order the rows of a dataset according to the values of a particular variable
## For example, let's order the dataframe by calispell_temp 
arrange(wtemp, calispell_temp)

## QUESTION: What is the lowest temperature observed in Calispell Creek?
# aNSWER: The lowest temperature observed in Calispell Creek is -0.28 °C.

## But wait! We're more worried about high temperatures.
## To do the same, but in descending order, you have two options. 
arrange(wtemp, -calispell_temp)
arrange(wtemp, desc(calispell_temp))

## And you can arrange by multiple variables. 
## TASK: arrange the tibble by date (ascending) and smalle_temp (descending)
arrange(wtemp, date, desc(smalle_temp))
## TASK: How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(wtemp, is.na(calispell_temp))

#aNSWER: `is.na(calispell_temp)` is TRUE for missing values and FALSE for values that exist, so using it with `arrange()` puts all the missing temperatures at the top of the table.

##################################
## 5) dplyr tool number 4: mutate
##################################

## It’s common to create a new variable based on the value of one or more variables already in a dataset. 
## The mutate() function does exactly this.

## I like that the data are all in C. But what if we want to talk to an "I'm not a scientist" politician about water temperature?
## We might want to convert it to F.
mutate(wtemp, calispell_temp_F = calispell_temp*9/5 + 32)

## To make our data more usable, we also might want to summarize data across time, or by month and year.
## The lubridate package helps a lot with this! Here is just a taste, but if you need to work with dates for your project check out the package.
## There is also a great swirl tutorial on how to use it.
## Let's load lubridate:
library(lubridate)

## TASK: Look at the lubridate help page. What do the functions with 'y' 'm' and 'd' (in various orders) do?  
# aNSWER: It turn text into dates and uses the letters y, m, d to know which is year, month, and day.

?lubridate

## Try it out:
mdy("1/13/09")

## Once dates are saved as date-time objects, we can extract information from them. Try it out.
## First, let's save the character string as a date-time object:
mydate <- mdy("1/13/09")

## Then extract the month and day:
month(mydate)
day(mydate)

##QUESTION: How would you extract the year from mydate?
year(mydate)
# Answer: This returns the year 2009 from the date-time object mydate.

## Let's use the mutate and mdy functions to create a variable called date2 that stores the date as a date-time object.
mutate(wtemp, date2 = mdy(date))

## Finally, we can use mutate to create several columns. For example, let's create date2, then create a column for month and year
mutate(wtemp, date2 = mdy(date), month = month(date2), year = year(date2))

## Let's go ahead and save those changes in an object called wtemp2 object:
wtemp2 <- mutate(wtemp, date2 = mdy(date), month = month(date2), year = year(date2))


## TASKS (definitely do these!): There are a variety of useful creation functions. Using the documentation in 5.5, please:
## 1) Create a column that is the ranked values of calispell_temp
wtemp2 <- mutate(wtemp2, calispell_rank = rank(calispell_temp))

## 2) Create a column that is the mean value of calispell_temp (hint: you might need to add na.rm = T)
wtemp2 <- mutate(wtemp2, calispell_mean = mean(calispell_temp, na.rm = TRUE))


## 3) Create a column that is the amount that calispell_temp deviates from its mean
wtemp2 <- mutate(wtemp2, deviation = calispell_temp - mean(calispell_temp, na.rm = TRUE))


## 4) Create a column that is the log of smalle_temp
wtemp2 <- mutate(wtemp2, log_smalle = log(smalle_temp))

## 5) Create a column that is the difference in temperature between smalle and winchester
wtemp2 <- mutate(wtemp2, diff_smalle_winchester = smalle_temp - winchester_temp)


## TASK: Name two other creation functions and give a scenario in which you would use them

#sd(): tells you how much the temperature fluctuates around the average, which tells you how stable or variable the creek’s environment is.
wtemp2 <- mutate(wtemp2, calispell_sd = sd(calispell_temp, na.rm = TRUE))


# ntile() – divide into groups (example: Split water temperatures)
wtemp2 <- mutate(wtemp2, temp_quartile = ntile(calispell_temp, 4))

