################################################################################
### Visualization: Linking data to geometric objects in ggplot2              ### 
################################################################################

## OBJECTIVE:
## To practice a "grammar of graphics"
## To become familiar with different types of graphs
## To determine the best graph to fit the question and data
## Handy reference: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#1.%20Correlation
## Another handy reference: https://r-graph-gallery.com/
## And of course the nice package documentation - geom names with little illustrations are particularly useful: https://ggplot2.tidyverse.org/reference/index.html

## First let's load the tidyverse
## This is a suite of useful packages that includes ggplot2 
library(tidyverse)


# In ggplot2, aesthetic means “something you can see”. Each aesthetic is a mapping between a visual cue and a variable. Examples include:
#   
# position (i.e., on the x and y axes)
# color (“outside” color)
# fill (“inside” color)
# shape (of points)
# line type
# size
# 
# Each type of geom accepts only a subset of all aesthetics—refer to the geom help pages to see what mappings each geom accepts. Aesthetic mappings are set with the aes() function.
## Resource on nomenclature from https://beanumber.github.io/sds192/lab-ggplot2.html


########################################
### The 8 categories of graphics in R ##
########################################

##################
### Correlation ##
##################

## Let's start by using a data set built into ggplot2
## The mpg dataset looks at the gas efficiency of different cars 
data(mpg, package = "ggplot2")

## for built-in packages, you can use the help function to get the metadata
?mpg #gas efficiency in 38 cars

## Let's use a scatterplot to relate city and highway mileage

ggplot(mpg, aes(cty, hwy)) + geom_point()

## Looks alright, but the graph may be hiding some information...
## QUESTION: How many data points are in mpg?
dim(mpg) #234 rows and 11 columns

## TASK: Try another correlation-focused geom that addresses this problem
ggplot(mpg, aes(cty, hwy)) + geom_jitter() #adding small random value to see overlapping

##################
### Deviation   ##
##################
## Used to compare variation in values between small number of items (or categories) with respect to a fixed reference.

## Whether the graphical type is a deviation graph or a composition graph (or a ranking graph) may depend on the underlying data structure

## geom_bar() is a bit tricky because it can make both a bar chart and a histogram
## the default of geom_bar is to set stat to count
## so if you give it just a continuous x value, it will make a histogram:
ggplot(mpg, aes(hwy)) + geom_bar()

## In order to have the geom create bars and not a histogram, you must:
## 1) Set stat = identity
## 2) Provide both an x and a y inside the aes(), where x is either a character or factor and y is numeric

## Let's look at which cars have above/below average fuel efficiency
data("mtcars")  # load data

## A LOT of graphing is first getting the data in the right format
## We will learn to do this later, but for now run the following: 
mtcars_dev <- mtcars %>%
  mutate(car_name = rownames(mtcars),
         mpg_z = (mpg - mean(mpg))/sd(mpg),
         mpg_type = ifelse(mpg_z < 0, "below", "above")) %>%
  arrange(mpg_z) %>%
  mutate(car_name = factor(car_name, levels = car_name))

## QUESTION: What does the column mpg_z contain? How about mpg_type? contains car_name, mpg_type contains above and below
#mpg_z contains the standardized values of MPG,  It shows how many standard deviations each car's fuel efficiency is from the mean MPG.
#mpg_type classify whether each car's MPG is above or below the data set mean based on mpg_z. 
ggplot(mtcars_dev, aes(x=car_name, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.25) + coord_flip()

## TASK: Try another deviation graph on this dataset, such as a diverging dot plot
# this ref might be helpful: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#1.%20Correlation

ggplot(mtcars_dev, aes(x = car_name, y = mpg_z)) + 
  geom_point(aes(color = mpg_type), size = 3) +
  geom_segment(aes(xend = car_name, yend = 0, color = mpg_type)) +
  coord_flip()

##################
### Ranking     ##
##################
## Used to compare the position or performance of multiple items with respect to each other. 
## Actual values matters somewhat less than the ranking.

## Let's go back to the mpg data - which manufacturers have the best average city fuel economy?

# Prepare data: group mean city mileage by manufacturer
## Again, a lot of graph creation starts with data manipulation,  but for now:
mpg_rank <- mpg %>%
  group_by(manufacturer) %>%
  summarize(avg_cty = mean(cty)) %>%
  arrange(avg_cty) %>% #lowest to hiest ranking
  mutate(manufacturer = factor(manufacturer, levels = manufacturer)) #ggplot respects the order, lock it in

# Graph it
ggplot(mpg_rank, aes(x=manufacturer, y=avg_cty)) + 
  geom_bar(stat="identity") 
  


##QUESTION: Why did we arrange and then convert to factor?
# * We arrange and convert to factor so the bars appear in order from worst to best fuel efficiency instead of alphabetically, making it easy to see which manufacturers rank highest and lowest.

# To find out, here's a version without those lines of code - try graphing it, then reflect
mpg_rank2 <- mpg %>%
  group_by(manufacturer) %>%
  summarize(avg_cty = mean(cty)) 


#######################
### Distribution     ##
#######################
## When you have lots and lots of data points and want to study where and how the data points are distributed.

## Histograms can be accomplished with either geom_bar() or geom_histogram()
## year's the distribution of car models with different highway fuel efficiency 
ggplot(mpg, aes(hwy)) + geom_histogram()

## TASK: Try making a histogram on a categorical variable, such as manufacturer
## What does this do? Counts how many car models are in the dataset from each manufacturer
## hint - if you get an error, amend your code as suggested by the error

ggplot(mpg, aes(manufacturer)) + 
  geom_histogram(stat = "count") +
  coord_flip() #flipped to manufacture names are easier to read

## TASK: Try making a boxplot comparing the distribution of cty (city mileage) within class of car
ggplot(mpg, aes(x = class, y = cty)) + 
  geom_boxplot() + coord_flip() # flip so class label is easy to read

#######################
### Composition      ##
#######################
## Bar charts and pie charts are classic ways of showing composition of a dataset
## (and you secretly made another composition as well as distribution graph in the last exercise)

ggplot(mpg, aes(x = manufacturer)) + 
  geom_bar(fill = "coral") +
  coord_flip() +
  labs(x = "Manufacturer", y = "Number of Models")

## TASK: Create a stacked bar showing the composition of car classes within each manufacturer
# Hint - move "fill" to the aes mapping and assign the class variable to it 

ggplot(mpg, aes(x = manufacturer, fill = class)) + #moved fill in aes to create stacked bar
  geom_bar() +
  coord_flip() +
  labs(x = "Manufacturer", y = "Number of Models")

## Fun extension - can show proportions instead of counts by trying position="fill" inside geom_bar
ggplot(mpg, aes(x = manufacturer, fill=class)) + 
  geom_bar(position = "fill") +
  coord_flip() +
  labs(x = "Manufacturer", y = "Number of Models")


#######################
### Change           ##
#######################
## Time series are often best visualized with line graphs
data("economics")
?economics
head(economics)

## TASK: Use the economics dataset to plot the proportion of the population
# that is unemployed over time

# calculate the unemployment proportion
economics_prop <- economics %>%
  mutate(unemploy_prop = unemploy / pop)

# Plot 
ggplot(economics_prop, aes(x = date, y = unemploy_prop)) +
  geom_point(color = "red") +
  geom_line() +
  xlab("Date") +
  ylab("Proportion Unemployed") +
  ggtitle("Unemployment Rate Over Time")

#############################
### Groups  & Spatial      ##
#############################
## Let's put a pin on these last two, ggplot2 extension packages can be useful for these.


#############################
### GRAPHS IN THE WILD    ##
#############################

# Start scoping papers related to your final project. Especially, check out their figures! 
# Find a set of figures that reflect three of the different graph types.
# What findings do they illustrate, and why did they use that type of graph? 

