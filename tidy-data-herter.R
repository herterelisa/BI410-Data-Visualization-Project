################################################################################
### WRANGLE: Tidying Data                                                    ### 
################################################################################


## OBJECTIVES:
## To identify tidy versus untidy data
## To learn to tidy data in a way that is traceable and reproducible.
## To this end we'll be using the tidyr and dplyr packages in the tidyverse.

## What are tidy data?:
## Tidy data are data that are easy to transform and visualize. 
## The key idea is that variables are stored in a consistent way.
## Each variable forms a column
## Each observation forms a row
## Each type of observational unit forms a table

## There are five common problems associated with messy data:
## 1) Column headers are values, not variable names
## 2) Multiple variables are stored in one column
## 3) Variables are stored in both rows and columns
## 4) Multiple types of observational units are stored in the same table
## 5) A single observational unit is stored in multiple tables

## Here we will build a workflow to demonstrate a scripted data tidying
## We will use tools from dplyr and tidyr 

## Functions from dplyr:
# filter: keep rows that match a criteria
# select: pick columns by name
# arrange: reorder rows
# mutate: add new variables
# summarize: reduce variables to values
# + group_by

## Functions from tidyr: 
# pivot_longer: convert many columns into variable/value pairs; akin to melt in reshape (previous gather in tidyr)
# pivot_wider: convert variable/value pairs into columns; akin to cast in reshape (previously spread in tidyr)
# separate: break one column into two (similar to strsplit)


# Load the tidyverse
library(tidyverse)


## Read in the raw data
## Also have a look at the intial data file
rawdat <- read_csv("Niwot_Salix_2014_WillowSeedlingSurvey.csv", skip = 10)
spec(rawdat)
glimpse(rawdat)

## QUESTIONS:
## Where are the meta data for this file? 

# The metadata for this dataset is located at the very top of the CSV file, before the actual data starts. It is embedded in the first 1-10 lines and provides contextual information about the project, experimental setup, and column definitions.

## What is this format of data useful for?

# each willow seedling has its own column and different measurements (ht1, ht2, cnpy1, cnpy2) are stacked in rows.
# makes it easy to check and enter data, can quickly calcualte avg, easy to read for small datasets

## What is this format of data not useful for?

# This wide format is not useful for modern data analysis, because most tools like dplyr or ggplot2 work best with tidy data, where each row is a single observation and each column is a variable. It’s harder to filter, summarize, or plot data without first reshaping it.

##########################################
## Cleaning 1: Fill in missing variables #
##########################################

## Sometimes when a data source has primarily been used for data entry, 
## missing values indicate that the previous value should be carried forward.
## Compare filldat and rawdat
filldat <- rawdat %>%
  fill(block:temp) #fill in missing values

print(filldat) #tidied
print(rawdat)  #originl dataset

# The difference between rawdat and filldat comes from how missing values (NA) are handled

#################################################################### 
## Common problem 1: Column headers are values, not variable names #
####################################################################

## QUESTION: Does this problem exist in this dataset? If so, how would you change it?

# Yes, this problem exists. The `w_1`–`w_10` and `w_A`–`w_C` columns are values, not variable names. 
# To tidy the data, you can use `pivot_longer()` to turn these columns into two: one for the willow ID and one for the measurement value. This makes each row a single observation and each column a variable, which is easier to analyze.

## FIX IT: Create a new dataframe called cleandat that starts with filldat and then solves this tidy data problem
cleandat <- filldat %>%
  # pivot_longer to collapse column headers to values in a column
  pivot_longer(cols = w_1:w_C, names_to = "willow_id", values_to = "value") %>%
  
  # and for fun let's remove the "w_" preface
  separate(col = willow_id, into = c("remove", "willow_ID"), sep = "_") %>%
  select(-remove)
print(cleandat)

# TASK: Get the same result but with the start_with argument

cleandat <- filldat %>%
  # pivot_longer to collapse column headers to values in a column
  # Using starts_with("w_") instead of w_1:w_C
  pivot_longer(
    cols = starts_with("w_"), 
    names_to = "willow_id", 
    values_to = "value"
  ) %>%
  
  # and for fun let's remove the "w_" preface
  separate(
    col = willow_id, 
    into = c("remove", "willow_ID"), 
    sep = "_"
  ) %>%
  
  select(-remove)
print(cleandat)
# Compare number of rows and columns
nrow(cleandat)             # should be much larger than original (one row per measurement)
ncol(cleandat)             # should have block, plot, code, snow, n, temp, variable, willow_ID, value


#################################################################### 
## Common problem 2: Variables are stored in both rows and columns #
####################################################################


## QUESTION: Does this problem exist in this dataset? If so, how would you change it?
## Hint: Whenever there is a column called "variable" it's likely this problem is happening

# Yes, this problem exists in the dataset. The column variable stores different measurement names 
# (like ht1, ht2, cnpy1, cnpy2) while the actual values are spread across multiple columns (w_1 to w_C). 
# This means the same type of information is stored both in rows and columns, which makes analysis difficult.
# To fix it, you would reshape the data so each variable has its own column and each row is one observation.


## FIX IT
## Note: I'll just keep coping the workflow from above - so look to the end of the workflow for this change!
cleandat <- filldat %>%
  # Step 1: Collapse the w_ columns into long format (plant ID + value)
  pivot_longer(
    cols = starts_with("w_"), 
    names_to = "willow_id", 
    values_to = "value"
  ) %>%
  
  # Step 2: Remove "w_" prefix from willow_id
  separate(
    col = willow_id, 
    into = c("remove", "willow_ID"), 
    sep = "_"
  ) %>%
  select(-remove) %>%
  
  # Step 3: FIX COMMON PROBLEM 2 – spread the different variables into columns
  pivot_wider(
    names_from = variable, 
    values_from = value
  )
print(cleandat)

#################################################################### 
## Common problem 3: Multiple variables are stored in one column   #
####################################################################

## QUESTION: Does this problem exist in this dataset? If so, how would you change it?

# The column variable contains different measurement types (like ht1, ht2, cnpy1, cnpy2) while the actual 
# values are spread across the wide columns (w_1 to w_C). This means the same type of information is stored
# both in rows and columns, making analysis difficult. To fix it, you would reshape the data:
# Use pivot_longer() to collapse the w_ columns into a single column of values. Clean the IDs 
# Use pivot_wider() to spread the variable column so each measurement type has its own column.


## FIX IT
cleandat <- filldat %>%
  # pivot_longer to collapse column headers to values in a column
  pivot_longer(
    cols = w_1:w_C, 
    names_to = "willow_id", 
    values_to = "value"
  ) %>%
  
  # and for fun let's remove the "w_" prefix
  separate(
    col = willow_id, 
    into = c("remove", "willow_ID"), 
    sep = "_"
  ) %>%
  select(-remove) %>%
  
  # pivot_wider to make each column a variable
  pivot_wider(
    names_from = variable, 
    values_from = value
  ) %>%
  
  # NEW CODE: Take information on whether the seedling was found from ht1 and make it its own column
  mutate(
    status = "present",
    status = ifelse(ht1 == "CF", "missing", status),
    status = ifelse(ht1 == "dead", "dead", status)
  ) %>%
  
  # Make sure relevant columns are numeric
  # Note: parse_number handles text like "12 cm", while as.numeric expects pure numbers
  mutate(
    ht1 = parse_number(ht1),
    ht2 = as.numeric(ht2),
    cnpy1 = as.numeric(cnpy1),
    cnpy2 = as.numeric(cnpy2)   # fixed: was incorrectly using cnpy1 before
  ) %>%
  
  # create a new column that codes for the year based on willow_ID
  mutate(
    year = ifelse(willow_ID %in% 1:10, 2007, 2006)
  )
print(cleandat)


######################################################################################### 
## Common problem 4: Multiple types of observational units are stored in the same table #
#########################################################################################


## QUESTION: Does this problem exist in this dataset? If so, how would you change it?

#Yes, it does. The dataset contains multiple types of observational units in the same table: 
# plot-level information (like block, plot, temp) and plant-level information (like willow_ID, ht1, cnpy1). 
# To fix this problem, you should separate the dataset into two tables based on the type of observational unit. 
# Create a plot-level table that contains one row per plot with all plot-specific information like block, plot, and temperature. 
# Then you can combine the two tables back together using joins, such as left_join or right_join, to access both plot-level and plant-level information in the same dataset.



## FIX IT
## Note - here we'll need to make two separate dataframes

plotdat <- cleandat %>%
  select(block:temp) %>%
  unique()  # FIXED: added unique() to ensure one row per plot, removing duplicates

willowdat <- cleandat %>%
  select(block, plot, willow_ID:year)

## These would be elements of a relational database
## It might seem trivial to have this be a relational database, but it's super useful for larger, more complicated datasets
## QUESTION: What are some reasons it would be better to store data as a relational database?

# because it reduces redundancy, makes updates easier and improves data organization for large or complex datasets.

## You can always put the data back together
tog <- right_join(plotdat, willowdat)  # FIXED: join plot-level info back to plant-level data

## And you can check to make sure it really works!
identical(cleandat, tog)


## TASK: read about the difference between left, right, and full joins
## Which joins would have worked here? When would one be better than the other? 

# In this case, both left_join and right_join would work. A left_join keeps all rows from the left
# table and adds matching rows from the right table, while a right_join keeps all rows from the right 
# table and adds matching rows from the left. Since we want all plant-level observations and to attach plot-level information, 
# a right_join works when willowdat is on the right, and a left_join works if willowdat is on the left.


############################################################################### 
## Common problem 5: A single observational unit is stored in multiple tables #
###############################################################################


## QUESTION: Does this problem exist in this dataset? If so, how would you change it?

# No, the current dataset does not have this problem. Each observation is already stored in a single table.


## ADDRESS IT: In this case we're in the clear, but below I've manipulated the data to show a common instance of this problem
# Separate the 2006 data and remove the year column temporarily
dat2006 <- cleandat %>%
  filter(year == 2006) %>%
  select(-year)
# Separate the 2007 data and remove the year column temporarily
dat2007 <- cleandat %>%
  filter(year == 2007) %>%
  select(-year)

## FIX IT:
dat2006 <- dat2006 %>% # Add a year column back to the 2006 data
  # move the data from the table name to a column
  mutate(year = 2006)

dat2007 <- dat2007 %>%
  # move the data from the table name to a column
  mutate(year = 2007)  # store table identity in a column

## Bind the data together
tog <- rbind(dat2006, dat2007)  # combine rows from both years


################################################################################################### 
## Put it together graphically: 
## How does snow addition, temperature and nitrogen affect willow seedling survival and growth? ###
###################################################################################################

## QUESTION: How would you create a measure of survival from this dataset?

# To create a measure of survival, you can make a new column where each seedling that is alive (status == "present") 
# is coded as 1, and all others (dead or missing) are coded as 0. Then, group the data by your treatment variables 
# (like block, plot, snow, nitrogen, temperature) and calculate the proportion of seedlings that survived in each group by
# dividing the number alive by the total number of seedlings.

## ADDRESS IT: Aggregate the data to create a proportion survival for each plot
survivedat <- cleandat %>%
  # let's give any willows that survived a 1 for present
  mutate(survive = ifelse(status == "present", 1, 0)) %>%

  # identify what variables you want to aggregate within
  group_by(block, plot, code, snow, n, temp) %>%
  
  # and summarize within that block 
  # number of willows that survived
  # total number of willows seeded
  # proportion survived
  summarize(numsurvive = sum(survive), numseeded = n(), propsurvive = numsurvive/numseeded)
  

## PLOT SURVIVAL - preliminary
ggplot(survivedat, aes(x = code, y = propsurvive)) + geom_boxplot()

# or break it up however is most meaninful to you - here's some examples of ways to break up the data
ggplot(survivedat, aes(x=snow, fill = n, y = propsurvive)) + geom_boxplot() + facet_wrap(~temp)



## TASK: Graph the growth data (height and canopy) to see if there's a treatment effect

growthdat <- cleandat %>%
  # Calculate average height and canopy per plant
  mutate(avg_ht = rowMeans(select(., ht1, ht2), na.rm = TRUE),
         avg_cnpy = rowMeans(select(., cnpy1, cnpy2), na.rm = TRUE)) %>%
  
  # Aggregate per plot/treatment
  group_by(block, plot, code, snow, n, temp) %>%
  summarize(
    mean_ht = mean(avg_ht, na.rm = TRUE),
    mean_cnpy = mean(avg_cnpy, na.rm = TRUE),
    .groups = "drop"
  )
print(growthdat)

## Plot mean height
ggplot(growthdat, aes(x = snow, y = mean_ht, fill = n)) +
  geom_boxplot() +
  facet_wrap(~temp) +
  labs(y = "Mean Height", x = "Snow Addition", fill = "Nitrogen Level")

## Plot mean canopy
ggplot(growthdat, aes(x = snow, y = mean_cnpy, fill = n)) +
  geom_boxplot() +
  facet_wrap(~temp) +
  labs(y = "Mean Canopy", x = "Snow Addition", fill = "Nitrogen Level")
