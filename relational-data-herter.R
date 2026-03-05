################################################################################
### WRANGLE: Relational data                                                 ### 
################################################################################

## OBJECTIVES:
# To learn what relational data are and why this is a useful data structure
# To learn how to join datasets using dplyr syntax
# To become comfortable developing workflows that integrate multiple data frames

# Lesson material adapted from: Paula Andrea Martinez, Timothée Poisot (eds): "Data Carpentry: SQL for Ecology lesson.
# "Version 2017.04.01, April 2017,
#http://www.datacarpentry.org/sql-ecology-lesson/

# You can learn more about the Portal Project here:
# https://portal.weecology.org/


## WHY USE A RELATIONAL DATABASE?
# Using a relational database serves several purposes
# 
# It keeps your data separate from your analysis
# This means there’s no risk of accidentally changing data when you analyze it
# If we get new data we can just rerun the script
# It’s fast, even for large amounts of data
# It improves quality control of data entry

## WHAT ARE RELATIONAL DATABASES?
# Relational databases store data in tables with fields (columns) and records (rows)
# Data in tables has types, and all values in a field have the same type (list of data types)
# Queries (or dplyr syntax) let us look up data or make calculations based on columns

## Characteristics: 
# Every row-column combination contains a single atomic value, i.e., not containing parts we might want to work with separately.
# One field per type of information
# No redundant information
# Split into separate tables with one table per class of information
# Needs an identifier in common between tables – shared column - to reconnect (known as a foreign key).


# Load the tidyverse
library(tidyverse)


# For practice with joins, generate two tibbles:
# If you forget what the different joins do, mess around with these practice tibbles! 
tibble1<-as_tibble(data.frame(x1=c(1,3,5,7,9),x2=c(2,4,6,8,10)))
tibble2<-as_tibble(data.frame(x2=c(4,6,8,10,12),x3=c(1,4,7,10,14)))

#joins can work without specifying the column if there is only one column they have in common
#Otherwise, you must specify using by="column_name".
#Most of the time, you will probably want an inner join as this retains only the data included in both datasets

#Inner join - both of these give the same thing in this case, since only one column is shared
inner_join(tibble1,tibble2)
inner_join(tibble1,tibble2,by="x2") 

#Left join
left_join(tibble1,tibble2,by="x2")

#Right join
right_join(tibble1,tibble2,by="x2")

#Full join
full_join(tibble1,tibble2,by="x2")

#Joins will (probably) be most useful with some categorical data...
tibble3<-as_tibble(data.frame(mammal=c("squirrel","dog","cat","bear","deer"),quantity=c(7,1,4,2,5)))
tibble4<-as_tibble(data.frame(mammal=c("squirrel","dog","cat","possum"),avgweight=c(2,25,8,2)))

inner_join(tibble3,tibble4,by="mammal")

left_join(tibble3,tibble4,by="mammal")

right_join(tibble3,tibble4,by="mammal")

full_join(tibble3,tibble4,by="mammal")


# Handy standard error function
calcSE <- function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

## Read in the data
surveys <- read_csv("surveys.csv")
spec(surveys)

species <- read_csv("species.csv")
spec(species)

plots <- read_csv("plots.csv")
head(plots)

## QUESTION: What are the primary keys of the plots, species and surveys data frames?
  # The primary key of the plots data frame is plot_id because each plot_id uniquely identifies a plot.
  # The primary key of the species data frame is species_id because each species_id uniquely identifies a species.
  # The primary key of the surveys data frame is record_id because each record_id uniquely identifies a survey observation.

## EXERCISE: How has the hindfoot length and weight of Dipodomys genus changed over time?

# Join surveys and species
dipodomys_data <- inner_join(surveys, species, by = "species_id") %>%
  
  # Keep only Dipodomys genus
  filter(genus == "Dipodomys") %>%
  
  # Group by year
  group_by(year) %>%
  
  # Calculate yearly averages
  summarize(mean_hindfoot = mean(hindfoot_length, na.rm = TRUE),
            se_hindfoot = calcSE(hindfoot_length),
            mean_weight = mean(weight, na.rm = TRUE),
            se_weight = calcSE(weight),
            n = n())
print(dipodomys_data)

# Which files have the data I need? 

# I need surveys.csv because it contains the year,
# hindfoot_length, weight, and species_id measurements.
# I also need species.csv because it tells me which
# species_id values belong to the Dipodomys genus.


# What operations would I need to perform if I were doing these analyses by hand?

# First, I would find which species_id values belong to Dipodomys.
# Then, I would keep only those records in the surveys data.
# Next, I would group the data by year.
# Finally, I would calculate the average hindfoot length and weight for each year.


##############################################
## Example workflow to answer this question ##
##############################################

# join together survey and species data
sumdat <- inner_join(surveys, species, by="species_id") %>%
  
  # just retain Dipodomys records
  filter(genus == "Dipodomys") %>%
  
  #Remove any rows missing hindfoot_length OR weight measurements
  filter(!is.na(hindfoot_length), !is.na(weight)) %>% 
  
  # group by year
  group_by(year) %>%
  
  # calculate the replication number, mean and se of length and mean and se of weight
  summarize(repnum = n(), 
            meanlength = mean(hindfoot_length, na.rm=T), 
            selength = calcSE(hindfoot_length),
            meanweight = mean(weight, na.rm=T),
            seweight = calcSE(weight))

# graph length
ggplot(sumdat, aes(x = year, y = meanlength, color = "Mean ± SE")) + 
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin = meanlength - selength, ymax = meanlength + selength)) +
  scale_color_manual(values = "blue", name = NULL) +
  labs(title = "Mean Hindfoot Length Over Time (Dipodomys)",
       x = "Year", y = "Mean Hindfoot Length (mm)") +
  theme_bw() +
  theme(legend.position = "bottom")

# graph weight
ggplot(sumdat, aes(x = year, y = meanweight, color = "Mean ± SE")) + 
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin = meanweight - seweight, ymax = meanweight + seweight)) +
  scale_color_manual(values = "darkred", name = NULL) +
  labs(title = "Mean Weight Over Time (Dipodomys)",
       x = "Year", y = "Mean Weight (g)") +
  theme_bw() +
  theme(legend.position = "bottom")

sum(is.na(surveys$hindfoot_length))
sum(is.na(surveys$weight))

## QUESTION: Could the species identity be affecting these patterns? How would you check? 

# Yes, the drop in average weight and hindfoot length over time is mainly due to changes in 
# which Dipodomys species were common. Early on, the large banner-tailed Dipodomys was common, 
# raising the averages, but it declined sharply after extreme flooding in the mid-1980s, leaving 
# mostly smaller species (D. ordii and D. merriami). This caused a big drop in mean weight and a 
# smaller decline in hindfoot length, even though individual sizes within each species stayed about the same.

# How to check: Group and plot by species_id (or species) to see if the overall pattern disappears or weakens
# within species, and if DS dominates early then fades.

# Summarize by year AND species
sumdat_sp <- inner_join(surveys, species, by = "species_id") %>%
  filter(genus == "Dipodomys") %>%
  filter(!is.na(hindfoot_length), !is.na(weight)) %>%
  group_by(year, species_id) %>%
  summarise(
    n = n(),
    mean_weight = mean(weight),
    se_weight = calcSE(weight),
    mean_length = mean(hindfoot_length),
    se_length = calcSE(hindfoot_length),
    .groups = "drop"
  )

# Plot weight by species — look for DS high early, then rare/low
ggplot(sumdat_sp, aes(x = year, y = mean_weight, color = species_id)) +
  geom_point(aes(size = n)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.5) +
  labs(title = "Mean Weight by Species ID Over Time",
       subtitle = "DS (large) prominent early then declines sharply → drives overall drop",
       y = "Mean Weight (g)") +
  theme_bw() +
  theme(legend.position = "bottom")

# Same for hindfoot length
ggplot(sumdat_sp, aes(x = year, y = mean_length, color = species_id)) +
  geom_point(aes(size = n)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_length - se_length, ymax = mean_length + se_length), width = 0.5) +
  labs(title = "Mean Hindfoot Length by Species ID Over Time",
       y = "Mean Hindfoot Length (mm)") +
  theme_bw()

##############################################
##             YOUR TURN                    ##
##############################################

## Within your group, use the imported files, join, and dplyr functions as needed to answer the following questions:
## Just write the code to generate the answer in a tibble; you do not need to copy the numbers from the tibble
library(tidyverse)
## QUESTIONS: 
# Q1) How many plots from each type are there? Determine which file(s) you need and which operations to perform.

plots %>%
  count(plot_type, name = "n_plots") %>%
  arrange(desc(n_plots))

# There are 8 Control plots, 6 Rodent Exclosure plots, 4 Long-term Krat Exclosure plots, 4 Short-term Krat Exclosure 
# plots, and 2 Spectab exclosure plots.


# Q2) How many specimens of each species were captured in each type of plot? Determine which file(s) you need and which operations to perform.

surveys %>%
  # Join species info (to get genus, species name, taxa)
  inner_join(species, by = "species_id") %>%
  
  # Join plot info (to get plot_type: Control, Rodent Exclosure, etc.)
  inner_join(plots, by = "plot_id") %>%
  
  # Count the number of captures for each combination of plot_type + species
  count(plot_type, species_id, genus, species, name = "n_captures") %>%
  
  # Sort first by plot type, then by most captured species within each type
  arrange(plot_type, desc(n_captures))

# you need three files: surveys (every captured animal, species_id (full species names, linked by species_id), and plots (the plot type for each plot_id).
# Start with the surveys table, then join it to species using inner_join on species_id to add genus and species names to each record. Next, join that result to plots using inner_join
# on plot_id to add the plot type (Control, Rodent Exclosure, Long-term Krat Exclosure, etc.). Use count() to tally captures for every unique plot_type–species combination, saving the 
# result as n_captures. Finally, arrange by plot_type and then by n_captures in descending order, so the most-captured species appear first within each plot type.

# Q3) What is the average weight of each taxa? Determine which file(s) you need and which operations to perform.
#Hint: find the average within each species first, then average species within each taxa these to find the average for the whole taxa

surveys %>%
  # Combine surveys with species info, focus on records with weight
  inner_join(species, by = "species_id") %>%
  filter(!is.na(weight)) %>%
  # Calculate average weight for each species
  group_by(taxa, species_id, species) %>%
  summarise(mean_weight_species = mean(weight), .groups = "drop") %>% #avg weight per species
  # average weight for each taxa and count species
  group_by(taxa) %>%
  summarise(avg_weight_taxa = mean(mean_weight_species, na.rm = TRUE), #Average across the taxa
            n_species = n(), # number of species in the taxa
            .groups = "drop") %>%
  arrange(desc(avg_weight_taxa)) #arrange taxa from heaviest to lightest

# You need the surveys file, which has each animal’s weight and species_id, and the species file, which links species_id to the taxa like 
# Rodent or Bird. Join surveys to species to add the taxa information, and remove any rows with missing weight. First, calculate the average 
# weight for each species, then calculate the overall average weight for each taxa and count how many species are in each. Finally, arrange 
# the results by descending average weight to see the heaviest taxa first, giving a table with one row per taxa showing its average weight and species count.

# Q4) What percentage of the rodents captured in the Portal project control plots were in the genus Dipodomys?
# Hint: you might want to join two datasets into an intermediate dataframe to join to again

#jOIN all captures with genus, taxa, and plot_type
all_data <- surveys %>%
  inner_join(species, by = "species_id") %>%
  inner_join(plots, by = "plot_id")

# Now filter and calculate for control plots only
all_data %>%
  filter(plot_type == "Control", taxa == "Rodent") %>%
  summarise(
    total_rodents = n(), #total captured
    dipo_count = sum(genus == "Dipodomys", na.rm = TRUE), #Number of Dipodomys captured
    percent_dipodomys = (dipo_count / total_rodents) * 100 #percent of rodents that are Dipodomys
    all_data %>%
  )

# 15,466 rodents were captured across all years, and 9,847 of 
# those were Dipodomys. This means Dipodomys made up about 63.7% 
# of all rodents in Control plots. To calculate this, surveys WE 
# joined species and plots to get genus and plot type, then filtered 
# for Control plots and rodents. Counting the total rodents and the Dipodomys 
# captures, the percentage was found by dividing Dipodomys by total rodents and multiplying by 100.

# Q5) What are the minimum, maximum and average weight for each species of Rodent?

# Combine surveys with species info,focus on rodents with known weights
surveys %>%
  inner_join(species, by = "species_id") %>%
  filter(taxa == "Rodent", !is.na(weight)) %>%
  # Group by each species to calculate stats per species
  group_by(species_id, genus, species) %>%
  summarise(
    min_weight  = min(weight), #lightest
    max_weight  = max(weight), #heaviest
    avg_weight  = mean(weight),#avg weight for species
    n_captures  = n(),       #how many species were measured     
    .groups     = "drop"            #ungroup after
  ) %>%
  arrange(desc(avg_weight))   #arrange from heaviest ot lightest

# he heaviest rodent on average is Neotoma albigula (packrat), followed by Dipodomys 
# spectabilis (banner-tailed kangaroo rat) and Spermophilus spilosoma (spotted ground squirrel). Smaller kangaroo rats like Dipodomys ordii and Dipodomys merriami are lighter, while pocket mice and grasshopper mice are the smallest.



