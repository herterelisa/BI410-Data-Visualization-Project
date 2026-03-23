################################################################################
### Making Better Graphics                                            
################################################################################

## OBJECTIVE:
## Practice improving upon default graphics
## Learn some of the tools available to us for customizing plots

## Note: There are many resources for effective statistical graphic design out here. 
## For example, check out The Fundamentals of Data Visualization
## https://clauswilke.com/dataviz/

## Go ahead and load the requisite packages environment
library(tidyverse)

## Now read in the "sparrows" dataset. Note that the sparrows data set is a tab-delimited text file. 
## I've got the data nested in a data folder. You can either do the same, or adjust the file location.
sparrows <- read.table("Sparrows.txt", header = TRUE, sep = "\t")

## Lets get a sense of the the data structure and its variables. 
head(sparrows)
str(sparrows) 

## It looks like we have a few variables that should be encoded as factors
## Species, Sex, and Observer
sparrows <- sparrows %>%
              mutate(Species = as.factor(Species),
              Sex = as.factor(Sex),
              Observer = as.factor(Observer))

## Now that our variables are correctly encoded, lets go ahead and plot our data
## Let's assess the relationship between the Head and Tarsus size
## We'll also differentiate points by species
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point()

## Out of the gate, our plot looks decent. This is the power of ggplot over base R. 
## However, there's room for improvement. 

#############################
# Axis labels and titles
#############################

## The first thing to consider is whether your labels need improvement. 
## In our case, they most certainly do. Rarely do our default variable names pass muster.

## We can change our axis labels with either the 'labs' function, or 'xlab' and 'ylab' respectively
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length")

## QUESTION: What key piece of information is missing from each of our labels? 
## i.e. What else would you want to know about the measurements?

#Answer: Units of measurement

## TASK: Add this information to the plot.

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) +
  geom_point() +
  labs(x = "Diameter of Head (mm)", y = "Tarsus Length (mm)")


######
# Annotations
######

## It is frequently helpful to add annotations to our plots.
## For example, we might want to label plots with a letter, or label a region of points.

## Let's do just that, and add species names directly to our plot. 
## A natural place to do this would be the average of our two variables for each species
## Lets first determine the average head diameter and tarsus length for each species:
sp_labels <- sparrows %>%
  group_by(Species) %>%
  select(head_diameter, tarsus_length) %>%
  summarize_all(mean)

## Now we can add our labels using the geom_text() function
## We'll make the text black to stand out against our point colors. 
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  geom_text(data = sp_labels, 
            aes(x=head_diameter, y=tarsus_length, label = Species, fontface = 'bold'), color = "black") + 
  labs(x = "Diameter of Head", y = "Tarsus Length")

## TASK: Lets assume our plot is actually part one of a two panel figure.
## To account for this, add an 'A' in the upper-left corner of the plot.

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) +
  geom_point() +
  geom_text(data = sp_labels,
            aes(x = head_diameter, y = tarsus_length, label = Species, fontface = "bold"),
            color = "black") +
  annotate("text", x = 28.5, y = 26.5, label = "A", size = 6, fontface = "bold") + #a panel label for a multi-panel figure
  labs(x = "Diameter of Head (mm)", y = "Tarsus Length (mm)") #labels with units

######
# Changing color
######

## Of all the properties in your figures, color might be the one you play around with the most. 
## While ggplot has nice looking defaults, I encourage you to discover interest colors for your own use. 

## For example, we could choose any number of default color palettes in ggplot
## Color Brewer is a common source of nice color palettes. 
## Check them out at https://colorbrewer2.org/
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  geom_text(data = sp_labels, 
            aes(x=head_diameter, y=tarsus_length, label = Species, fontface = 'bold'), color = "black") + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2")

## Or you could use one of the many custom palettes out in the wild! 
## Lets try using a Wes Anderson color palette (you might have to first install the package)
library(wesanderson)

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  geom_text(data = sp_labels, 
            aes(x=head_diameter, y=tarsus_length, label = Species, fontface = 'bold'), color = "black") + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_manual(values = wes_palette(n = 2, name = "GrandBudapest1"))

## Note that here we use the function scale_color_manual
## This lets us set our own colors that aren't part of a preset ggplot palette
## This frees us from the use of a pre-set palette. We can tell ggplot to use any colors we want!

## A quick note: R recognizes three color formats:
## 1) Specific color names, like "red", "blue", "coral", etc... 
## 2) Hexadecimal colors, like "#32a852" or "#FF0000"
## 3) RGB colors using the rgb() function, like rgb(red = 0, green = 0, blue = 255, alpha = 1)
## The latter formats give you fine control over your desired colors. 

## TASK: Using scale_color_manual, change the colors to whatever you want.
## You'll have to give pass the function a vector of your colors. 

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) +
  geom_point() +
  geom_text(data = sp_labels,
            aes(x = head_diameter, y = tarsus_length, label = Species, fontface = "bold"),
            color = "black") + #labeling species clusters
  labs(x = "Diameter of Head (mm)", y = "Tarsus Length (mm)") +
  scale_color_manual(values = wes_palette(n = 2, name = "Zissou1")) #a color theme palette


######
# Changing point attributes
######

## Color is not the only way we can encode information in our graphs
## In this case, we have another factor ('sex') that might be useful to visualize in our figure
## Instead of using another color, lets encode our 'sex' factor using point shapes
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species, shape = Sex)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2")

## Admittedly, these points are a bit hard to distinguish. Let's change the point types. 
## Similar to color, we can manually set point types with 'scale_shape_manual()'

## TASK: Distinguish sex using two different point types. 
## You may need to search online for R point or 'pch' values. 

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species, shape = Sex)) +
  geom_point(size = 2.5) +
  labs(x = "Diameter of Head (mm)", y = "Tarsus Length (mm)") +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(16, 17)) #diff shaped points for each species

## Finally, let's do the impossible - layer even more information into our plot!
## We've changed the point color and shape, but what about point size?


## CHALLENGE: Scale point size by weight ('Wt').
## Ensure that the range in point sizes is sufficiently large to distinguish point size.
## HINT: Like previous examples, you will need an aesthetic argument and a new layer.  

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length,
                     color = Species, shape = Sex, size = Wt)) +
  geom_point() +
  labs(x = "Diameter of Head (mm)", y = "Tarsus Length (mm)") +
  scale_color_brewer(palette = "Dark2") +       #color theme
  scale_shape_manual(values = c(16, 17)) 
  scale_size_continuous(range = c(1, 8))     #scales point sizes between lightest and heaviest


######
# Axis limits
######

## In general,'ggplot' does a nice job with default axis settings
## However, sometimes we need to customize our axes
## e.g. axis limits, order of values, tick intervals, tick labels, etc...

## If we wanted more padding around our data, we could increase our axis limits.
## We can do this with the 'xlim()', 'ylim()', or 'lims()' layers. 
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2") + 
  xlim(28,37) + ylim(18,27)


## I don't like how the default axis ticks have come out. Let's customize our axis as well. 
## For this, we need the 'scale_[axis]_continuous' layer.
## Note that this new layer will override any changes to the axis scale we made prior. 
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2") + 
  scale_x_continuous(breaks = seq(29, 37, by = 2), labels = seq(29, 37, by = 2)) +
  scale_y_continuous(breaks = seq(18, 26, by = 2), labels = seq(18, 26, by = 2))

## We can also reverse the order of the axis. 
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2") + 
  scale_x_reverse(breaks = seq(29, 37, by = 2), labels = seq(29, 37, by = 2)) +
  scale_y_continuous(breaks = seq(18, 26, by = 2), labels = seq(18, 26, by = 2))

######
# Background themes
######

## You've likely noticed at this point that 'ggplot' imposes a number of style elements automatically. 
## Most notably, axis styles and the background. 
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2")

## BUT - we can alter these defaults in a number of ways by changing the 'theme'.
## For example, there are a wide range of build-in ggplot themes (the default is 'theme_gray')
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_bw()

## QUESTION: What elements of your plot did this alter from the default? 
#Answer: theme_bw() removes the grey background panel and replaces it with

# white, and adds a black border around the plot area. Grid lines are retained
# but are now more visible against the white background.
## Some other themes I like
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_classic() #creates white backround, no grid lines, only has axis lines

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_minimal() #removes axis lines and backround, lightens the grid lines, no axis lines

## Check out some other fun themes in the ggthemes package:
## https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/

## TASK: Change the plot using a theme from the 'ggthemes' package

library(ggthemes)

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) +
  geom_point() +
  labs(x = "Diameter of Head (mm)", y = "Tarsus Length (mm)") +
  scale_color_brewer(palette = "Dark2") +
  theme_clean()    # clean white background with minimal grid lines
                   # and a legend box for readability

## Finally, if you get sick of updating the theme every time, you can change your defaults
## To do so, 'theme_set()' and 'theme_update()' are your friends.
theme_set(theme_bw())

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2")

theme_update(panel.grid.minor = element_line(colour = "orchid2"))

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2")

## There are many, many ways to customize a theme
## https://ggplot2.tidyverse.org/reference/theme.html

######
# Saving your figure
######

## To wrap up this lesson, we'll briefly show you how to write your figures to file. 

## Note that there are two primary types of computer-generated figures
## 1) Bitmaps/rasters: fixed resolution images made up of pixels, such as .jpeg, .png
## 2) Vector graphics: images composed of points and lines/curves drawn between them, such as .pdf

## In general, vector graphics are higher quality but require more computer memory
## Bitmaps are lower quality, but require less computer memory

## To create a .pdf from our image: 
## First, open a 'graphics device', set file name, and provide height and width (in inches)
pdf('./figures/new_image.pdf', height = 4, width = 6)

## Create your figure
ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_bw()

## Turn off the graphics device
dev.off()

## The process to create a bitmap is similar, but we need to set the resolution
png('./figures/new_image.png', height = 480, width = 720)

ggplot(sparrows, aes(x = head_diameter, y = tarsus_length, color = Species)) + 
  geom_point() + 
  labs(x = "Diameter of Head", y = "Tarsus Length") + 
  scale_color_brewer(palette = "Dark2") + 
  theme_bw()

dev.off()
