ls()# C. E. Timothy Paine
# BIOU9PC POpulation and Community Ecology
# Practical 1a
# Sep 15 2016
# Modified from Luc Bussiere's introductory practical to SCIU7SR

# last modified Sep 10, 2013

# before you begin, make sure you are working on a new project, within a new directory, and that you have created a new R script in the source panel (check that the tab has an RStudio logo to make sure commands will work)

# clear R of all objects
rm(list=ls())

# use right alt and 3 to get the hash sign 
# if you can't already see it on your keyboard

# these commands can check or change the default directory, but easiest to use pulldown menus in files panel
# if you have done the above and are working through a correctly set up project, these commands are not necessary
# getwd()
# setwd()

#### Getting objects into R  ####
# import the data; note that read.csv works for comma delimited files
WORMS<-read.csv("worms.csv")

# examine the object in the console
WORMS

# examine the new object
head(WORMS)
names(WORMS)
summary(WORMS)
str(WORMS)

# examine a vector from the data frame
WORMS$AREA

# perform operations on vectors
mean(WORMS$AREA)
sd(WORMS$DENSITY) 

# value of NA is not very likely. have we called the object correctly?
names(WORMS)
# column is called worm.density, not density
sd(WORMS$WORM.DENSITY) 


# store results of operations in new objects. Note that they will appear in the workspace panel!
MEAN.SOIL.PH<-mean(WORMS$SOIL.PH)

# you have to ask for the object to see the result in the console, or in some cases the result will be displayed in the workspace panel
MEAN.SOIL.PH

#### Examining and manipulating data #####
# examine the third record of the second column, the AREA for Nursery Field
WORMS[3,2]

# examine the vector SLOPE
WORMS$SLOPE
WORMS[,3]

# request rows 5-15 of the data frame
WORMS[5:15,]

# store rows 5-15 in a new object
WORMS5TO15<-WORMS[5:15,]

# specify records satisfying certain conditions
WORMS[WORMS$AREA>3,]
WORMS[WORMS$AREA>3 & WORMS$SLOPE<3,]

# order cols 1:6 by AREA
WORMS[order(WORMS[,2]),1:6]

# ask for DATAFRAME, descending order by soil pH
WORMS[rev(order(WORMS[,5])),]
# alternatively to using the rev command, there is an "argument" for the order command that you could use
# i found this argument by inspecting the help file for rev; we'll see this later

?order
WORMS[order(WORMS[,5],decreasing=TRUE),]

# get only data for which Damp is TRUE, and store in a new object
DAMPWORMS<-WORMS[WORMS$DAMP==TRUE,]
# make sure there are two "equals" signs in a row!

DAMPWORMS

# Remember operators are <, <=, >, >=, == for exact equality and != for inequality

# All columns with Meadow vegetation
MEADOWWORMS<-WORMS[WORMS$VEGETATION=="Meadow",]
MEADOWWORMS
# make sure you put Meadow in quotation marks because it is text

# All columns in Grasslands, sorted by density
GRASSWORMS<-WORMS[WORMS$VEGETATION=="Grassland",]
GRASSWORMS.BYDENS<-GRASSWORMS[order(GRASSWORMS[,7]),]
GRASSWORMS
GRASSWORMS.BYDENS


# The plots where density < 5.5, sorted by Slope
HIDENSWORMS<-WORMS[WORMS$WORM.DENSITY>5.5,]
HIDENSWORMS.BYSLOPE<-HIDENSWORMS[order(HIDENSWORMS[,3]),]
HIDENSWORMS.BYSLOPE

# The area and soil pH for all plots not in Grassland
NOTGRASSWORMS.AREAPH<-WORMS[WORMS$VEGETATION!="Grassland",c(2,5)]
NOTGRASSWORMS.AREAPH
# Note the c command is for concatenate, meaning stick together. you will use it often!


#### GRAPHING ####
plot(x = WORMS$SOIL.PH, y = WORMS$WORM.DENSITY)
plot(WORMS$SOIL.PH, WORMS$WORM.DENSITY) # names of arguments are optional, as long as they are supplied in the correct order.
plot(WORMS$SOIL.PH, WORMS$WORM.DENSITY, col = 'blue') # make the points blue
plot(WORMS$SOIL.PH, WORMS$WORM.DENSITY, col = 'blue', xlab = 'Soil pH', ylab = "Worm density")
plot(WORMS$SOIL.PH, WORMS$WORM.DENSITY, cex = WORMS$AREA) # make the points vary by the size of each site



#### Getting help #####
# use the ? to get help on a command
?read.table
?order

# ?? if the subject doesn't turn up using a single question mark, search within the help files
??standard.deviation
# or use help.search, which has a more flexible range of options
# as illustrated in the help file
?help.search


help.search("non-existent topic")

?median
?max
?min
?log
# log will default to the natural log, so use log10 if you want to do the log base 10

?square.root
# this doesn't provide any hits because the help file for sqrt doesn't contain the phrase "square root"! So you sometimes need to be resourceful or guess at commands

help.search ("square root")
# no hits here either

# guess?
?sqrt


# example() gives examples of simple procedures
example(sd)

# demo() is generally reserved for more complicated methods
demo(lm.glm)

# you will need to click on the console window to proceed through the demonstration
# don't be afraid to consult the help and FAQ files on CRAN: http://cran.r-project.org
# google is also often useful, especially for searching error messages

