# coding club workshop 1 - R basics
# learning how to import and explore data, and make graphs about Edinburgh's biodiversity
# written by Molly Johnson, Bradley University/Oregon State University

# load packages
library(dplyr)

# set working directory
setwd("C:/Users/mljoh/github/CC-RBasics-master")

# import .csv data into an object (edidiv)
edidiv <- read.csv("C:/Users/mljoh/github/CC-RBasics-master/edidiv.csv")

# confirm that the data was imported without mistakes
head(edidiv) # displays first few rows
tail(edidiv) # displays the last rows
str(edidiv) # tells you whether the variables are continuous, integers, categorical or characters

head(edidiv$taxonGroup) # displays the first few rows of this column only
class(edidiv$taxonGroup) # tells you what type of variable we're dealing with: it's char now but we want it to be a factor

# as.factor() turns whatever values you put inside it into a factor. if we were to run only the code on the right side of the
# arrow, would work one time, but would not modify the data stored IN the object. by ASSIGNING with the arrow the output of the
# function to the variable, the original edidiv$taxonGroup actually gets overwritten, i.e. the transformation is stored in the object
edidiv$taxonGroup <- as.factor(edidiv$taxonGroup) # force taxonGroup to be a factor (categorical variable)
class(edidiv$taxonGroup) # check that forcing taxonGroup to be a factor was successful

# more dataset exploration
dim(edidiv) # displays number of rows and columns
summary(edidiv) # gives you a summary of the data
summary(edidiv$taxonGroup) # gives you a summary of that particular variable (column) in your dataset

#-------------------------------------------------------------------------------------------------------------------------------

# CALCULATE SPECIES RICHNESS OF EDINBURGH FROM 2000 - 2016
# split edidiv into multiple objects, each containing rows for only one taxonomic group using the
# filter() function from the dplyr package

# first arg of the function is the dataframe, second arg is the condition you want to filter on. Since we only want beetles here,
# we say: the variable taxonGroup MUST BE EXACTLY (==) Beetle - drop everything else from the dataset. remember R is case-sensitive.
Beetle <- filter(edidiv, taxonGroup == "Beetle")

# do the same w birds. very similar to filtering in other languages.
Bird <- filter(edidiv, taxonGroup == "Bird")

# continue to do the same w all the remaining taxa. use summary(edidiv$taxonGroup if you need to remember how things are spelled etc)
Butterfly <- filter(edidiv, taxonGroup == "Butterfly")

Dragonfly <- filter(edidiv, taxonGroup == "Dragonfly")

Flowering.Plants <- filter(edidiv, taxonGroup == "Flowering.Plants")  

Fungus <- filter(edidiv, taxonGroup == "Fungus")

Hymenopteran <- filter(edidiv, taxonGroup == "Hymenopteran")

Lichen <- filter(edidiv, taxonGroup == "Lichen")

Liverwort <- filter(edidiv, taxonGroup == "Liverwort")

Mammal <- filter(edidiv, taxonGroup == "Mammal")

Mollusc <- filter(edidiv, taxonGroup == "Mollusc")

# calculate species richness (i.e. the number of different species in each group)
# unique() will identify different species, and length() will count them
numBeetleSpecies <- length(unique(Beetle$taxonName))

numBirdSpecies <- length(unique(Bird$taxonName))

numButterflySpecies <- length(unique(Butterfly$taxonName))

numDragonflySpecies <- length(unique(Dragonfly$taxonName))

numFlowering.PlantsSpecies <- length(unique(Flowering.Plants$taxonName))

numFungusSpecies <- length(unique(Fungus$taxonName))

numHymenopteranSpecies <- length(unique(Hymenopteran$taxonName))

numLichenSpecies <- length(unique(Lichen$taxonName))

numLiverwortSpecies <- length(unique(Liverwort$taxonName))

numMammalSpecies <- length(unique(Mammal$taxonName))

numMolluscSpecies <- length(unique(Mollusc$taxonName))

#------------------------------------------------------------------------------------------------------------------------------------

# now that we have species richness for each taxon, combine all of those values into a vector (another type of R object that
# stores values). a data frame has 2 dimensions, rows & columns, while a vector has 1. create vectors using c() i.e. "concat"
# can add labels using the names() function

# create biodiversity vect, chaining all of the object names
biodiv <- c(numBeetleSpecies, 
            numBirdSpecies, 
            numButterflySpecies, 
            numDragonflySpecies, 
            numFlowering.PlantsSpecies, 
            numFungusSpecies,
            numHymenopteranSpecies, 
            numLichenSpecies, 
            numLiverwortSpecies, 
            numMammalSpecies, 
            numMolluscSpecies)

names(biodiv) <- c("Beetle",
                   "Bird",
                   "Butterfly",
                   "Dragonfly",
                   "Flowering.Plants",
                   "Fungus",
                   "Hymenopteran",
                   "Lichen",
                   "Liverwort",
                   "Mammal",
                   "Mollusc")

#------------------------------------------------------------------------------------------------------------------------------------

# now we can visualise species richness using the barplot() function 
# barplot(biodiv) #won't specify parameters this way, needs some work.
# check out help(barplot) and help(par) to help w barplot() function and plotting generally

# can also save file by wrapping code in the png() and dev.off() functions which respectively
# open and shut dowwn the plotting device (but won't show up in plots preview pane)
#  png("barplot.png", width = 1600, height = 600) # customize size/resolution of image
#  barplot(biodiv, xlab="Taxa", ylab="Number of Species", ylim=c(0, 600),
        #cex.names=1.5, cex.axis=1.5, cex.lab=1.5) # cex increases font size when >1 & vice versa
#  dev.off()

# printing w specifications to barplot viewer, needs exporting manually
barplot(biodiv, xlab="Taxa", ylab="Number of Species", ylim=c(0, 600), # customize size/resolution of image
        cex.names=0.75, cex.axis=0.75, cex.lab=1.5) # cex increases font size when >1 & vice versa

#------------------------------------------------------------------------------------------------------------------------------------
# CREATE A DATAFRAME AND PLOT IT

# data frames are tables of values, having 2-dimensional structure w rows and columns, where each col can have a different
# data type. vectors, remember, are only appropriate when dealing w just one set of values. 
# for instance (w data frame), a column called "Wingspan" would have numeric values measured on different birds (21.3, 182.1,
# 25.1, 8.9), and a column "Species" would have char values of the names of the species ("House sparrow", "Golden eagle", 
# "Eurasian kingfisher", "Ruby-throated hummingbird"). Another possible data format is a matrix - a matrix can have several rows
# of data as well (e.g. you can combine vectors into a matrix), but the variables must be all of the same type. for instance,
# they are all numerical and are the same length in terms of number of rows.

# create an object called "taxa" that contains all taxa names
taxa <- c("Beetle",
          "Bird",
          "Butterfly",
          "Dragonfly",
          "Flowering.Plants",
          "Fungus",
          "Hymenopteran",
          "Lichen",
          "Liverwort",
          "Mammal",
          "Mollusc")

# turn this object into a factor, i.e. a categorical variable
taxaFactor <- factor(taxa)

#combine all the values for the number of species into an object called richness
richness <- c(numBeetleSpecies, 
            numBirdSpecies, 
            numButterflySpecies, 
            numDragonflySpecies, 
            numFlowering.PlantsSpecies, 
            numFungusSpecies,
            numHymenopteranSpecies, 
            numLichenSpecies, 
            numLiverwortSpecies, 
            numMammalSpecies, 
            numMolluscSpecies)

# create the data frame from the two vectors
biodata <- data.frame(taxaFactor, richness)

# save this to a new .csv file (never want to overwrite your original data)
write.csv(biodata, file="biodata.csv") # file will be saved in your working directory

# make new barplot using the data frame (since data frames can contain multiple variables, we need to tell R
# exactly which one we want it to plot. like before, we can specify columns from a data frame using $)
# (will print same info as before tho)
barplot(biodata$richness, names.arg=c("Beetle",
                                      "Bird",
                                      "Butterfly",
                                      "Dragonfly",
                                      "Flowering.Plants",
                                      "Fungus",
                                      "Hymenopteran",
                                      "Lichen",
                                      "Liverwort",
                                      "Mammal",
                                      "Mollusc"),
        xlab="Taxa", ylab="Number of Species", ylim=c(0,600))
# or could've done:
# png("barplot2.png", width=1600, height=600)
# barplot(biodata$richness, names.arg=c(Beetle",
#         "Bird",
#         "Butterfly",
#         "Dragonfly",
#         "Flowering.Plants",
#          "Fungus",
#          "Hymenopteran",
#          "Lichen",
#         "Liverwort",
#         "Mammal",
#         "Mollusc"),
#   xlab="Taxa", ylab="Number of species", ylim=c(0,600))
#dev.off()