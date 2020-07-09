# coding club workshop challenge 1 - wingspan challenge
# given a .csv file w/ values of wingspan in cm measured on 4 different species
# of birds, produce a bar plot of the mean wingspan for each species and save it
# to my computer.
# written by Molly Johnson, Bradley University/Oregon State University

# load packages
library(dplyr)

setwd("C:/Users/mljoh/github/CC-RBasics-master")

# import .csv data into an object
wingspanObj <- read.csv("C:/Users/mljoh/github/CC-RBasics-master/wingspanChallenge.csv")

# confirm that the data was imported without mistakes
head(wingspanObj) # displays first few rows
tail(wingspanObj) # displays last few rows
str(wingspanObj) # tells you whether the variables are continuous, integers, categorical, or chars

head(wingspanObj$bird_sp) # displays first few rows of this column only
class(wingspanObj$bird_sp) # tells you what type of variable it is (it's char but i need factor)

head(wingspanObj$wingspan) #displays first few rows of this column only
class(wingspanObj$wingspan) # tells you what type of variable it is (it's int but i need num)


# force bird_sp to be a factor
wingspanObj$bird_sp <- as.factor(wingspanObj$bird_sp)

# force wingspan to be a num
wingspanObj$wingspan <- as.numeric(wingspanObj$wingspan)

# confirm that bird_sp is now a factor
class(wingspanObj$bird_sp)

# confirm that wingspan is now numeric
class(wingspanObj$wingspan)

# more dataset exploration
dim(wingspanObj) # displays num of rows and cols
summary(wingspanObj) # gives you a summary of the data
summary(wingspanObj$bird_sp) # gives you a summary of that particular variable (col) in your dataset

#-----------------------------------------------------------------------------------------------------

# CALCULATE MEAN WINGSPAN (CM) BY SPECIES
# split wingspanObj into multiple objects, each containing rows for only one species using the
# filter() function from the dplyr package

# first arg of the function is the dataframe, second arg is the condition you want to filter on
eagle <- filter(wingspanObj, bird_sp == "eagle")
hummingbird <- filter(wingspanObj, bird_sp == "hummingbird")
kingfisher <- filter(wingspanObj, bird_sp == "kingfisher")
sparrow <- filter(wingspanObj, bird_sp == "sparrow")

# sum the wingspans for each of the species
eagleWspanSum <- sum(eagle$wingspan)
hummingbirdWspanSum <- sum(hummingbird$wingspan)
kingfisherWspanSum <- sum(kingfisher$wingspan)
sparrowWspanSum <- sum(sparrow$wingspan)

# get num of each species from the dataset
numEagles <- length(eagle$bird_sp)
numHummingbirds <- length(hummingbird$bird_sp)
numKingfishers <- length(kingfisher$bird_sp)
numSparrows <- length(sparrow$bird_sp)

# calculate mean wingspan for each species (wspan sum for each / num of each species present in dataset)
eagleWspanMean <- eagleWspanSum / numEagles
hummingbirdWspanMean <- hummingbirdWspanSum / numHummingbirds
kingfisherWspanMean <- kingfisherWspanSum / numKingfishers
sparrowWspanMean <- sparrowWspanSum / numSparrows

# create dataframe for the new data
# data frames are tables of values, having 2-dimensional structure w rows and columns, where each
# col can have a different data type. vectors, remember, are only appropriate when dealing w just
# one set of values

# create an object called "species" that contains all species names
species <- c("eagle",
             "hummingbird",
             "kingfisher",
             "sparrow")

# turn this object into a factor, i.e. a categorical variable
speciesFactor <- factor(species)

# combine the num of each species into an object called numSpecies
numSpecies <- c(numEagles, numHummingbirds, numKingfishers, numSparrows)

# combine the sum of each species' wingspan into an object called wingspanSum
wingspanSum <- c(eagleWspanSum, hummingbirdWspanSum, kingfisherWspanSum, sparrowWspanSum)

# combine the mean of each species wingspan into an object called wingspanMean
wingspanMean <- c(eagleWspanMean, hummingbirdWspanMean, kingfisherWspanMean, sparrowWspanMean)

# create the data frame from the 4 vectors
birdWspanData <- data.frame(speciesFactor, numSpecies, wingspanSum, wingspanMean)

# save this to a new .csv file (never want to overwrite your original data)
write.csv(birdWspanData, file="birdWspanData.csv") # file will be saved in your working directory

# create barplot of species and mean wingspan. since using data frames can contain multiple 
# variables, we need to tell R exactly which ones we want it to plot.
barplot(birdWspanData$wingspanMean, names.arg=c("eagle",
                                                "hummingbird",
                                                "kingfisher",
                                                "sparrow"),
        xlab="Species", ylab="Mean Wingspan (cm)", ylim=c(0,200), col = "blue")

# write it to a .png file automatically
png("wingspanPlot.png", width=1600, height=600)
barplot(birdWspanData$wingspanMean, names.arg=c("eagle",
                                                "hummingbird",
                                                "kingfisher",
                                                "sparrow"),
        xlab="Species", ylab="Mean Wingspan (cm)", ylim=c(0,200), col = "blue")
dev.off()