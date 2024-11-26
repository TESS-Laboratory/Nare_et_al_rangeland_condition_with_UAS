# Coding Club Workshop 1 - R Basics
# Learning how to import and explore data, and make graphs about Edinburgh's biodiversity
# Written by Alan Nare
library(dplyr)
setwd("C:/Users/202200875/OneDrive - buan.ac.bw/Desktop/R2024")
head(edidiv$taxonGroup)     # Displays the first few rows of this column only
class(edidiv$taxonGroup)    # Tells you what type of variable we're dealing with: it's character now but we want it to be a factor

edidiv$taxonGroup <- as.factor(edidiv$taxonGroup)     # Converting character to factor

# More exploration
dim(edidiv)                 # Displays number of rows and columns
summary(edidiv)             # Gives you a summary of the data
summary(edidiv$taxonGroup)  # Gives you a summary of that particular variable (column) in your dataset

Beetle <- filter(edidiv, taxonGroup == "Beetle")
# The first argument of the function is the data frame, the second argument is the condition you want to filter on. Because we only want the beetles here, we say: the variable taxonGroup MUST BE EXACTLY (==) Beetle - drop everything else from the dataset. (R is case-sensitive so it's important to watch your spelling! "beetle" or "Beetles" would not have worked here.)

Bird <- filter(edidiv, taxonGroup == "Bird")   # We do the same with birds. It's very similar to filtering in Excel if you are used to it.
# You can create the objects for the remaining taxa. If you need to remind yourself of the names and spellings, type summary(edidiv$taxonGroup)
Butterfly <- filter(edidiv,taxonGroup == "Butterfly")
Dragonfly <- filter(edidiv,taxonGroup == "Dragonfly")
Flowering.Plants <- filter(edidiv,taxonGroup == "Flowering.Plants")
Fungus <- filter(edidiv,taxonGroup == "Fungus")
Hymenopteran <- filter(edidiv,taxonGroup == "Hymenopteran")
Lichen <- filter(edidiv,taxonGroup == "Lichen")
Liverwort <- filter(edidiv,taxonGroup == "Liverwort")
Mammal <- filter(edidiv,taxonGroup == "Mammal")
Mollusc <- filter(edidiv,taxonGroup == "Mollusc")

##### Species richness######
a <- length(unique(Beetle$taxonName))
b <- length(unique(Bird$taxonName))
c <- length(unique(Butterfly$taxonName))
d <- length(unique(Dragonfly$taxonName))
e <- length(unique(Flowering.Plants$taxonName))
f <- length(unique(Fungus$taxonName))
g <- length(unique(Hymenopteran$taxonName))
h <- length(unique(Lichen$taxonName))
i <- length(unique(Liverwort$taxonName))
j <- length(unique(Mammal$taxonName))
k <- length(unique(Mollusc$taxonName))

biodiv <- c(a,b,c,d,e,f,g,h,i,j,k)     # We are chaining together all the values; pay attention to the object names you have calculated and their order
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
barplot(biodiv)
png("barplot.png", width=1600, height=600)  # look up the help for this function: you can customise the size and resolution of the image
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5)
dev.off()
# The cex code increases the font size when greater than one (and decreases it when less than one). 
