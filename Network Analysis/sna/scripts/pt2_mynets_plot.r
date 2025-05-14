#####################################
# Social Network Analysis Workshop  #
# Part 2: Initial Steps             #
# by Scott Leo Renshaw              #
#####################################

library(statnet)
library(sna)
load("data/florentine.RData")


#
#-Create Your Own Network-------------------------------------------------------
#

# Example usage:
# Assuming your CSV file is stored at "data/ego_network.csv"
mynet <- read.csv("data/ego_network.csv", header = TRUE, row.names = 1,
                  stringsAsFactors = FALSE, check.names = FALSE)

# Plot the network with vertex labels.
gplot(mynet, displaylabels = TRUE)

mynet # Looks about right, however there's an issue with the first column.
View(mynet) # Opens data in new tab to inspect, good for large datasets!

mynet[,1]  # Using bracket notation to subset the first column, [,1] returns 
# column 1.

class(mynet)

mynet <- as.matrix(mynet) # Convert to matrix data type.

class(mynet)

# Going forward we will start using the network type.

mynet <- as.network(mynet,directed=FALSE) # Convert to network
class(mynet) # Network data type.

# Did anything change?

# Inspect mynet network object.
summary.network(mynet) # Has built in features that can store 
# more information about the network.

# Save
save(mynet,file="r_mynet") 

#
#-Built-in datasets-------------------------------------------------------------
#

data(package="network")     # List available datasets in network package.
data(flo)                   # Load a built-in dataset
flo                         # Examine the flo adjacency matrix.

# Inspect class.
class(flo) # What kind of dataset is it?
?flo # More information on Florence Wedding Dataset.

# Plot the Florence Dataset
gplot(flo)

?gplot # Find out more that goes into gplot function. 

# Simple Visualization Changes to try:
# Use commas to separate arguments 
gplot(flo, displaylabels = TRUE) #like this!

# Your turn:
# Try adding vertex.cex = some number, vertex.col = "some color" (pick a color)


#
#-Description and visualization-------------------------------------------------
#

nflo <- as.network(flo, directed=FALSE) # Convert flo to network flo object.

network.dyadcount(nflo)                 # Count the dyads.
network.edgecount(nflo)                 # Count the edges.
network.size(nflo)                      # How large is the network?
as.sociomatrix(nflo)                    # Back out the Sociomatrix
nflo[,]                                 # Another way to do it
as.edgelist.sna(nflo)                   # sna edgelist form

# Lets try different layouts...

# Default: Fruchterman & Reingold Force-Directed 
gplot(nflo,displaylabels=T,mode="fruchtermanreingold")  

# Circle Graph (Not very useful...)
gplot(nflo,displaylabels=T,mode="circle")               

# Kamada & Kawai Force-Directed
gplot(nflo,displaylabels=T,mode="kamadakawai")  

# So RANDOM!
gplot(nflo,displaylabels=T,mode="random") 

