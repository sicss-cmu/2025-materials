#####################################
# Social Network Analysis Workshop  #
# Part 4: Exploratory Analyses      #
# by Scott Leo Renshaw              #
#####################################

library(statnet)
library(sna)

load("data/wdcutternet")
load("data/florentine.RData")


#
#-Exploratory Analyses----------------------------------------------------------
#

# While it's nice to visualize the networks, we'd like to know something more 
# substantial about what's going on. Let's conduct some qualitative, exploratory
# analyses of the Bulgarian Woodcutter's data. 


#
#-Centrality Measures-----------------------------------------------------------
#

# We may want to know who plays a key role, who bridges more diverse ties, etc.

# One key concept for networks is that of centrality. It can often indicate who
# is playing a key role in the network. Nodes with high centrality may be more
# influential, have more control, or might have greater access to information.

# Lets look at a few:

# Degree Centrality
?degree #in degree, out degree, and Freeman centrality (both in and out degree)
wd.deg <- degree(wdcutters,cmode="indegree") # Who was selected most frequently?
wd.deg

# If we turn this into a factor (think category) we can see the breakdown:
summary(as.factor(wd.deg)) # One person was chosen 14 times, two 7, and six 5 times.

# Out degree will be uninteresting as the researcher only allowed people to choose 
# 3 alters.
degree(wdcutters,cmode="outdegree") # Almost all used all of there 3 votes. 


# Betweeness Centrality
?betweenness # Shortest-path betweenness of a vertex, high-betweenness actors 
# lie on a large number of non-redundant shortest paths between other actors.
# These actors can be thought of as "bridges" or "boundary spanners."

wd.bet <- betweenness(wdcutters) 
wd.bet

# Eigenvector Centrality
?evcent # Can be interpreted as arising from a reciprocal process in which the 
# centrality of each actor is proportional to the sum of the centralities of the 
# actors they are connected to.

wd.eigen <- evcent(wdcutters)
wd.eigen

# Now we might like to know, based on these centrality measures which actors are
# in the top 5?

# Some new R concepts:
# We can order vectors by decreasing or increasing order.
order(wd.deg,decreasing=TRUE) # Gives us which vector numbers had the
                              # highest in degree centrality

# Using head() will give us the top n items in that vector.
head(order(wd.deg,decreasing=TRUE),n=5) # useful as a way to preview data...

# If we combine with cbind (column bind) we will get a table of the top 5 actors
# per centrality measure.

wd.cent <- cbind(head(order(wd.deg,decreasing=TRUE),n=5),
                 head(order(wd.bet,decreasing=TRUE),n=5),
                 head(order(wd.eigen,decreasing=TRUE),n=5)) 
# Make sure to use commas in between!

wd.cent 

# Let's add column titles:
# c() is a vector of elements, in this case 3 column titles.
colnames(wd.cent) <- c("In-Degree","Betweenness","Eigen-vector")
  
wd.cent 

# What is this table telling us? What have we learned from this approach?
# Little overlap between measures.

# Leveraging something we know about the data, Obstina appointed...
which(wdcutters%v%"obstina.appointed"==1)

#[1] 4 7

# None of the Obstina appointees are in the top centrality measures.


#
#-Reciprocity & The Dyad Census-------------------------------------------------
#

# Load Sampson Monastery Data
load("data/sampson.Rdata")

# Look at the network data summary:

summary(sampson) # Many networks!!!

par(mfrow=c(2,2)) # Lets plot some of them:

#Temporal Network Data:
gplot(sampson$LikeT1, main="Like Time 1")  # In R we use the $ symbol followed
gplot(sampson$LikeT2, main="Like Time 2")  # by the sublist name to extract it. 
gplot(sampson$LikeT3, main="Like Time 3")
dev.off()


par(mfrow=c(2,2)) # Reset plotting parameters
# Networks of other relations
gplot(sampson$Dislike, main="Dislike") 
gplot(sampson$Blame, main="Blame") 
gplot(sampson$Disesteem, main="Disesteem")
gplot(sampson$Praise, main="Praise")


par(mfrow=c(1,1)) # Don't forget to reset parameters...

# Get Dyadic Reciprocity 
grecip(sampson)

# Several relations have identical indices. The dyad census can tell us why...
dc <- dyad.census(sampson)       # Get dyad census
dc                             # Several have same # Asyms


# (Number of Mutual Ties + Number of Null Ties) / Total Ties
(dc[,1]+dc[,3])/rowSums(dc) == grecip(sampson) # Math to show how to calculate
                                               # Dyadic Reciprocity.
# Proportion of ties that are symmetric. 

# Edgewise Reciprocity (Tends to be more useful)
grecip(sampson,measure="edgewise")      # Calculate the measure

# 2 * Mutual / (2 * Mutual + Asymmetric Ties)
2*dc[,1]/(2*dc[,1]+dc[,2])              # Show it the "hard way"
# Tells us of all ties sent, the proportion reciprocated.

# Compare edgewise reciprocity to density 
log(grecip(sampson,measure="edgewise")/gden(sampson))

#
#-Hierarchy and centralization--------------------------------------------------
#

# Hierarchy is the flip side of reciprocity.
hierarchy(sampson)                       # Default measure: 1-dyadic reciprocity
1-grecip(sampson)                        # Manually confirm

hierarchy(sampson,measure="krackhardt")  # Krackhardt's non-local measure
?hierarchy # Inspect the meaning for Krackhardt's reciprocity.

# Visualize the difference between 
plot(hierarchy(sampson),hierarchy(sampson,measure="krackhardt"))

# What about graph centralization?
centralization(sampson,degree,cmode="indegree")    #Indegree centralization
centralization(sampson,degree,cmode="outdegree")   #Outdegree centralization
centralization(sampson,betweenness)                #Betweenness centralization

#
#-Transitivity and the triad census---------------------------------------------
#

# Now, let's get the triad census for each network 
tc <- triad.census(sampson)
tc

# Learn more about the triad census:
?triad.census

# Transitivity of a relation means that when there is a te from i to j, and a
# tie from j to k, that there is also a tie from i to k.
# "A friend of my friend is my friend"

# Get transitivity for the Sampson monastery data
gtrans(sampson)

# How does transitivity compare to density? (Using log-odds)
log(gtrans(sampson)/gden(sampson))







