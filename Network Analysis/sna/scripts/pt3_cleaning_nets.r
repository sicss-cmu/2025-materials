#####################################
# Social Network Analysis Workshop  #
# Part 3: Managing Network Data     #
# by Scott Leo Renshaw              #
#####################################

library(statnet)
library(sna)


#
#-Cleaning & Managing Network Data----------------------------------------------
#

# Load in Irwin T. Sander's 1939 Sociometric Work with a 
# Bulgarian Woodcutting Group data.

#Load in linux, uncomment:
load("data/wdcutters") #Load R data object wdcutter

# Load in Windows/Mac:
load("data/wdcutters.gz") # Load R Data Object wdcutter


# Inspect the network object
summary(wdcutters) # Network object of Woodcutters

# What's it look like?
gplot(wdcutters)

# Inspect the other objects...

familyvec # vector of family affiliation

obstina       # Vector of whether actor was elected by the Obstina,
              # basically governmental appointees by the Mayor of Sofia.


#
#-Adding Vertex Attributes------------------------------------------------------
#

# Here's how you add a vertex attribute to a Network object
set.vertex.attribute(wdcutters,"obstina.appointed",obstina)

# This is the short hand operation for setting vertex attribute. 
wdcutters %v% "obstina.appointed" <- obstina     # Pass the vector to what you 
                                                 # want it named.

# Long hand way to get attribute
get.vertex.attribute(wdcutters,"obstina.appointed") # Inspect to see it's under vertex attributes.

# Short hand
wdcutters %v% "obstina.appointed" # You get your vector back.
 
# Your turn:
# Add the family vector as a vector using the same approach;
# name the attribute "family"



# See if it worked:
# List all vertex attributes
list.vertex.attributes(wdcutters)

# What is "na"?

# Inspect: 
wdcutters %v% "na"

# Seems to be erroneous, let's remove it!
delete.vertex.attribute(wdcutters,"na")

# List all vertex attributes
list.vertex.attributes(wdcutters) # Success!

#
#-Visualizing Vertex Attributes-------------------------------------------------
#

# Let's save the plot coordinates so that it stays still...

coords <- gplot(wdcutters) # Sets coords to object.

# Change plot, add family as label.
gplot(wdcutters, label.cex=1,
      label.col=1,label= wdcutters%v%"family", coord = coords)

# Let's try to color nodes who were appointed by the Mayor of Dragalevtsy

wdcutters %v% "obstina.appointed" # 1 represent appointed...

# We can use an ifelse statement (Might be an advanced approach, but lets try)
# IFELSE first evaluates a logical statement, if TRUE do the first argument, if
# FALSE do the second. 

ifelse(wdcutters %v% "obstina.appointed"==1,"yellow","red")

obstina.col <- ifelse(wdcutters %v% "obstina.appointed"==1,"yellow","red")

# Change plot, add obstina related coloring...
gplot(wdcutters, label.cex=0.5, vertex.col = obstina.col,
      label= wdcutters%v%"family", main= "1939 Bulgarian Woodcutter's Group",
      coord = coords)

#
#-Network Projection via Mixing Matrix------------------------------------------
#

# We can also create a mixing matrix to see the family x family network:

fam.mix <- mixingmatrix(wdcutters,"family")

# What does a mixing matrix look like?
fam.mix # It counts the sums per column and per row

# If we drop the last row and column (sum info), we can convert it to a graph
fam.mix <- fam.mix[-9,-9] # Subtracting row 9 and column 9 
                          # (negative removes that row/column)

gplot(fam.mix, displaylabels = TRUE, label.cex=0.5)

# Plot the two networks side by side!

par(mfrow=c(1,2)) # Changes parameter of how many plots, here: 1 row, 2 columns.

gplot(wdcutters, label.cex=0.5, vertex.col = obstina.col,
      label= wdcutters%v%"family", main="1939 Bulgarian Woodcutter's Group",
      coord = coords)
gplot(fam.mix, displaylabels = TRUE, label.cex=0.5,
      main="1939 Dragalevtsy Families")

par(mfrow=c(1,1)) # Change it back before you forget to!

# Don't forget to save our changes:
save(wdcutters,fam.mix,coords, file="data/wdcutternet")

# Trick for wiping your global environment clean:
# (uncomment if you want to try, just make sure you saved!)
# rm(list=ls())
# It provides a list of the objects in your environment and removes them all.


#
#-CMU Summer Institute CSS Network-------------------------------------------------
#
#install.packages("stringr","dplyr")
library(stringr)
library(dplyr)

# Load the SICSS participant data
sicss_data <- read.csv("data/sicss_all_people.csv", stringsAsFactors = FALSE)

# Examine the data
head(sicss_data)
table(sicss_data$role) # See distribution of roles

#
#-Define Key Terms to Extract---------------------------------------------------
#

# Define key terms relevant to computational social science
# These are the terms we'll look for in the bios
css_terms <- c(
  "network", "data", "computational", "social", "research", 
  "analysis", "machine learning", "ai", "policy", "visualization", 
  "modeling", "simulation", "ethics", "education", "psychology", 
  "sociology", "engineering", "design", "health", "economics", 
  "community", "digital", "software", "technology", "science",
  "faculty", "phd", "student", "library", "assistant", "teaching"
)

#
#-Extract Terms from Bios------------------------------------------------------
#

# Initialize a matrix to record which terms appear in which bios
term_matrix <- matrix(0, nrow = nrow(sicss_data), ncol = length(css_terms))
colnames(term_matrix) <- css_terms
rownames(term_matrix) <- sicss_data$name

# For each person and each term, check if the term appears in their bio
for (i in 1:nrow(sicss_data)) {
  bio_text <- tolower(sicss_data$bio[i])
  
  for (j in 1:length(css_terms)) {
    # Check if the term is present in the bio
    if (str_detect(bio_text, tolower(css_terms[j]))) {
      term_matrix[i, j] <- 1
    }
  }
}

# See which terms are most common
term_counts <- colSums(term_matrix)
term_summary <- data.frame(
  term = css_terms,
  count = term_counts
)
term_summary <- term_summary[order(term_summary$count, decreasing = TRUE),]
print("Most common terms in bios:")
print(head(term_summary, 10)) # Top 10 most common terms

# Keep only terms that appear in at least 3 bios
common_terms <- term_summary$term[term_summary$count >= 3]
term_matrix <- term_matrix[, common_terms]

# Examine our binary matrix
head(term_matrix)[,1:5] # Just looking at first 5 terms for first few people 

#
#-Creating the Term-Based Network-----------------------------------------------
#

# Calculate person-to-person similarity based on shared terms
# This matrix multiplication gives us counts of shared terms between individuals
similarity_matrix <- term_matrix %*% t(term_matrix)
diag(similarity_matrix) <- 0  # Remove self-connections

# Two people are connected if they share at least 2 terms
threshold <- 5
adjacency_matrix <- ifelse(similarity_matrix >= threshold, 1, 0)

# Convert to a network object
term_network <- as.network(adjacency_matrix, directed = FALSE)

# Add attributes to the network
term_network %v% "name" <- sicss_data$name
term_network %v% "role" <- sicss_data$role

# Inspect the network
summary(term_network)

#
#-Visualizing the Network-------------------------------------------------------
#

# Define colors based on role
role_colors <- c(
  "Faculty" = "blue",
  "Speakers" = "red",
  "Teaching Assistants" = "green",
  "Participants" = "orange"
)

# Create color vector based on roles
node_colors <- role_colors[term_network %v% "role"]

# Plot the network
coords <- gplot(
  term_network,
  gmode = "graph",
  displaylabels = FALSE,
  label.cex = 0.7,
  vertex.col = node_colors,
  main = "SICSS Bio Term Similarity Network"
)

# Add a legend
legend("bottomleft", 
       legend = names(role_colors),
       fill = role_colors, 
       cex = 0.8, 
       title = "Role")

# Try a different layout
gplot(
  term_network,
  gmode = "graph",
  displaylabels = FALSE,
  label.cex = 0.7,
  vertex.col = node_colors,
  main = "SICSS Bio Term Similarity Network (Kamada-Kawai Layout)",
  mode = "kamadakawai"
)

#
#-Network Analysis--------------------------------------------------------------
#

# Calculate basic network metrics
density <- gden(term_network)
cat("Network Density:", density, "\n")

# Calculate degree centrality
degree_cent <- degree(term_network, gmode = "graph")
degree_df <- data.frame(
  name = term_network %v% "name",
  role = term_network %v% "role",
  degree = degree_cent
)

# Find people with highest degree centrality
top_central <- degree_df %>%
  arrange(desc(degree)) %>%
  head(5)

print("Top 5 central individuals (most shared terms):")
print(top_central)

# Let's also try betweenness centrality
between_cent <- betweenness(term_network)
between_df <- data.frame(
  name = term_network %v% "name",
  role = term_network %v% "role",
  betweenness = between_cent
)

top_between <- between_df %>%
  arrange(desc(betweenness)) %>%
  head(5)

print("Top 5 individuals by betweenness centrality:")
print(top_between)


#
#-Role Mixing Analysis----------------------------------------------------------
#

# Look at connections between roles using a mixing matrix
role_mix <- mixingmatrix(term_network, "role")
print("Mixing matrix showing connections between roles:")
role_mix

# Create a role-level network from the mixing matrix
# Remove the sum row and column (the last row and column)
role_mix_network <- role_mix[-nrow(role_mix), -ncol(role_mix)]

# Plot the role mixing network
par(mfrow=c(1,2)) # Set up to show two plots side by side

# First, plot the original term network
gplot(
  term_network,
  gmode = "graph",
  displaylabels = FALSE,
  vertex.col = node_colors,
  main = "Individual Term Network",
  label.cex = 0.5
)

# Add a legend to the first plot
legend("bottomleft", 
       legend = names(role_colors),
       fill = role_colors, 
       cex = 0.6, 
       title = "Role")

# Second, plot the role mixing network
gplot(
  role_mix_network,
  displaylabels = TRUE,
  vertex.col = role_colors[rownames(role_mix_network)],
  label.cex = 0.9,
  main = "Role Mixing Network",
  edge.lwd = role_mix_network * .4  # Make edge thickness proportional to connection strength
)

# Reset the plot layout
par(mfrow=c(1,1))

# Save the network and coordinates
save(term_network, coords, role_mix_network, file = "data/sicss_term_network.RData")





       
