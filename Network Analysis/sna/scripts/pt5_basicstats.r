#####################################
# Social Network Analysis Workshop  #
# Part 5: Basic Statistical Tests   #
# by Scott Leo Renshaw              #
#####################################

library(statnet)
library(sna)

load("data/wdcutternet")
load("data/florentine.RData")


#
#-Simple Univariate CUG Tests---------------------------------------------------
# 

# Conditional Uniform Graph (CUG) tests graph level indices (graph-level 
# reciprocity, transitivity etc.) against a conditional uniform graph null 
# hypothesis. Can generate conditional uniform graphs in reference to size,
# density, and dyad census. 

# Let's test with the woodcutters data to see whether the whether the number 
# of choices (density) is less than we'd expect of a network this size.
# (Hint: since we know the choice was constrained to 3 alters, it should be less...)

load("data/wdcutternet")

cug.test(wdcutters,gden,cmode="size") # Call cug.test with the gden (density)
                                      # function;

# Plot:
plot(cug.test(wdcutters,gden,cmode="size")) # Shows that indeed, our network
# Is much more sparse than we would otherwise expect of networks of that size.


# Is our observed network more reciprocal than random graphs with similar density?
cug.test(wdcutters,grecip,cmode="edges")  # Conditioning on edges, calling grecip

# Plot:
plot(cug.test(wdcutters,grecip,cmode="edges")) # When we control for density
# we find that the network is more reciprocal than we would expect at random.

# Since we're seeing clear bias in one direction for size and another for density,
# Let's see if there's a bias in transitivity once we control for those factors.

# Write a code conducting a cug.test using the gtrans function, and cmode = "dyad"
# Using cmode = "dyad" is a test against networks controlling for size and density



# Plot that result:


# There are also other more complex commands you can test using this method.
# Find out more using:
?cug.test

# Conditional Uniform Graph (CUG) tests evaluate whether an observed network
# statistic (like density, reciprocity, or transitivity) differs significantly
# from what would be expected by chance, conditional on some fixed property.
# These tests work by generating random networks that preserve specific network
# properties (size, density, or dyad census) and comparing the observed statistic
# against the distribution of that statistic across random graphs.
#
# The cmode parameter determines what is held constant:
# - cmode="size" - holds number of nodes constant
# - cmode="edges" - holds number of edges (density) constant 
# - cmode="dyad" - holds the dyad census constant (# of mutual, asymmetric, null dyads)
#
# CUG tests help determine whether observed network properties (like high
# transitivity) might simply arise from more basic properties like 
# network density.


#
#-Graph correlation and bivariate QAP-------------------------------------------
#

# Let's load the Florentine families data
data(florentine)
gplot(flobusiness)                 # Examine business ties
gplot(flomarriage)                 # Examine marriage ties

# How related are these two graphs? Use graph correlation.
gcor(flobusiness,flomarriage) 
# 37% correlation between Florence Marriage Network and the Florence Business Network

# To test this correlation, we can do a Quadratic Assignment Procedure, QAP test.

# Make a list of these two networks (a network stack)
flonets <- list(flobusiness,flomarriage) 
flonets # See what the network stack (list of networks) looks like.

#Conduct QAP test.
flo.qap1 <- qaptest(flonets,gcor,g1=1,g2=2)
summary(flo.qap1)                   # Look at the results...
plot(flo.qap1)                      # Plot the QAP distribution

###### To Try later! EXTRA
# Let's test using covariates...
# First: wealth, sapply replicates the covariate wealth the same size as the network.
wealth <- sapply(flomarriage%v%"wealth",rep,network.size(flomarriage))
wealth # Wealth per family, as a valued network.

# Second: wealthdiff, the absolute value difference between two families.
wealthdiff <- abs(outer(flomarriage%v%"wealth",flomarriage%v%"wealth","-"))
wealthdiff # What's it look like.

flo.qap2 <- qaptest(list(flomarriage,wealth),gcor,g1=1,g2=2)
flo.qap3 <- qaptest(list(flomarriage,wealthdiff),gcor,g1=1,g2=2)

summary(flo.qap2)               # Do wealthy families have more ties?
plot(flo.qap2)                 
summary(flo.qap3)               # Is there a wealth difference effect?
plot(flo.qap3)

##### END EXTRA

# For more information on the functions we just used...
?qaptest
?gcor
?outer
?sapply
?rep

# Quadratic Assignment Procedure (QAP) tests examine the correlation between 
# two networks while accounting for the inherent dependencies in network data. 
# QAP works by repeatedly permuting the rows and columns of one network matrix 
# and recalculating the correlation with the other network. This creates a null 
# distribution of correlations that would be expected by chance.
#
# The significance of the observed correlation is determined by its position
# in this null distribution. If the observed correlation falls in the extreme
# tails of the null distribution, we can reject the null hypothesis of no 
# relationship between the networks.
#
# QAP is particularly useful for testing:
# - Relations between different types of ties (e.g., friendship vs business)
# - Effects of node attributes on tie formation
# - Effects of dyadic attributes (like similarity measures) on tie formation
#
# QAP maintains the dependency structure within each network, making it more
# appropriate for network data than standard correlation tests which assume
# independence of observations.