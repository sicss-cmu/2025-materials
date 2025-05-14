#####################################
# Social Network Analysis Workshop  #
# Part 6: Overview of Advanced      #
# Statistical Tests                 #
# by Scott Leo Renshaw              #
#####################################

library(statnet)
library(sna)

#
#-Network regression------------------------------------------------------------
#

# Network regression (netlm) should only be used in valued network cases.
# Valued networks are networks where the i,jth cell represents a value (like 
# number of trades between partners, number of calls between actors in a radio
# network...). Can handle dependency from the past, but not within network dependency.

# Data needs to be supplied in the form of a matrix the same dimensions at the network.

# Read ?netlm in more detail to see discussion of issues with autocorrelation in 
# network data, etc.

# We will use Drabek's (1981) Search and Rescue Dataset
# EMON: Emergency Multi-Organizational Networks. We will look at the Cheyene

data(emon) # load data from network package

gplot(emon) # What's it look like.

# Frequencies is currently coded 4 as most infrequent, and 1 as most frequent.
Y <- as.sociomatrix(emon[[1]], "Frequency")     # Pull out the frequencies
# Also, emon[[1]] is pulling out the first sub list in the EMON network. 

# Recode frequencies so higher is more frequent.
Y[Y>0] <- 5-Y[Y>0]                              
# Where Y is > 0, we want to take 5 (4 is the largest value), and flip the coding.
# 1s are now 4s, and 4s are 1s. 

# Pull out covariate information from the dataset.
crk <- emon[[1]] %v% "Command.Rank.Score"           # Command rank
spon <- emon[[1]] %v% "Sponsorship"                 # Type of organization

# Create predictor matrices from the covaraites.
Xcr<-sapply(crk,rep,length(crk))            # Column effects for command rank
Xcr

Xsp<-outer(spon,spon,"!=")                  # Dyadic effect for type difference
Xsp

# Fit the model (takes a while to perform QAP test)
emonfit <- netlm(Y,list(Xcr,Xsp))
summary(emonfit)                              # Examine the results

# For more information....
?outer
?netlm

#
#-Logistic network regression---------------------------------------------------
#

# Network logistic regression (netlogit) can be used in cases of dichotomous
# network data (or categorical). Can handle dependence on the past network
# structures, but cannot handle within network dependency.
# (ERGMs handle within and past dependence if thats what you're interested in) 

# Let's leverage logistic regression to help us better understand factors that
# influence the likelihood of strategic family marriage in the Florentine Families
# data.

load("data/florentine.RData")


# More information on these functions.
?netlogit
?outer


# Construct various covariates based on wealth and 
# priorates (# of seats held on the civic council between 1282 -1344)
wealthsum<-outer(flomarriage%v%"wealth",flomarriage%v%"wealth","+")
wealthdiff<-abs(outer(flomarriage%v%"wealth",flomarriage%v%"wealth","-"))
priorsum<-outer(flomarriage%v%"priorates",flomarriage%v%"priorates","+")
priordiff<-abs(outer(flomarriage%v%"priorates",flomarriage%v%"priorates","-"))


# Run netlogit with our new covariates to see marginal effects for each.

flo.b <- netlogit(flomarriage,flobusiness,mode="graph") 
flo.ws <- netlogit(flomarriage,wealthsum,mode="graph")  
flo.wd <- netlogit(flomarriage,wealthdiff,mode="graph")
flo.ps <- netlogit(flomarriage,priorsum,mode="graph")
flo.pd <- netlogit(flomarriage,priordiff,mode="graph")

# Inspect models to see what the marginal effects are
flo.b  # Business Relations
flo.ws # Wealth Sum
flo.wd # Wealth Diff
flo.ps # Seats Held Sum
flo.pd # Seats Held Diff 

# Let's try them all now. This can take a few seconds...
flo.all<-netlogit(flomarriage,list(flobusiness,wealthsum,wealthdiff,priorsum,
                                   priordiff),mode="graph")

# Inspect results using summary (you get more information this way)
summary(flo.all)  

# The contingency table we can see total fraction correct: .85
# Our fraction of predicted 0s is higher than 1s. (.87 compared to .6)
# Our false negative rate is .7, not great...

# One could model select based on AIC or BIC, but here we will take variables that
# are closest to significance (Business and Seats Held Sum).
flo.bps <- netlogit(flomarriage,list(flobusiness,priorsum),mode="graph")

# Inspect:
summary(flo.bps) # We now have a more parismonious model

# Compare AIC (less is better)
flo.bps$aic < flo.all$aic # Nice!

# But in terms of contingency table, we still have a poor False Negative rate. 
# Something else is obviously going on, but through this model we can say that
# business and politics has measurable influence on the Renaissance Florence Families.


#
#-Basic ERG Fitting-------------------------------------------------------------
#

# Let's continue trying to understand the marriage dynamics of the Florentine 
# Families Network using more informative model.
data(florentine)

# One of the major benefits to using ERGM is that we not only can model likelihood
# of connections between actors in a network, but we can also add structural terms.

# Suggested method, inspect ergm formula before running:
summary(flomarriage~edges) # Tells you the number of edges in the network.
# Tells you whatever the number of that statistic is, good to inspect before.
# Helpful for mixingterms (homophily etc.) to make sure how many people you have
# in some categories.

# Help related ERGM Commands

search.ergmTerms(search='triangle')
search.ergmTerms(keywords=c('bipartite','dyad-independent'))

vignette("ergm-term-crossRef") # Helpful vignette for terms in ERGM and how to search for them
data(package='ergm') # tells us the datasets in the packages

#Examples:
# edges: The count of edges (ties) in the network. Baseline term in almost all ERGMs.
# Similar to an intercept term in regression, represents the baseline probability of a tie.
model <- ergm(network ~ edges)
# Negative coefficient = sparse network, Positive coefficient = dense network

# nodecov("attribute"): Models the effect of a continuous node attribute on tie formation.
# Higher values of the attribute increase/decrease the likelihood of ties.
model <- ergm(network ~ edges + nodecov("wealth"))
# Positive coefficient: nodes with higher values of the attribute form more ties

# nodefactor("attribute"): Similar to nodecov but for categorical attributes.
# Models the effect of belonging to a category on tie formation probability.
model <- ergm(network ~ edges + nodefactor("gender"))

# edgecov(matrix): Controls for the effect of a covariate defined on pairs of nodes.
# The matrix must have the same dimensions as the network.
model <- ergm(network ~ edges + edgecov(distance_matrix))

#
#-Homophily Terms--------------------------------------------------------------
#

# nodematch("attribute"): Models homophily effects (tendency to form ties with similar others)
# Counts the number of edges where both nodes have the same attribute value
model <- ergm(network ~ edges + nodematch("gender"))
# Positive coefficient: nodes tend to form ties with others of the same category

# absdiff("attribute"): Absolute difference in attributes between nodes in a dyad.
# For continuous attributes - measures how difference affects tie probability
model <- ergm(network ~ edges + absdiff("income"))
# Negative coefficient: nodes with similar values are more likely to connect

#
#-Structural Terms-------------------------------------------------------------
#

# mutual: Measures the tendency toward reciprocity in directed networks
# Counts the number of pairs of nodes with reciprocated ties
model <- ergm(network ~ edges + mutual)

# triangle: Counts the number of triangles in the network (transitive closure)
# WARNING: Often leads to degeneracy - gwesp is usually a better alternative
model <- ergm(network ~ edges + triangle)

# gwesp: Geometrically weighted edgewise shared partner distribution
# Models transitivity without the degeneracy problems of the triangle term
model <- ergm(network ~ edges + gwesp(decay=0.5, fixed=TRUE))
# Positive coefficient: evidence of triadic closure/clustering

# gwdsp: Geometrically weighted dyadwise shared partner distribution
# Counts shared partners for all dyads (not just tied ones)
model <- ergm(network ~ edges + gwdsp(decay=0.5, fixed=TRUE))

#
#-Degree-Based Terms-----------------------------------------------------------
#

# degree(k): Counts the number of nodes with exactly degree k
# Can specify a vector of degrees to include multiple degree statistics
model <- ergm(network ~ edges + degree(1))       # Nodes with exactly 1 tie
model <- ergm(network ~ edges + degree(0:3))     # Nodes with 0, 1, 2, or 3 ties

# gwdegree: Geometrically weighted degree distribution
# Models the entire degree distribution with a single parameter
model <- ergm(network ~ edges + gwdegree(decay=0.5, fixed=TRUE))

# isolates: Counts the number of isolated nodes (degree=0)
model <- ergm(network ~ edges + isolates)
# Positive coefficient: more isolated nodes than expected by chance

#
#-Advanced Structural Terms----------------------------------------------------
#

# kstar(k): Counts the number of k-stars in the network
# A k-star is a node with k neighbors
model <- ergm(network ~ edges + kstar(2))        # Count 2-stars
model <- ergm(network ~ edges + kstar(2:3))      # Count 2-stars and 3-stars

# twopath: Counts the number of 2-paths (i->j->k, where i!=k)
model <- ergm(network ~ edges + twopath)

# cyclicalties: Counts the number of cyclic triads (i->j->k->i)
# Only applicable for directed networks
model <- ergm(network ~ edges + cyclicalties)

#
#-Constraints and Fixed Edges--------------------------------------------------
#

# offset(edges): Include ties that are fixed and won't be estimated
# Useful when certain ties are mandatory in the network
model <- ergm(network ~ edges + offset(edgecov(fixed_ties)))
# Often used in the context of edges that people WON'T have. 
# A and B did not have the opportunity to tie, therefore, set to 0.


#
#-Nodal Attribute Interactions-------------------------------------------------
#

# nodemix("attribute"): Interaction effects between different categories of an attribute
# Refined version of nodematch that models mixing between different groups
model <- ergm(network ~ edges + nodemix("race"))

# For more information, see:
?"ergm-terms"

#Look at the Flomarriage dataset

par(mfrow=c(1,2)) # Set up a 2-column (and 1-row) plot area

# Plot the network, saving the coordinates for the next plot
coords <- plot(flomarriage, 
     main="Florentine Marriage", 
     cex.main=0.8, 
     label = network.vertex.names(flomarriage), 
     label.cex=0.4,
     pad=3) # Equivalent to plot.network(...)

wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes, equivalent to get.vertex.attribute(flomarriage, "wealth")

wealth

par(mfrow=c(1,1)) # Default

# Plot with vertex size proportional to wealth
plot(flomarriage, coord=coords, jitter=FALSE,
     vertex.cex=wealth/25, 
     main="Florentine marriage by wealth", cex.main=0.8,
     pad=0.5) 

 # Most basic model: a lone edges term
flom.edges <- ergm(flomarriage~edges) # Data ~ covariates

# Inspect Model.
flom.edges
summary(flom.edges)      
                                     # Get a summary
# The coefficient is -1.61

# This means:
# 1. The log-odds of any tie existing is -1.61
# 2. The coefficient is the same for all potential ties in the network
# 3. The δ_ij(y) change statistic equals 1 for all i,j pairs because adding 
#    any tie increases the edges count by exactly 1

# Why is δ_ij(y) = 1 for all i,j pairs?
# - The edges statistic simply counts ties
# - Adding any single tie increases this count by exactly 1
# - Therefore, the change statistic is always 1, regardless of which tie we add

#
#-Converting Log-odds to Probability-------------------------------------------
#

# To convert the log-odds coefficient to a probability, use the inverse logit:
# p = exp(coefficient) / (1 + exp(coefficient))
# For our -1.61 coefficient:
p = exp(-1.61) / (1 + exp(-1.61))

# In R, we can calculate this using plogis():
plogis(-1.61)  # Should return approximately 0.167

# This probability corresponds to network density - the proportion of possible
# ties that actually exist. For the flomarriage network:
# - 20 observed ties
# - (16 choose 2) = (16 × 15)/2 = 120 possible ties
# - Density = 20/120 = 0.167
#
#-Practical Interpretation----------------------------------------------------
#

# For flom.edges with just an edges term:
# - Negative coefficient (-1.61): ties are less likely than random (sparse network)
# - Positive coefficient: ties are more likely than random (dense network)
# - The coefficient directly reflects the network's density via logit transformation
# - A coefficient of 0 would indicate a network with density = 0.5

# If we exponentiate the coefficient:
# odds = exp(-1.61) ≈ 0.2
# This means the odds of observing a tie are 0.2:1 (or 1:5)
# i.e., ties are 5 times less likely to be present than absent

#### Continuing ERGM Examples:

# Add wealth covariate
flom.ew<-ergm(flomarriage~edges+nodecov("wealth"))
summary(flom.ew)

# Triangles? Often a term you won't want to use for degeneracy reasons
flom.et<-ergm(flomarriage~edges+triangle)
summary(flom.et)

# With coefficients: edges = -1.69, triangles = 0.17

# The conditional log-odds becomes:
# log-odds = -1.69 × (change in edges) + 0.17 × (change in triangles)

# Unlike with just edges, the change statistic for triangles varies by tie:
# - A tie that creates 0 triangles: log-odds = -1.69
# - A tie that creates 1 triangle: log-odds = -1.69 + 0.17 = -1.52
# - A tie that creates 2 triangles: log-odds = -1.69 + 2×0.17 = -1.35

# Converting these to probabilities:
plogis(-1.69)# 0 triangles
plogis(-1.52)  # 1 triangle
plogis(-1.35)  # 2 triangles
# Note, usually Triangles term lead to degeneracy, GWESP is usually more sensible

# Calculating directly from flom.et object
plogis(coef(flom.et)[1] + (0:2) * coef(flom.et)[2])

# Business ties?
flom.ec<-ergm(flomarriage~edges+edgecov(flobusiness))
summary(flom.ec)

# Trying edges, low-order degree, and business ties
flom.edc<-ergm(flomarriage~edges+degree(0:1)+edgecov(flobusiness))
summary(flom.edc)

# What if we model the business ties in reference to marriage etc. 
flob.edc<-ergm(flobusiness~edges+degree(0)+edgecov(flomarriage)+
                 nodecov("wealth")+nodecov("priorates"))
summary(flob.edc)

# For more information....
?ergm
?"ergm-terms"

#-Goodness-of-Fit---------------------------------------------------------------
#

# Checking Goodness-of-fit information:
flom.edc.gof<-gof(flom.edc)      # Simulate gof from the model
flom.edc.gof
par(mfrow=c(2,2))
plot(flom.edc.gof)                # Plot the results

dev.off() # Usually good to dev.off() to delete plot when done viewing.
# Especially big ones like this...
par(mfrow=c(1,1))

#### Extra, to check for degeneracy...

#Example of Degenerate MCMC Diagnostics:
# Load the EMON data
data(emon)

# Try fitting a triangle model to the Cheyenne data
fit<-ergm(emon[[1]]~edges+triangle,   # Forcing MCMLE (it wont run otherwise)
          control=control.ergm(MCMLE.maxit=1))

# Check the simulation using mcmc.diagnostics
mcmc.diagnostics(fit)      # Result is degenerate!

#Check goodness of fit...
emon.gof<-gof(fit)
par(mfrow=c(3,2))
plot(emon.gof) # More like badness of fit!

