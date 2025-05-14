#####################################
# Social Network Analysis Workshop  #
# Part 7: Relational Event Models   #
# by Scott Leo Renshaw              #
#####################################

library(statnet)
library(sna)
library(relevent)  # Load the relevent library for REM analysis

# Load the workshop data
# (You may need to change directory)
load("data/relevent_workshop.Rata") 


#
#-Introduction to Relational Event Models-----------------------------------------
#

# Relational Event Models (REMs) capture behavior of systems in which individual
# social units (persons, organizations, animals, etc.) direct discrete actions 
# towards other individuals in their environment.

# Within the relevent package, the rem.dyad function is the primary workhorse for 
# modeling dyadic data.

# Data for REMs consists of dynamic edge lists, each edge being characterized by 
# a sender, a recipient, and an event time. In cases where the exact timing is unknown,
# the order of events can still be used (ordinal timing).


#
#-World Trade Center Police Radio Data------------------------------------------
#

# Let's examine the WTC Police radio communication data set 
# It consists of radio calls among 37 named communicants belonging to a 
# police unit at the World Trade Center complex on 9/11/2001.

# Examine the data:
head(WTCPoliceCalls, 10)  # Show first 10 rows of edge list

# Convert to sociomatrix for visualization
WTCPoliceNet <- as.sociomatrix.eventlist(WTCPoliceCalls, 37)

# Plot the network
gplot(WTCPoliceNet, edge.lwd = WTCPoliceNet^0.75, 
      vertex.col = 2 + WTCPoliceIsICR, vertex.cex = 1.25,
      main="WTC Police Radio Network")

# The ICR vector indicates whether nodes occupy Institutionalized Coordinative Roles
# Green (2+1=3) = ICR, Red (2+0=2) = non-ICR
table(WTCPoliceIsICR)  # Count of ICR vs non-ICR roles


#
#-Basic REM: ICR Effects--------------------------------------------------------
#

# Let's fit a simple model examining the effect of ICR roles on communication

# Model 1: Effect of ICR on total interaction
wtcfit1 <- rem.dyad(WTCPoliceCalls, n = 37, effects = c("CovInt"),
                    covar = list(CovInt = WTCPoliceIsICR), hessian = TRUE)

# Examine model results
summary(wtcfit1)

# Interpret coefficients as log multipliers for hazard rates
# Convert to relative hazards for easier interpretation
cat("Relative hazard for a non-ICR/ICR vs. a non-ICR/non-ICR event:", 
    exp(wtcfit1$coef), "\n")

cat("Relative hazard for an ICR/ICR vs. a non-ICR/non-ICR event:",
    exp(2 * wtcfit1$coef), "\n")

# Model 2: Separate effects for sending and receiving
wtcfit2 <- rem.dyad(WTCPoliceCalls, n = 37, 
                    effects = c("CovSnd", "CovRec"), 
                    covar = list(CovSnd = WTCPoliceIsICR, 
                                 CovRec = WTCPoliceIsICR),
                    hessian = TRUE)

summary(wtcfit2)

# Compare models using BIC (lower is better)
cat("BIC difference (Model 1 - Model 2):", wtcfit1$BIC - wtcfit2$BIC, "\n")
# Is Model 1 preferred? If difference is negative, yes


#
#-Conversational Dynamics in REMs-----------------------------------------------
#

# Radio communication follows conversational norms with systematic turn-taking
# We can test this with participation shifts (P-shifts)

# Model 3: Basic reciprocity via AB-BA shift
# AB-BA means B calls A after A called B (reciprocity)
wtcfit3 <- rem.dyad(WTCPoliceCalls, n = 37, 
                    effects = c("CovInt", "PSAB-BA"), 
                    covar = list(CovInt = WTCPoliceIsICR), 
                    hessian = TRUE)

summary(wtcfit3)

# Compare with Model 1
cat("BIC difference (Model 1 - Model 3):", wtcfit1$BIC - wtcfit3$BIC, "\n")
# If positive, Model 3 is preferred

# Model 4: More complex participation shifts
# AB-BY: B calls someone new after A called B
# AB-AY: A calls someone new after calling B
wtcfit4 <- rem.dyad(WTCPoliceCalls, n = 37, 
                   effects = c("CovInt", "PSAB-BA", "PSAB-BY", "PSAB-AY"), 
                   covar = list(CovInt = WTCPoliceIsICR),
                   hessian = TRUE)

summary(wtcfit4)

# Compare with Model 3
cat("BIC difference (Model 3 - Model 4):", wtcfit3$BIC - wtcfit4$BIC, "\n")


#
#-Recency Effects in Communication----------------------------------------------
#

# Do people tend to communicate with those they've recently interacted with?
# RRecSnd: Person tends to call those who recently called them
# RSndSnd: Person tends to call those they recently called

wtcfit5 <- rem.dyad(WTCPoliceCalls, n = 37, 
                   effects = c("CovInt", "PSAB-BA", "PSAB-BY", "PSAB-AY", 
                               "RRecSnd", "RSndSnd"), 
                   covar = list(CovInt = WTCPoliceIsICR),
                   hessian = TRUE)

summary(wtcfit5)

# Compare with Model 4
cat("BIC difference (Model 4 - Model 5):", wtcfit4$BIC - wtcfit5$BIC, "\n")


#
#-Preferential Attachment-------------------------------------------------------
#

# Does communication follow a "rich get richer" pattern?
# NTDegRec: Normalized Total Degree as Receiver - tests if people 
# with more overall communication are more likely to receive new calls

set.seed(13)  # For reproducibility
wtcfit6 <- rem.dyad(WTCPoliceCalls, n = 37, 
                   effects = c("CovInt", "PSAB-BA", "PSAB-BY", "PSAB-AY", 
                              "RRecSnd", "RSndSnd", "NTDegRec"),
                   covar = list(CovInt = WTCPoliceIsICR), 
                   hessian = TRUE)

summary(wtcfit6)

# Compare with Model 5
cat("BIC difference (Model 5 - Model 6):", wtcfit5$BIC - wtcfit6$BIC, "\n")


#
#-Model Comparison and Interpretation-------------------------------------------
#

# Create a table of BIC values for all models
bic_values <- c(wtcfit1$BIC, wtcfit2$BIC, wtcfit3$BIC, 
                wtcfit4$BIC, wtcfit5$BIC, wtcfit6$BIC)
model_names <- c("ICR Only", "ICR Send/Receive", "ICR + Reciprocity", 
                "ICR + P-shifts", "ICR + P-shifts + Recency", 
                "ICR + P-shifts + Recency + Pref. Attachment")
bic_table <- data.frame(Model = model_names, BIC = bic_values)
bic_table

# Final model interpretation:
# 1. What does PSAB-BA coefficient tell us about conversational norms?
cat("Reciprocity effect (exp(PSAB-BA)):", exp(wtcfit6$coef["PSAB-BA"]), "\n")

# 2. How much does ICR status affect communication rate?
cat("ICR effect (exp(CovInt)):", exp(wtcfit6$coef["CovInt.1"]), "\n")

# 3. How strong is preferential attachment?
cat("Preferential attachment effect (exp(NTDegRec)):", 
    exp(wtcfit6$coef["NTDegRec"]), "\n")

# For more information on available effects, see:
# ?rem.dyad

#
#-Prediction Rank Assessment--------------------------------------------------
#

# Another approach: examine the rank of observed events in predicted rate structure
# Rank 1 means the model correctly predicted the exact next event

# Distribution of ranks
hist(wtcfit6$observed.rank, breaks=50, main="Histogram of Observed Event Ranks",
     xlab="Rank (1 is best)", ylab="Frequency")

# Examine how many events were predicted with high precision (low rank)
cat("Percent of events ranked in top 5:", 
    mean(wtcfit6$observed.rank <= 5) * 100, "%\n")

# Plot the cumulative distribution function of ranks
# Shows what fraction of events are covered at different prediction thresholds
plot(ecdf(wtcfit6$observed.rank/(37 * 36)), 
     xlab = "Prediction Threshold (Fraction of Possible Events)",
     ylab = "Fraction of Observed Events Covered", 
     main = "Classification Accuracy")
abline(v = c(0.05, 0.1, 0.25), col = 2)  # Mark 5%, 10%, 25% thresholds

# Check exact matches - when model predicted exactly the right source/recipient
prediction_matches <- wtcfit6$predicted.match

# Overall match rate - when at least one of source or recipient was correct
cat("Fraction of events where at least source OR recipient correct:", 
    mean(apply(prediction_matches, 1, any)), "\n")

# Perfect match rate - when both source AND recipient were correct
cat("Fraction of events where BOTH source AND recipient correct:", 
    mean(apply(prediction_matches, 1, all)), "\n")

# Separate match rates for source and recipient
cat("Fraction correct for source:", colMeans(prediction_matches)[1], "\n")
cat("Fraction correct for recipient:", colMeans(prediction_matches)[2], "\n")

# Conclusion: Despite its simplicity, our model fits extremely well
# Further improvements are possible, but this model provides an adequate 
# representation of communication dynamics in the WTC police network

#####################################
# Social Network Analysis Workshop  #
# Part 7: Relational Event Models   #
# by Scott Leo Renshaw              #
#####################################

library(statnet)
library(sna)
library(relevent)  # Load the relevent library for REM analysis

# Load the workshop data
# (You may need to change directory)
load("data/relevent_workshop.RData") 


#
#-Introduction to Relational Event Models-----------------------------------------
#

# Relational Event Models (REMs) capture behavior of systems in which individual
# social units (persons, organizations, animals, etc.) direct discrete actions 
# towards other individuals in their environment.

# Within the relevent package, the rem.dyad function is the primary workhorse for 
# modeling dyadic data.

# Data for REMs consists of dynamic edge lists, each edge being characterized by 
# a sender, a recipient, and an event time. In cases where the exact timing is unknown,
# the order of events can still be used (ordinal timing).


#
#-World Trade Center Police Radio Data------------------------------------------
#

# Let's examine the WTC Police radio communication data set 
# It consists of radio calls among 37 named communicants belonging to a 
# police unit at the World Trade Center complex on 9/11/2001.

# Examine the data:
head(WTCPoliceCalls, 10)  # Show first 10 rows of edge list

# Convert to sociomatrix for visualization
WTCPoliceNet <- as.sociomatrix.eventlist(WTCPoliceCalls, 37)

# Plot the network
gplot(WTCPoliceNet, edge.lwd = WTCPoliceNet^0.75, 
      vertex.col = 2 + WTCPoliceIsICR, vertex.cex = 1.25,
      main="WTC Police Radio Network")

# The ICR vector indicates whether nodes occupy Institutionalized Coordinative Roles
# Green (2+1=3) = ICR, Red (2+0=2) = non-ICR
table(WTCPoliceIsICR)  # Count of ICR vs non-ICR roles


#
#-Basic REM: ICR Effects--------------------------------------------------------
#

# Let's fit a simple model examining the effect of ICR roles on communication

# Model 1: Effect of ICR on total interaction
wtcfit1 <- rem.dyad(WTCPoliceCalls, n = 37, effects = c("CovInt"),
                    covar = list(CovInt = WTCPoliceIsICR), hessian = TRUE)

# Examine model results
summary(wtcfit1)

# Interpret coefficients as log multipliers for hazard rates
# Convert to relative hazards for easier interpretation
cat("Relative hazard for a non-ICR/ICR vs. a non-ICR/non-ICR event:", 
    exp(wtcfit1$coef), "\n")

cat("Relative hazard for an ICR/ICR vs. a non-ICR/non-ICR event:",
    exp(2 * wtcfit1$coef), "\n")

# Model 2: Separate effects for sending and receiving
wtcfit2 <- rem.dyad(WTCPoliceCalls, n = 37, 
                    effects = c("CovSnd", "CovRec"), 
                    covar = list(CovSnd = WTCPoliceIsICR, 
                                 CovRec = WTCPoliceIsICR),
                    hessian = TRUE)

summary(wtcfit2)

# Compare models using BIC (lower is better)
cat("BIC difference (Model 1 - Model 2):", wtcfit1$BIC - wtcfit2$BIC, "\n")
# Is Model 1 preferred? If difference is negative, yes


#
#-Conversational Dynamics in REMs-----------------------------------------------
#

# Radio communication follows conversational norms with systematic turn-taking
# We can test this with participation shifts (P-shifts)

# Model 3: Basic reciprocity via AB-BA shift
# AB-BA means B calls A after A called B (reciprocity)
wtcfit3 <- rem.dyad(WTCPoliceCalls, n = 37, 
                    effects = c("CovInt", "PSAB-BA"), 
                    covar = list(CovInt = WTCPoliceIsICR), 
                    hessian = TRUE)

summary(wtcfit3)

# Compare with Model 1
cat("BIC difference (Model 1 - Model 3):", wtcfit1$BIC - wtcfit3$BIC, "\n")
# If positive, Model 3 is preferred

# Model 4: More complex participation shifts
# AB-BY: B calls someone new after A called B
# AB-AY: A calls someone new after calling B
wtcfit4 <- rem.dyad(WTCPoliceCalls, n = 37, 
                   effects = c("CovInt", "PSAB-BA", "PSAB-BY", "PSAB-AY"), 
                   covar = list(CovInt = WTCPoliceIsICR),
                   hessian = TRUE)

summary(wtcfit4)

# Compare with Model 3
cat("BIC difference (Model 3 - Model 4):", wtcfit3$BIC - wtcfit4$BIC, "\n")


#
#-Recency Effects in Communication----------------------------------------------
#

# Do people tend to communicate with those they've recently interacted with?
# RRecSnd: Person tends to call those who recently called them
# RSndSnd: Person tends to call those they recently called

wtcfit5 <- rem.dyad(WTCPoliceCalls, n = 37, 
                   effects = c("CovInt", "PSAB-BA", "PSAB-BY", "PSAB-AY", 
                               "RRecSnd", "RSndSnd"), 
                   covar = list(CovInt = WTCPoliceIsICR),
                   hessian = TRUE)

summary(wtcfit5)

# Compare with Model 4
cat("BIC difference (Model 4 - Model 5):", wtcfit4$BIC - wtcfit5$BIC, "\n")


#
#-Preferential Attachment-------------------------------------------------------
#

# Does communication follow a "rich get richer" pattern?
# NTDegRec: Normalized Total Degree as Receiver - tests if people 
# with more overall communication are more likely to receive new calls

set.seed(13)  # For reproducibility
wtcfit6 <- rem.dyad(WTCPoliceCalls, n = 37, 
                   effects = c("CovInt", "PSAB-BA", "PSAB-BY", "PSAB-AY", 
                              "RRecSnd", "RSndSnd", "NTDegRec"),
                   covar = list(CovInt = WTCPoliceIsICR), 
                   hessian = TRUE)

summary(wtcfit6)

# Compare with Model 5
cat("BIC difference (Model 5 - Model 6):", wtcfit5$BIC - wtcfit6$BIC, "\n")


#
#-Model Comparison and Interpretation-------------------------------------------
#

# Create a table of BIC values for all models
bic_values <- c(wtcfit1$BIC, wtcfit2$BIC, wtcfit3$BIC, 
                wtcfit4$BIC, wtcfit5$BIC, wtcfit6$BIC)
model_names <- c("ICR Only", "ICR Send/Receive", "ICR + Reciprocity", 
                "ICR + P-shifts", "ICR + P-shifts + Recency", 
                "ICR + P-shifts + Recency + Pref. Attachment")
bic_table <- data.frame(Model = model_names, BIC = bic_values)
bic_table

# Final model interpretation:
# 1. What does PSAB-BA coefficient tell us about conversational norms?
cat("Reciprocity effect (exp(PSAB-BA)):", exp(wtcfit6$coef["PSAB-BA"]), "\n")

# 2. How much does ICR status affect communication rate?
cat("ICR effect (exp(CovInt)):", exp(wtcfit6$coef["CovInt.1"]), "\n")

# 3. How strong is preferential attachment?
cat("Preferential attachment effect (exp(NTDegRec)):", 
    exp(wtcfit6$coef["NTDegRec"]), "\n")

# For more information on available effects, see:
# ?rem.dyad


#
#-Model Adequacy Assessment----------------------------------------------------
#

# Even with the best fitting model, we need to assess if it's adequate for our purposes
# One key question: When is the model "surprised" by observations?

# Examine deviance residuals - how well are events predicted?
nullresid <- 2 * log(37 * 36)  # Deviance residual for the null model (pure chance)
hist(wtcfit6$residuals, main="Deviance Residuals", 
     xlab="Deviance Residual", ylab="Frequency")
abline(v = nullresid, col = 2)  # Red line shows null model level

# Calculate percentage of events better predicted than pure chance
cat("Percent of events better predicted than chance:", 
    mean(wtcfit6$residuals < nullresid) * 100, "%\n")

# Percentage of events very well predicted (below threshold of 3)
cat("Percent of events with low residuals (<3):", 
    mean(wtcfit6$residuals < 3) * 100, "%\n")

# The "random guessing equivalent" shows how many events a random guess 
# would need to predict as well as the model
# Lower values are better - baseline comparison is 1332 possible events (37×36)
rge <- exp(wtcfit6$residuals/2)
quantile(rge)  # Summary of random guessing equivalent distribution

# Compare with our first model - how much improvement have we made?
rge1 <- exp(wtcfit1$residuals/2)
quantile(rge1)

# Identify surprising events - those with residuals higher than null model
surprising_events <- which(wtcfit6$residuals > nullresid)
length(surprising_events)  # Number of surprising events
head(cbind(WTCPoliceCalls[surprising_events,], residual=wtcfit6$residuals[surprising_events]), 10)

# Visualize the surprising events in network form
surprising <- as.sociomatrix.eventlist(WTCPoliceCalls[wtcfit6$residuals > nullresid, ], 37)
gplot(surprising, main="Network of Surprising Events")

# Overlay surprising events on original network
# Edge color intensity shows proportion of surprising events
edge_color <- matrix(rgb(surprising/(WTCPoliceNet + 0.01), 0, 0), 37, 37)
gplot(WTCPoliceNet, edge.col = edge_color, edge.lwd = WTCPoliceNet^0.75,
      vertex.col = 2 + WTCPoliceIsICR,
      main="WTC Network - Red Intensity Shows Surprising Events")


#
#-Prediction Rank Assessment--------------------------------------------------
#

# Another approach: examine the rank of observed events in predicted rate structure
# Rank 1 means the model correctly predicted the exact next event

# Distribution of ranks
hist(wtcfit6$observed.rank, breaks=50, main="Histogram of Observed Event Ranks",
     xlab="Rank (1 is best)", ylab="Frequency")

# Examine how many events were predicted with high precision (low rank)
cat("Percent of events ranked in top 5:", 
    mean(wtcfit6$observed.rank <= 5) * 100, "%\n")

# Plot the cumulative distribution function of ranks
# Shows what fraction of events are covered at different prediction thresholds
plot(ecdf(wtcfit6$observed.rank/(37 * 36)), 
     xlab = "Prediction Threshold (Fraction of Possible Events)",
     ylab = "Fraction of Observed Events Covered", 
     main = "Classification Accuracy")
abline(v = c(0.05, 0.1, 0.25), col = 2)  # Mark 5%, 10%, 25% thresholds

# Check exact matches - when model predicted exactly the right source/recipient
prediction_matches <- wtcfit6$predicted.match

# Overall match rate - when at least one of source or recipient was correct
cat("Fraction of events where at least source OR recipient correct:", 
    mean(apply(prediction_matches, 1, any)), "\n")

# Perfect match rate - when both source AND recipient were correct
cat("Fraction of events where BOTH source AND recipient correct:", 
    mean(apply(prediction_matches, 1, all)), "\n")

# Separate match rates for source and recipient
cat("Fraction correct for source:", colMeans(prediction_matches)[1], "\n")
cat("Fraction correct for recipient:", colMeans(prediction_matches)[2], "\n")

# Conclusion: Despite its simplicity, our model fits extremely well
# Further improvements are possible, but this model provides an adequate 
# representation of communication dynamics in the WTC police network


#
#-Simulating from Fitted REM Models--------------------------------------------
#

# In addition to fitting REMs, the relevent package provides tools for simulating 
# from them. This is useful for:
# 1. Model validation and adequacy checking
# 2. Exploratory simulation studies
# 3. "What-if" scenario analyses
# 4. Understanding the role of specific effects

# Simulation syntax:
# simulate(object, nsim = object$m, seed = NULL, coef = NULL, 
#          covar = NULL, verbose = FALSE, ...)
# Where:
# - object is the fitted model
# - nsim is the number of events to simulate
# - coef can override coefficients (for scenario analysis)
# - covar provides covariates used in the model

# Let's simulate a synthetic replicate of our WTC police data
set.seed(1331)  # For reproducibility
simwtc <- simulate(wtcfit6, covar = list(CovInt = WTCPoliceIsICR),
                  verbose = TRUE)

# Examine the simulated event sequence
head(simwtc, 20)  # First 20 events of simulation

# Convert to sociomatrix for visualization
sim_network <- as.sociomatrix.eventlist(simwtc, 37)
gplot(sim_network, edge.lwd = sim_network^0.75, 
      vertex.col = 2 + WTCPoliceIsICR, vertex.cex = 1.25,
      main="Simulated WTC Network")

# Compare original and simulated networks visually
par(mfrow=c(1,2))
gplot(WTCPoliceNet, edge.lwd = WTCPoliceNet^0.75, 
      vertex.col = 2 + WTCPoliceIsICR, vertex.cex = 1.25,
      main="Original Network")
gplot(sim_network, edge.lwd = sim_network^0.75, 
      vertex.col = 2 + WTCPoliceIsICR, vertex.cex = 1.25,
      main="Simulated Network") 
par(mfrow=c(1,1))


#
#-In Silico Knockout Experiments-----------------------------------------------
#

# We can use simulation to understand how specific model components affect outcomes
# By "knocking out" (setting to zero) specific coefficients and comparing results

# Let's examine how the AB-BA participation shift (turn-taking reciprocity) 
# affects the relationship between ICR status and betweenness centrality

# Set up a small simulation study with multiple replications
set.seed(1331)
reps <- 6  # Number of replicate series to generate

# Create "knockout" coefficient vector with PSAB-BA effect set to zero
kocoef <- wtcfit6$coef  # Copy coefficients
kocoef["PSAB-BA"] <- 0   # Zero out the AB-BA reciprocity effect

# Initialize results matrix - will store correlation between ICR and betweenness
ICRBetCor <- matrix(nrow = reps, ncol = 3)
colnames(ICRBetCor) <- c("ICR Only", "Full Model", "No ABBA")

# Run the simulation study
for (i in 1:reps) {
  cat("Replicate", i, "of", reps, "\n")
  
  # Simulate from ICR-only model
  simwtc1 <- simulate(wtcfit1, covar = list(CovInt = WTCPoliceIsICR))
  sim_net1 <- as.sociomatrix.eventlist(simwtc1, 37)
  ICRBetCor[i, 1] <- cor(betweenness(sim_net1), WTCPoliceIsICR)
  
  # Simulate from full model
  simwtc2 <- simulate(wtcfit6, covar = list(CovInt = WTCPoliceIsICR))
  sim_net2 <- as.sociomatrix.eventlist(simwtc2, 37)
  ICRBetCor[i, 2] <- cor(betweenness(sim_net2), WTCPoliceIsICR)
  
  # Simulate from knockout model (no AB-BA effect)
  simwtc3 <- simulate(wtcfit6, covar = list(CovInt = WTCPoliceIsICR), coef = kocoef)
  sim_net3 <- as.sociomatrix.eventlist(simwtc3, 37)
  ICRBetCor[i, 3] <- cor(betweenness(sim_net3), WTCPoliceIsICR)
}

# Calculate observed correlation in original data
observed_cor <- cor(betweenness(WTCPoliceNet), WTCPoliceIsICR)

# Plot and compare results
boxplot(ICRBetCor, 
        main="Effect of Turn-Taking on ICR-Betweenness Correlation",
        ylab="Correlation")
abline(h = observed_cor, col = "red", lwd=2)
legend("topleft", legend="Observed correlation", col="red", lwd=2)

# Interpret results:
# - The ICR-only model may overstate the relationship between ICR status and betweenness
# - The full model with turn-taking typically produces networks closer to observed data
# - When we "knock out" turn-taking (AB-BA), ICRs gain relatively more betweenness
# 
# This suggests that turn-taking creates opportunities for non-ICR responders to 
# gain airtime and become emergent coordinators. Without turn-taking, institutional
# coordinators dominate the network more.


#
#-Using Simulation for Scenario Analysis--------------------------------------
#

# Another application: What if ICR effects were stronger or weaker?
# We can modify coefficients to test counterfactual scenarios

# Create coefficient sets with varied ICR effects
stronger_icr <- wtcfit6$coef
stronger_icr["CovInt.1"] <- wtcfit6$coef["CovInt.1"] * 1.5  # 50% stronger

weaker_icr <- wtcfit6$coef
weaker_icr["CovInt.1"] <- wtcfit6$coef["CovInt.1"] * 0.5   # 50% weaker

# Simulate from these scenarios
simwtc_stronger <- simulate(wtcfit6, covar = list(CovInt = WTCPoliceIsICR), 
                           coef = stronger_icr)
simwtc_weaker <- simulate(wtcfit6, covar = list(CovInt = WTCPoliceIsICR), 
                         coef = weaker_icr)

# Convert to networks
net_stronger <- as.sociomatrix.eventlist(simwtc_stronger, 37)
net_weaker <- as.sociomatrix.eventlist(simwtc_weaker, 37)

# Compare network properties
par(mfrow=c(1,3))
# Original strength
gplot(sim_network, edge.lwd = sim_network^0.75, 
      vertex.col = 2 + WTCPoliceIsICR, vertex.cex = 1.25,
      main="Normal ICR Effect")
# Stronger ICR
gplot(net_stronger, edge.lwd = net_stronger^0.75, 
      vertex.col = 2 + WTCPoliceIsICR, vertex.cex = 1.25,
      main="Stronger ICR Effect")
# Weaker ICR
gplot(net_weaker, edge.lwd = net_weaker^0.75, 
      vertex.col = 2 + WTCPoliceIsICR, vertex.cex = 1.25,
      main="Weaker ICR Effect")
par(mfrow=c(1,1))

# Compare centralization metrics
cat("Network centralization with normal ICR effect:", centralization(sim_network, degree), "\n")
cat("Network centralization with stronger ICR effect:", centralization(net_stronger, degree), "\n")
cat("Network centralization with weaker ICR effect:", centralization(net_weaker, degree), "\n")

# Simulation provides a powerful tool for understanding how network structures 
# might change under different conditions, and for exploring counterfactual scenarios
# that would be impossible to observe in real-world settings

#
#-The McFarland classroom data---------------------------------------------------
#

# Dyadic Relational Event Models with rem.dyad: Exact Timing

# Examine the structure of the event data
head(Class)
# Each row represents an interaction event with StartTime, FromId, and ToId
# StartTime: Minutes from observation start
# FromId: ID of student/teacher initiating interaction (numbered 1-20)
# ToId: ID of student/teacher receiving interaction (numbered 1-20)

# The last row contains the observation end time with NAs for the actors
tail(Class, 1)

# Load teacher and gender indicator variables
ClassIsTeacher # Indicator for instructor role
ClassIsFemale  # Indicator for gender

# Create a time-aggregated network visualization 
ClassNet <- as.sociomatrix.eventlist(Class, 20)
gplot(ClassNet, vertex.col = 4 - 2 * ClassIsFemale, vertex.sides = 3 + 
      ClassIsTeacher, vertex.cex = 2, edge.lwd = ClassNet^0.75)
# vertex.col = blue for males, red for females
# vertex.sides = triangles for teachers, circles for students
# edge.lwd = line thickness based on number of interactions

#
#-Modeling with covariates-------------------------------------------------------
#

# Create an intercept vector (needed for all models)
ClassIntercept <- rep(1, 20)

# Fit a basic intercept model (equivalent to null model)
classfit1 <- rem.dyad(Class, n = 20, effects = c("CovSnd"), 
                     covar = list(CovSnd = ClassIntercept),
                     ordinal = FALSE, hessian = TRUE)

# Examine model results
summary(classfit1)

# Calculate average events per minute from model
(classfit1$m - 1)/max(Class[, 1])  # Events per minute (observed)
20 * 19 * exp(classfit1$coef)      # Predicted events per minute

# Add effects for role and gender
classfit2 <- rem.dyad(Class, n = 20, effects = c("CovSnd", "CovRec"),
   covar = list(CovSnd = cbind(ClassIntercept, ClassIsTeacher,
      ClassIsFemale), CovRec = cbind(ClassIsTeacher, ClassIsFemale)),
   ordinal = FALSE, hessian = TRUE)

# Compare model fits
summary(classfit2)
classfit1$BIC - classfit2$BIC  # Is the more complex model preferred?

# Gender sending effect wasn't significant; try a reduced model
classfit3 <- rem.dyad(Class, n = 20, effects = c("CovSnd", "CovRec"),
   covar = list(CovSnd = cbind(ClassIntercept, ClassIsTeacher),
      CovRec = cbind(ClassIsTeacher, ClassIsFemale)), 
   ordinal = FALSE, hessian = TRUE)

# Compare this reduced model
summary(classfit3)
classfit2$BIC - classfit3$BIC  # Is the reduced model preferred?

#
#-Endogenous social dynamics-----------------------------------------------------
#

# Add recency effects to model 
classfit4 <- rem.dyad(Class, n = 20, effects = c("CovSnd", "CovRec",
   "RRecSnd", "RSndSnd"), 
   covar = list(CovSnd = cbind(ClassIntercept, ClassIsTeacher), 
   CovRec = cbind(ClassIsTeacher, ClassIsFemale)),
   ordinal = FALSE, hessian = TRUE)

# RRecSnd = recency of receiving from the current sender
# RSndSnd = recency of sending to the current sender

# Compare models
summary(classfit4)
classfit3$BIC - classfit4$BIC  # Is this model preferred?

# Add P-shift effects to capture conversational dynamics
classfit5 <- rem.dyad(Class, n = 20, effects = c("CovSnd", "CovRec",
   "RRecSnd", "RSndSnd", "PSAB-BA", "PSAB-AY", "PSAB-BY"), 
   covar = list(CovSnd = cbind(ClassIntercept, ClassIsTeacher), 
   CovRec = cbind(ClassIsTeacher, ClassIsFemale)),
   ordinal = FALSE, hessian = TRUE)

# P-shift effects:
# PSAB-BA = turn-taking (B responds to A)
# PSAB-AY = persistence (A speaks to new person Y after speaking to B)
# PSAB-BY = "addressee" effect (B speaks to Y after receiving from A)

# Compare models
summary(classfit5)
classfit4$BIC - classfit5$BIC  # Is this model preferred?

# Gender effect no longer significant; create final model without gender
set.seed(13) # Set seed for reproducibility
classfit6 <- rem.dyad(Class, n = 20, effects = c("CovSnd", "CovRec",
   "RRecSnd", "RSndSnd", "PSAB-BA", "PSAB-AY", "PSAB-BY"), 
   covar = list(CovSnd = cbind(ClassIntercept, ClassIsTeacher), 
   CovRec = ClassIsTeacher), 
   ordinal = FALSE, hessian = TRUE)

# Compare final models
summary(classfit6)
classfit5$AICC - classfit6$AICC  # Is the reduced model preferred?

#
#-Investigating event timing from fitted model-----------------------------------
#

# Examine hazard multipliers (coefficient exponentiated)
exp(classfit6$coef["PSAB-BA"])  # Response events have ~100 times normal hazard

# Calculate expected inter-event times under different scenarios
# Mean time if nothing else going on (baseline)
1/(20 * 19 * exp(classfit6$coef["CovSnd.1"]))

# Mean teacher-student time (if nothing else happened)
1/(2 * 18 * exp(sum(classfit6$coef[c("CovSnd.1", "CovSnd.2")])))

# Sequential address by teacher without prior interaction
1/(17 * exp(sum(classfit6$coef[c("CovSnd.1", "CovSnd.2", "PSAB-AY")])))

# Teacher responding to a specific student (immediate follow-up)
1/(exp(sum(classfit6$coef[c("CovSnd.1", "CovSnd.2", "PSAB-BA", "RRecSnd")])))

# Student responding to a specific teacher (immediate follow-up)
1/(exp(sum(classfit6$coef[c("CovSnd.1", "CovRec.1", "PSAB-BA", "RRecSnd")])))

#
#-Assessing model adequacy-------------------------------------------------------
#

# Look at deviance residuals distribution
hist(classfit6$residuals)

# Classification accuracy
mean(apply(classfit6$predicted.match, 1, all))  # Exactly right sender & receiver
mean(apply(classfit6$predicted.match, 1, any))  # At least one correct
colMeans(classfit6$predicted.match)             # Sender vs. receiver accuracy

# Examine surprising events (where model prediction was poor)
surprising <- as.sociomatrix.eventlist(Class[classfit6$observed.rank > 19, ], 20)
gplot(surprising, vertex.col = 4 - 2 * ClassIsFemale, 
      vertex.sides = 3 + ClassIsTeacher, vertex.cex = 2)

# Visualize surprising events within the network context
edgecol <- matrix(rgb(surprising/(ClassNet + 0.01), 0, 0), 20, 20)
gplot(ClassNet, edge.col = edgecol, edge.lwd = ClassNet^0.75,
   vertex.col = 4 - 2 * ClassIsFemale, vertex.sides = 3 + ClassIsTeacher,
   vertex.cex = 2)

#
#-Simulating from the fitted model-----------------------------------------------
#

# Generate a new trajectory from our model
set.seed(1331) # For reproducibility
ClassSim <- simulate(classfit6, 
                    covar = list(CovSnd = cbind(ClassIntercept, ClassIsTeacher), 
                                CovRec = ClassIsTeacher))

# Alternative scenario: what if we had no teachers? (all values set to 0)
AnarchSim <- simulate(classfit6, 
                     covar = list(CovSnd = cbind(ClassIntercept, rep(0, 20)), 
                                 CovRec = rep(0, 20)))

# Convert simulations to networks for visualization
SimNet <- as.sociomatrix.eventlist(ClassSim, 20)
AnarchNet <- as.sociomatrix.eventlist(AnarchSim, 20)

# Compare observed and simulated networks
par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))

# Plot 1: Observed network
gplot(ClassNet, vertex.col = 4 - 2 * ClassIsFemale, vertex.sides = 3 +
   ClassIsTeacher, vertex.cex = 2, edge.lwd = ClassNet^0.75,
   main = "Observed Network", 
   edge.col = rgb(0, 0, 0, (1 - 1/(1 + ClassNet))^3))

# Plot 2: Simulated network (with teachers)
gplot(SimNet, vertex.col = 4 - 2 * ClassIsFemale, vertex.sides = 3 +
   ClassIsTeacher, vertex.cex = 2, edge.lwd = SimNet^0.75, 
   main = "Simulated Network",
   edge.col = rgb(0, 0, 0, (1 - 1/(1 + SimNet))^3))

# Plot 3: Anarchic network (no teachers)
gplot(AnarchNet, vertex.col = 4 - 2 * ClassIsFemale, vertex.sides = 3 +
   ClassIsTeacher, vertex.cex = 2, edge.lwd = AnarchNet^0.75,
   main = "Anarchic Network", 
   edge.col = rgb(0, 0, 0, (1 - 1/(1 + AnarchNet))^3))

# Plot 4: Degree distributions comparison
plot(density(degree(ClassNet), bw = "SJ"), lwd = 3, 
     main = "Degree Distribution")
lines(density(degree(SimNet), bw = "SJ"), lwd = 3, col = 2)
lines(density(degree(AnarchNet), bw = "SJ"), lwd = 3, col = 4)
legend("topright", legend = c("Observed", "Simulated", "Anarchic"), 
       lwd = 3, col = c(1, 2, 4))

# Reset plotting parameters
par(mfrow = c(1, 1))

#
#-Model comparison interpretation------------------------------------------------
#

# Comparing the plots reveals model strengths and limitations:
# 
# The fitted model does well at:
#   - Making teachers central nodes
#   - Creating strong student-teacher interactions
#   - Localizing strong interactions to reciprocal dyads
#   - Reproducing the overall degree distribution
#
# However, it also creates:
#   - A "halo" of weak side-interactions among students not seen in observed data
#   - This suggests potential for further model improvements
#
# The "anarchy" simulation (removing teachers) shows reasonable effects:
#   - Former teacher nodes blend in with peers (no special significance)
#   - Network becomes less centralized without teachers focusing attention
#   - Structure changes in ways we'd expect from removing authority figures
#
# These scenario-based simulations provide valuable tools for:
#   - Testing model behavior under counterfactual conditions
#   - Exploring substantive questions about network structure
#   - Understanding how key factors shape communication patterns
