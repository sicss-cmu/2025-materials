
### SICSS-CMU Day 1
### R Review
### Group 2

## ====== Instructions =====
## Research has shown that older female actors get fewer roles and less recognition than their male counterparts.
## Use the dataset provided to investigate whether a similar bias exists in the amount of dialogue of male and female 
## actors by age.
## =========================


# build packages vector
packages <- c("tidyverse", "ggplot2", "here", "gridExtra")

# use lapply() to load packages, set character
lapply(packages, library, character.only=T)

# set working directory to where the file is
getwd()
setwd("/Users/ian/Google Drive/CUNY GC/Classes/2025 Summer/SICSS-CMU")
getwd()

# load the data
films <- read.csv("data/film_genre.csv")

# inspect the data
head(films)
glimpse(films)

# check for NA values
films_aged <- !is.na(films$age)


# remove all NA values
# easiest way to get "usable" data set
films_cleaned <- na.omit(films)

# check for na values
is.na(films_cleaned)

# quick check for unique numbers...
# some do not look like ages
# visual inspection of dataframe indicates release year
# and age are simetimes the same number
unique(films_cleaned$age)


# restrict age range to believable number
films_ages <- films_cleaned %>% filter(between(age,0, 100))

# check unique values for age
unique(films_ages$age)

# make a simple plot of age by proportion of dialogue, color by gender
plot1 <- ggplot(films_ages, aes(y=age, x=proportion_of_dialogue, color = gender))+
  geom_line()+
  theme_classic()+
  xlab("Proportion of Dialogue")+
  ylab("Age")+

plot1

# plot for gender
# present this one
plot2 <- ggplot(films_ages, aes(y=age, x=proportion_of_dialogue, color = gender))+
  geom_line()+
  theme_classic()+
  ylab("Age")+
  xlab("Proportion of Dialogue")

plot2

# plot for for age by word count, color by gender
# cool plot #1
plot3 <- ggplot(films_ages, aes(y=age, x=words, color = gender))+
  geom_line()+
  theme_classic()+
  ylab("Age")+
  xlab("Wordcount")+
  ggtitle("Age By Wordcount, Colored By Gender")

plot3

# same variables, but divided into sub-plots for each gender category
# split up for visual inspection
plot3_facet <- ggplot(films_ages, aes(y=age, x=words, color = gender))+
  geom_line()+
  theme_classic()+
  ylab("Age")+
  xlab("Wordcount")+
  ggtitle("Age By Wordcou t, Split By Gender")+
  facet_grid(. ~ gender)

plot3_facet


# made a grid of these two plots
# looked better stacked as rows
grid.arrange(plot3, plot3_facet, 
             ncol = 1, nrow = 2)


############################
# plot for gender as color
# our example from presentation
# cool plot #2
############################
############################

# wordcount by age, color gender
plot4 <- ggplot(films_ages, aes(y=words, x=age, color = gender))+
  geom_line()+
  theme_minimal()+
  ylab("Wordcount")+
  xlab("Age")+
  ggtitle("Wordcount By Age, Colored By Gender")

plot4

# facet split genders
plot4_facet <- ggplot(films_ages, aes(y=words, x=age, color = gender))+
  geom_line()+
  theme_minimal()+
  ylab("Wordcount")+
  xlab("Age")+
  ggtitle("Wordcount By Age, Split By Gender")+
  facet_grid(.~gender)


plot4_facet


# another stacked grid of these two plots
grid.arrange(plot3, plot3_facet, 
             ncol = 1, nrow = 2)

# 2 x 2 grid of first four plots  
grid.arrange(plot1, plot2, plot3, plot4, 
             ncol = 2, nrow = 2)


### genre plots
### we got curious

# plot for genre, more playing around
plot5 <- ggplot(films_ages, aes(y=age, x=proportion_of_dialogue, color = genre))+
  geom_line()+
  theme_classic()+
  ylab("Age")+
  xlab("Proportion of Dialogue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5

# plot for genre, facet wrapped (rather messy)
plot5_facet <- ggplot(films_ages, aes(y=age, x=proportion_of_dialogue, color = genre))+
  geom_line()+
  theme_classic()+
  ylab("Age")+
  xlab("Proportion of Dialogue")+
  facet_grid(.~genre)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5_facet

# invert x and y axis
plot5_facet_invert <- ggplot(films_ages, aes(y=proportion_of_dialogue, x=age, color = genre))+
  geom_line()+
  theme_classic()+
  ylab("Age")+
  xlab("Proportion of Dialogue")+
  facet_grid(.~genre)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot5_facet_invert

# playing around with lines... not accurate
ggplot(films_ages, aes(y=gender, x=proportion_of_dialogue))+
  geom_line()


