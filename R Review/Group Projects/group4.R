#installing packages

library(tidyverse)
library(here)
library(ggplot2)

# naming and reading in the dataset

imdb_data <- read_csv("film_genre.csv")

# total number of films

length(unique(imdb_data[["title"]])

# filtering by age, only over 50

old_data <- imdb_data%>%
  filter(age > 50)

# filtering by gender, only men

old_man_data <- old_data%>%
  filter(gender=='man')

# getting a count for the number of films with men over 50

length(unique(old_man_data[["title"]]))

# getting a count for older man data

count(old_man_data)

# filtering by gender, only women

old_woman_data <- old_data%>%
  filter(gender=='woman')


# getting a count for the number of films with women over 50

length(unique(old_woman_data[["title"]]))

# getting a count for older woman data

count(old_woman_data)

# one plot with men

old_man_plot <- ggplot(old_man_data, aes(x=proportion_of_dialogue, y=rating)) + 
  geom_point(colour="blue")+
    labs(title="Film Characters' Proportion of Dialogue to IMDB Rating, Men Characters Over 50 (Film N = 1,464, Character N = 7,786)", x = "Proportion of Dialogue per Character", y = "IMDB Rating", )

old_man_plot

# one plot with women

old_woman_plot <- ggplot(old_woman_data, aes(x=proportion_of_dialogue, y=rating)) + 
  geom_point(colour="red")+
    labs(title="Film Characters' Proportion of Dialogue to IMDB Rating, Women Characters Over 50 (Film N - 598, Character N = 1,553)", x = "Proportion of Dialogue per Character", y = "IMDB Rating", )

old_woman_plot
