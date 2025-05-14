#SICSS R Refresher 
#Example scripts
##Spring 2025

#Link to data: https://drive.google.com/file/d/1BLlccH17AuilksRQa-8iI6zX5cZka3hY/view
#More information about the dataset: https://pudding.cool/2017/03/film-dialogue/ 

#Getting set up----

##Loading libraries----

#install.packages(c("tidyverse", "here"))

library(tidyverse)

##Setting up the working directory----

#Working with the package here

#Creating an R Project and loading the library here will establish the current working directory.
#The here package builds a path to the top level of your project whenever you use it.

library(here)

#Print top level of working directly path
here::here()

##Creating new sub-directories----

#Using here ensures that these new directories go in the right place, even if someone else runs this code
#dir.create(here("data"))
#dir.create(here("scripts"))
dir.create(here("plots"))
dir.create(here("data_output"))

##Importing our data----

#[NOTE: Assumes that the dataset was downloaded and put into the data folder]
#Use the arguments of here to build your path
film_data <- read_csv(here("data", "film_genre.csv"))

#Step 1. Exploring our dataset----

#dim, nrow, ncol, head, tail, names, str, summary

dim(film_data)
nrow(film_data)
ncol(film_data)
head(film_data)
tail(film_data)
names(film_data)
str(film_data)
summary(film_data)

##Explore missing data in key columns----

sum(is.na(film_data$gender))
sum(is.na(film_data$words))
sum(is.na(film_data$age))

#Consider steps to explore the cases with missing age data in more detail
#For example, is there a difference in the proportion of missing values among male and female actors?

film_data %>%
  group_by(gender) %>%
  summarize(
    n = n(),
    missing_age = sum(is.na(age)), #For each group, checks which age values are missing (is.na(age) returns TRUE/FALSE)
    prop_missing_age = mean(is.na(age)) #Calculates the proportion of missing values by taking the mean of the TRUE/FALSE values (TRUE = 1, FALSE = 0)
  )


#Step 2. Plan your steps (this will be done in analog format)----

#Step 3. Create your plot----
#The following is an example of one way you might create a plot to visualize the differences in the amount of dialogue by gender and age

##Data wrangling with dplyr----

#Select the relevant columns
film_data_selected <- select(film_data, c("title", "release_year", "gender", "words", "age"))

#Filter to only post-2000 films
recent_films <- filter(film_data_selected, release_year >= 2000)

#Remove rows with missing values in age column using dplyr's drop_na function

?drop_na #get documentation for drop_na
recent_films <- drop_na(recent_films, age)

#Create age group variable based on age

recent_films <- recent_films %>%
  mutate(age_group = case_when(
    age < 21 ~ "Under 21",
    age >= 21 & age <= 31 ~ "22-31",
    age >= 32 & age <= 41 ~ "32-41",
    age >= 42 & age <= 65 ~ "42-65",
    age > 65 ~ "65 and older"
  ))

#Check variable type
str(recent_films$age_group)

#Change age_group to factor type
recent_films$age_group <- as.factor(recent_films$age_group)
str(recent_films$age_group)

#Reorder factor levels
levels(recent_films$age_group)

recent_films$age_group <- 
  factor(recent_films$age_group, levels = c("Under 21", "22-31", "32-41", "42-65", "65 and older")) 

levels(recent_films$age_group)


##Group and count total words----
#Let's use count to get a count of each genre and then arrange it in order.

words_summary <- recent_films %>%
  group_by(gender, age_group) %>%
  summarise(total_words = sum(words))

words_summary

#Visualizing the data: Option 1 Two bar charts----

ggplot(words_summary, aes(x = age_group, y = total_words, fill = gender)) +
  geom_col() +
  facet_wrap(~gender, nrow = 1, scales = "free_y") +
  labs(x = "Age Group", y = "Total Words") +
  coord_flip() +
  scale_fill_manual(values=c("#5ab4ac", "#d8b365")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here("plots", "barplot_1.png"))

#Could improve things like labels, size etc.

#Visualizing the data: Option 2 Side-by-side bar chart----

#Need to show proportion of total word count by age group to get a sense of the difference in a combined chart
#Use mutate to add a proportion column by gender group

words_summary <- words_summary %>%
  group_by(gender) %>%
  mutate(prop_words = total_words / sum(total_words)) %>%
  ungroup()

ggplot(words_summary, aes(x = age_group, y = prop_words, fill = gender)) +
  geom_col(position = "dodge") +
  labs(x = "Age Group", y = "Proportion of Total Words", fill = "Gender") +
  scale_fill_manual(values=c("#5ab4ac", "#d8b365")) +
  theme_minimal()

ggsave(here("plots", "barplot_2.png"))

#Exporting your data----
write_csv(recent_films, file = here("data_output", "recent_films.csv")) 

#If time, think through a more complex analysis (without coding)
#For example, how would you plot the total proportion of dialogue spoken by men and women over time? 


