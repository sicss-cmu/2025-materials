library(tidyverse)


film_genre <- read_csv("data/film_genre.csv") %>%
  group_by(title) %>%
  mutate(age_cat =
           case_when(
             age <= 25 ~ "Age 0 - 25",
             age > 25 & age <= 50 ~ "Age 25 - 50",
             age > 50 & age <= 75 ~ "Age 50 - 75",
             age > 75 ~ "Over 75",
             TRUE ~ NA
           )) %>%
  filter(!is.na(age)) %>%
  filter(gender != "?")

film_genre_dedup <- film_genre %>%
  group_by(title, character) %>%
  slice_head(n = 1) %>%
  select(-genre) %>%
  ungroup()



film_rev <- film_genre_dedup %>%
  group_by(title, gender) %>%
  summarise(prop_dialogue = sum(proportion_of_dialogue))

glimpse(film_genre)

length(unique(film_genre$title))

summary(film_genre$release_year)
summary(film_genre$words)
hist(film_genre$words)

ggplot() +
  geom_boxplot(data = film_genre, aes(y = proportion_of_dialogue, color = gender)) +
  facet_wrap(vars(genre))

ggplot() +
  geom_point(data = film_genre, aes(x = ))

ggplot() +
  geom_point(data = film_genre_dedup %>% filter(age < 110), aes(x = age, y = proportion_of_dialogue, color = gender), alpha = 0.25, size = 0.5) +
  geom_smooth(data = film_genre_dedup %>% filter(age < 110), aes(x = age, y = proportion_of_dialogue, color = gender))

ggplot() +
  # geom_point(data = film_genre %>% filter(age < 110), aes(x = age, y = proportion_of_dialogue, color = gender)) +
  geom_smooth(data = film_genre %>% filter(age < 110), aes(x = gross, y = proportion_of_dialogue, color = gender)) +
  facet_wrap(vars(age_cat))
