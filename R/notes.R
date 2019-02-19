# https://pokemondb.net/pokedex/all
# https://www.kaggle.com/alopez247/pokemon

library(tidyverse)
library(janitor)

pokemon <- read_csv("data/pokemon.csv")

# pokemon <- pokemon %>%
#     clean_names()
#
# write_csv(pokemon, "data/pokemon.csv")

pokemon %>%
    ggplot(aes(type_1)) +
    geom_bar() +
    coord_flip()

pokemon %>%
    mutate(type_1 = fct_infreq(type_1)) %>%
    ggplot(aes(type_1)) +
    geom_bar() +
    coord_flip()

pokemon %>%
    mutate(type_1 = fct_infreq(type_1),
           type_1 = fct_rev(type_1)) %>%
    ggplot(aes(type_1)) +
    geom_bar() +
    coord_flip()

pokemon %>%
    mutate(type_1 = fct_lump(type_1, 5),
           type_1 = fct_infreq(type_1),
           type_1 = fct_rev(type_1),
           type_1 = fct_relevel(type_1, "Other")) %>%
    ggplot(aes(type_1)) +
    geom_bar() +
    coord_flip()

pokemon %>%
    mutate(type_1 = fct_lump(type_1, 5),
           type_1 = fct_infreq(type_1),
           type_1 = fct_rev(type_1)) %>%
    ggplot(aes(type_1)) +
    geom_bar() +
    coord_flip()

pokemon %>%
    ggplot(aes(generation, total)) +
    geom_boxplot()

pokemon %>%
    mutate(generation = as.factor(generation)) %>%
    mutate(generation = fct_reorder(generation, total)) %>%
    ggplot(aes(generation, total)) +
    geom_boxplot()


library(ggpubr)

pokemon %>%
    mutate(generation = as.factor(generation)) %>%
    count(generation, type_1) %>%
    ggballoonplot(x = "generation",
              y = "type_1",
              size = "n",
              fill = "n") +
    scale_fill_viridis_c(option = "C")

pokemon %>%
    mutate(generation = as.factor(generation)) %>%
    count(generation, type_2) %>%
    ggballoonplot(x = "generation",
                  y = "type_2",
                  size = "n",
                  fill = "n") +
    scale_fill_viridis_c(option = "C")

pokemon %>%
    mutate(generation = as.factor(generation)) %>%
    gather(type_group, type, type_1, type_2) %>%
    count(generation, type) %>%
    filter(!is.na(type)) %>%
    ggballoonplot(x = "generation",
                  y = "type",
                  size = "n",
                  fill = "n") +
    scale_fill_viridis_c(option = "C")

# let's do some modeling

poke2 <- pokemon %>%
    gather(type_group, type, type_1, type_2) %>%
    filter(!is.na(type)) %>%
    mutate(type_group = case_when(
        type %in% c("Water", "Ice") ~ "Water",
        type %in% c("Fire", "Dragon") ~ "Fire",
        type %in% c("Flying", "Fairy", "Ghost") ~ "Wind",
        type %in% c("Grass", "Ground", "Rock") ~ "Earth",
        TRUE ~ "Other"
    )) %>%
    filter(type_group != "Other")

ggplot(poke2, aes(type_group)) +
    geom_bar()

ggplot(poke2, aes(attack, sp_atk,
                  color = type_group)) +
    geom_point() +
    facet_wrap(~ type_group, nrow = 2)

pokemon
m <- glm(legendary ~ attack + defense + speed,
    data = poke2,
    family = "binomial")
summary(m)
