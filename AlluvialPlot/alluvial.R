
## Libraries ----
library(tidylog)
library(tidyverse)
library(lubridate)
library(ggforce) # for alluvial plots
library(extrafont)
library(showtext) #for importing cistom text
library(here)

## Read in Font ----
font_add(family = "StarOut", regular = here(path = "Starjhol.ttf"))
font_add(family = "Star", regular = here(path = "Starjedi.ttf"))
font_add_google("Offside",
                "Offside")
showtext_auto()

trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

## Colours ----
StarWarsCol = c(
  "#DBA579", ## tatooine
  "#133054", ## jango
  "#FACD63", ## mustafar
  "#ECE8D6", ## leia
  "#406E4F", ## dagobah
  "#80B972", ## jabba
  "#CC463C"  ## kylo saber
)

## Data ----

SW = 
  dplyr::starwars %>%
  unnest(films) %>% # unnest films
  group_by(films, gender, homeworld, species) %>%
  summarise(value = n()) %>% #get count for each group size
  ungroup() %>%
  #for ggforce plotting
  gather_set_data(-c(5)) %>%
  mutate(films = factor(films, levels = c("The Phantom Menace", "Attack of the Clones","Revenge of the Sith",
                                          "A New Hope", "The Empire Strikes Back", "Return of the Jedi", 
                                          "The Force Awakens"))) %>% #factor by episode number
  arrange(films)  %>%
  mutate(y = fct_inorder(if_else(is.na(y),
                                 "unspecified",
                                 y)),
         x = factor(x, levels = c("films", "gender", "species", "homeworld"))) #order of x-axis vars


## Plot ----

ggplot(data = SW,
       aes(x, 
           id = id,
           split = y,
           value = value)) +
  geom_parallel_sets(aes(fill = films),
                     alpha = 0.6,
                     axis.width = 0.17,#'gap'  of axis
                     sep = 0.04,
                     size = 0) +
  geom_parallel_sets_axes(axis.width = 0.17, #'gap'  of axis
                          sep = 0.04,
                          fill = "#050304"
  )  +
  geom_parallel_sets_labels(data = SW %>% #plot each x label alone so that you can tweak positioning/sizing
                              filter(x == "species"),
                            colour = 'white',
                            angle = 0,
                            family = "Offside",
                            sep = 0.04,
                            size = 3) +
  geom_parallel_sets_labels(data = SW %>%
                              filter(x == "gender"),
                            colour = 'white',
                            angle = 0,
                            family = "Offside",
                            sep = 0.04,
                            size = 3,
  ) +
  geom_parallel_sets_labels(data = SW %>%
                              filter(x == "films"),
                            colour = 'white',
                            angle = 0,
                            family = "Offside",
                            sep = 0.04,
                            size = 3,
                            position = position_nudge(x = 0.08, y = 0),
                            hjust = 1) +
  geom_parallel_sets_labels(data = SW %>%
                              filter(x == "homeworld"),
                            colour = 'white',
                            angle = 0,
                            family = "Offside",
                            sep = 0.04,
                            size = 3,
                            position = position_nudge(x = -0.08, y = 0),
                            hjust = 0) +
  labs(caption = "Visualisation by @TanyaS_08") +
  scale_fill_manual(values = StarWarsCol) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16,
                                   colour = "#F2D335",
                                   family = "Star",
                                   margin = margin(t = -1, r = 0, b = 0, l = 0, unit = "pt")),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(fill = "#050304",
                                         color = "#050304"),
        plot.background = element_rect(fill = "#050304",
                                       color = "#050304"),
        panel.background = element_rect(fill = "#050304",
                                        color = "#050304"),
        legend.position = "none",
        plot.caption = element_text(size = 9,
                                    colour = "#FBFBFB",
                                    family = "Offside"),
        plot.margin = unit(c(0,-4,0,-4), "cm"))  +
  annotate(
    geom = "text",
    x = 2,
    y = 400,
    vjust = 0.5,
    hjust = 0.5,
    label = 'Star \nWars',
    family = "StarOut",
    size = 22,
    colour = "#F2D335")  +
  annotate(
    geom = "text",
    x = 2,
    y = 400,
    vjust = 0.5,
    hjust = 0.5,
    label = 'the Who, When, Where and What',
    family = "Star",
    size = 7,
    colour = "#FBFBFB")

ggsave(here::here("alluvial.png"),
       height = 8.5, width = 15,
       units = "in", dpi = 600)

