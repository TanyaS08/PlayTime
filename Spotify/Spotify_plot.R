
pacman::p_load(padr, hablar, jsonlite, ggbump, 
               httr, xml2, lubridate, tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))

df <- bind_rows(
  as_tibble(fromJSON("StreamingHistory0.json")),
  as_tibble(fromJSON("StreamingHistory1.json")),
  as_tibble(fromJSON("StreamingHistory2.json")),
  as_tibble(fromJSON("StreamingHistory3.json")),
  as_tibble(fromJSON("StreamingHistory4.json"))) %>% 
  mutate(month = ymd_hm(endTime) %>% as.Date() %>% floor_date("month"))

get_tags_list <- df %>% 
  group_by(artistName, trackName, month) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  filter(n > 10) %>%
  distinct(artistName, trackName)

# API path
path <- "HTTP://ws.audioscrobbler.com/2.0/"

# Get the data through the API
# Generates a dataframe with multiple tags for each songs
tags_df <- map2_dfr(get_tags_list$artistName, 
                    get_tags_list$trackName, 
                    function(x, y) {
                      request <- GET(url = path,
                                     query = list(
                                       method = "track.gettoptags",
                                       track = y,
                                       artist = x,
                                       autocorrect = 1,
                                       api_key = "3eaad12704c1e3be438e627cce58ed27")
                      )
                      
                      # Sleep for 10 seconds to be kind to the API
                      Sys.sleep(10)
                      
                      # Extract the top 20 tags for each song
                      tags <- content(request, "text") %>% 
                        as.character() %>% 
                        read_xml() %>% 
                        xml_find_all(".//toptags/tag/name") %>%
                        xml_text() %>%
                        .[1:20]
                      
                      # Create a data frame with the tags
                      tibble(artist = x,
                             song = y,
                             tags = unique(tags))
                    }
)

tags_df = 
  tags_df %>%
  mutate(tags = case_when(tags %in% c("RB", "r'n'b", "Rnb", "r&b", "r and b", "rnb", 
                                      "rnb number ones") ~ "R&B",
                          tags %in% c("Glam Rock", "80s Hard Rock", "80s Rock", "Rock N Roll", 
                                      "Classic Rock", "Rock  Roll", "90s Rock", "classic rock") ~ "Rock",
                          TRUE ~ tags))

mg <- df %>% 
  semi_join(tags_df, by = c("artistName" = "artist",
                            "trackName" = "song"))

# Summarise the data to the number of streams per month, artist and song.
mg <- mg %>% 
  group_by(month, artist = artistName, song = trackName) %>% 
  summarise(n_streams = n()) %>% 
  ungroup()

# Join the tags on artists and song. 
# Makes the data frame longer since there are multiple tags per song.
mg <- mg %>% 
  left_join(tags_df, by = c("artist", "song")) %>% 
  drop_na()

# Mark the tags nice with title case
# Summarise number of streams per tag and month. Implicitely dropping songs and artist.
mg <- mg %>% 
  mutate(tags = str_to_title(tags)) %>% 
  group_by(month, tags) %>% 
  summarise(streams_value = sum_(n_streams)) %>% 
  ungroup() %>% 
  arrange(-streams_value) %>%
  filter(tags %!in% c("American", "Beyonce", "Favorites"))

# Pad the tags in order to get a streams value of 0 for months when I did not listen to that tag.
rk <- mg %>%
  pad("month", end_val = max(mg$month), group = "tags") %>%
  mutate(streams_value = if_na(streams_value, 0L))

# Create one season variable in text and one in numeric to be used for a discrete x axis..
rk <- rk %>% 
  mutate(season_txt = case_when(
    month(month) %in% c(12)  ~ paste("Summer", year(month) + 1, sep = "\n"),
    month(month) %in% c(1:2) ~ paste("Summer", year(month), sep = "\n"),
    month(month) %in% c(3:5) ~ paste("Autumn", year(month), sep = "\n"),
    month(month) %in% c(6:8) ~ paste("Summer", year(month), sep = "\n"),
    month(month) %in% c(9:11) ~ paste("Spring", year(month), sep = "\n")),
    season_num = case_when(
      month(month) %in% c(12)  ~ paste0(year(month) + 1, 1),
      month(month) %in% c(1:2) ~ paste0(year(month), 1),
      month(month) %in% c(3:5) ~ paste0(year(month), 2),
      month(month) %in% c(6:8) ~ paste0(year(month), 3),
      month(month) %in% c(9:11) ~ paste0(year(month), 4)))

# Collapse data to season.
rk <- rk %>% 
  group_by(tags, season_txt, season_num) %>% 
  summarise(streams_value = sum_(streams_value)) %>% 
  ungroup()

# Create a x axis variable with the season variable from format `20191, 20192` to `1, 2` etc.
rk <- rk %>%
  left_join(rk %>%
              distinct(season_num) %>%
              mutate(order = rank(season_num)), by = "season_num")

# Rank the streams value per. 
# Ranks above 5 will be set to 6 and tags the never reached top 5 are removed.
rk <- rk %>%
  group_by(order) %>%
  mutate(rank = rank(-streams_value, ties.method = "random")) %>%
  ungroup() %>%
  group_by(tags) %>%
  mutate(any_top_5 = any(rank <= 5)) %>% 
  ungroup() %>%
  mutate(rank = if_else(rank > 5,
                        6L,
                        rank)) %>% 
  filter(any_top_5 == TRUE)

# Find first and last season for each tag. Remove observations if outside that span.
rk <- rk %>%
  group_by(tags) %>%
  mutate(first_top5 = min_(order[rank <= 5]),
         last_top5 = max_(order[rank <= 5]),
         d_first_top5 = if_else(order == first_top5,
                                1,
                                0)) %>%
  filter(!is.na(first_top5),
         order >= first_top5,
         order <= last_top5) %>%
  ungroup()

# Create groups for the "active" top 5 tags. 
# This is needed to supress geom_bump to draw lines more than to the next season.
rk <- rk %>% 
  arrange(tags, order) %>% 
  group_by(tags) %>% 
  mutate(lag_zero = if_else(lag(rank) %in% c(6, NA) & rank <= 5, 1, 0, 0)) %>% 
  ungroup() %>% 
  mutate(group = cumsum(lag_zero))

# Select the columns needed for the plot
rk <- rk %>% 
  select(tags, season_txt, order, rank, first_top5, last_top5, d_first_top5, group)

# Make a palette by drawing pseudo random colors from `RColorBrewer`
col_pal =
  tibble::tibble(
    genre = 
      c("80s", "Alternative", "Americana", "Hard Rock", "Heavy Metal", "Hip-Hop", "Metal", "Pop", "R&B", 
        "Rock", "Soul", "Dance", "Funk"),
    colour = palettetown::pokepal(206, spread = 13)
  )

p <- rk %>%
  mutate(season_txt = ifelse(season_txt == "Summer\n2021",
                             "Winter\n2021",
                             season_txt)) %>%
  ggplot(aes(order, rank, color = tags, group = tags)) +
  geom_bump(smooth = 15, size = 2, alpha = 0.2) +
  scale_y_reverse()  +
  geom_bump(data = rk %>% filter(rank <= 5),
            aes(order, rank, group = group, color = tags),
            smooth = 15, size = 2, inherit.aes = F) +
  geom_point(data = rk %>% filter(d_first_top5 == 1),
             aes(x = order - .2),
             size = 5) +
  geom_segment(data = rk %>% filter(rank <=5),
               aes(x = order - .2, xend = order + .2, y = rank, yend = rank),
               size = 2,
               lineend = "round") +
  scale_x_continuous(breaks = rk$order %>% unique() %>% sort(),
                     labels = rk %>% distinct(order, season_txt) %>% arrange(order) %>% pull(season_txt),
                     expand = expansion(mult = .1)) +
  geom_text(data = rk %>% filter(d_first_top5 == 1),
            aes(label = tags, x = order-.2),
            color = "white",
            nudge_y = .43,
            nudge_x = -.05,
            size = 3.5,
            fontface = 2,
            hjust = 0) +
  geom_text(data = rk %>% filter(order == max(order)),
            aes(label = tags),
            color = "gray70",
            nudge_x = .31,
            hjust = 0,
            size = 3,
            fontface = 2) +
  cowplot::theme_minimal_hgrid(font_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = .5, color = "white"),
        plot.caption = element_text(hjust = 1, color = "white", size = 8),
        plot.subtitle = element_text(hjust = .5, color = "white", size = 10),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = 2, color = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")) +
  labs(x = NULL,
       title ="My year on Spotify",
       subtitle ="Top 5 genre tags spanning the seasons",
       caption = "\nSource:\nPersonal Spotify data\nLast.fm API") +
  scale_colour_manual(values = col_pal$colour,
                      breaks = col_pal$genre) +
  geom_point(data = tibble(x = 0.55, y = 1:5), aes(x = x, y = y),
             inherit.aes = F,
             color = "white",
             size = 10,
             pch = 21) +
  geom_text(data = tibble(x = .55, y = 1:5), aes(x = x, y = y, label = y),
            inherit.aes = F,
            color = "white")

p

mg <- df %>% 
  semi_join(tags_df, by = c("artistName" = "artist",
                            "trackName" = "song")) %>%
  filter(artistName != "Prince")

# Summarise the data to the number of streams per month, artist and song.
mg <- mg %>% 
  group_by(month, artist = artistName, song = trackName) %>% 
  summarise(n_streams = n()) %>% 
  ungroup()

# Join the tags on artists and song. 
# Makes the data frame longer since there are multiple tags per song.
mg <- mg %>% 
  left_join(tags_df, by = c("artist", "song")) %>% 
  drop_na()

# Mark the tags nice with title case
# Summarise number of streams per tag and month. Implicitely dropping songs and artist.
mg <- mg %>% 
  mutate(tags = str_to_title(tags)) %>% 
  group_by(month, tags) %>% 
  summarise(streams_value = sum_(n_streams)) %>% 
  ungroup() %>% 
  arrange(-streams_value) %>%
  filter(tags %!in% c("American", "Beyonce", "Favorites"))

# Pad the tags in order to get a streams value of 0 for months when I did not listen to that tag.
rk <- mg %>%
  pad("month", end_val = max(mg$month), group = "tags") %>%
  mutate(streams_value = if_na(streams_value, 0L))

# Create one season variable in text and one in numeric to be used for a discrete x axis..
rk <- rk %>% 
  mutate(season_txt = case_when(
    month(month) %in% c(12)  ~ paste("Summer", year(month) + 1, sep = "\n"),
    month(month) %in% c(1:2) ~ paste("Summer", year(month), sep = "\n"),
    month(month) %in% c(3:5) ~ paste("Autumn", year(month), sep = "\n"),
    month(month) %in% c(6:8) ~ paste("Summer", year(month), sep = "\n"),
    month(month) %in% c(9:11) ~ paste("Spring", year(month), sep = "\n")),
    season_num = case_when(
      month(month) %in% c(12)  ~ paste0(year(month) + 1, 1),
      month(month) %in% c(1:2) ~ paste0(year(month), 1),
      month(month) %in% c(3:5) ~ paste0(year(month), 2),
      month(month) %in% c(6:8) ~ paste0(year(month), 3),
      month(month) %in% c(9:11) ~ paste0(year(month), 4)))

# Collapse data to season.
rk <- rk %>% 
  group_by(tags, season_txt, season_num) %>% 
  summarise(streams_value = sum_(streams_value)) %>% 
  ungroup()

# Create a x axis variable with the season variable from format `20191, 20192` to `1, 2` etc.
rk <- rk %>%
  left_join(rk %>%
              distinct(season_num) %>%
              mutate(order = rank(season_num)), by = "season_num")

# Rank the streams value per. 
# Ranks above 5 will be set to 6 and tags the never reached top 5 are removed.
rk <- rk %>%
  group_by(order) %>%
  mutate(rank = rank(-streams_value, ties.method = "random")) %>%
  ungroup() %>%
  group_by(tags) %>%
  mutate(any_top_5 = any(rank <= 5)) %>% 
  ungroup() %>%
  mutate(rank = if_else(rank > 5,
                        6L,
                        rank)) %>% 
  filter(any_top_5 == TRUE)

# Find first and last season for each tag. Remove observations if outside that span.
rk <- rk %>%
  group_by(tags) %>%
  mutate(first_top5 = min_(order[rank <= 5]),
         last_top5 = max_(order[rank <= 5]),
         d_first_top5 = if_else(order == first_top5,
                                1,
                                0)) %>%
  filter(!is.na(first_top5),
         order >= first_top5,
         order <= last_top5) %>%
  ungroup()

# Create groups for the "active" top 5 tags. 
# This is needed to supress geom_bump to draw lines more than to the next season.
rk <- rk %>% 
  arrange(tags, order) %>% 
  group_by(tags) %>% 
  mutate(lag_zero = if_else(lag(rank) %in% c(6, NA) & rank <= 5, 1, 0, 0)) %>% 
  ungroup() %>% 
  mutate(group = cumsum(lag_zero))

# Select the columns needed for the plot
rk <- rk %>% 
  select(tags, season_txt, order, rank, first_top5, last_top5, d_first_top5, group)

p2 <- rk %>% 
  mutate(season_txt = ifelse(season_txt == "Summer\n2021",
                             "Winter\n2021",
                             season_txt)) %>%
  ggplot(aes(order, rank, color = tags, group = tags)) +
  geom_bump(smooth = 15, size = 2, alpha = 0.2) +
  scale_y_reverse()  +
  geom_bump(data = rk %>% filter(rank <= 5), 
            aes(order, rank, group = group, color = tags), 
            smooth = 15, size = 2, inherit.aes = F) + 
  geom_point(data = rk %>% filter(d_first_top5 == 1),
             aes(x = order - .2),
             size = 5) +
  geom_segment(data = rk %>% filter(rank <=5),
               aes(x = order - .2, xend = order + .2, y = rank, yend = rank),
               size = 2,
               lineend = "round") +
  scale_x_continuous(breaks = rk$order %>% unique() %>% sort(),
                     labels = rk %>% distinct(order, season_txt) %>% arrange(order) %>% pull(season_txt), 
                     expand = expansion(mult = .1)) +
  geom_text(data = rk %>% filter(d_first_top5 == 1),
            aes(label = tags, x = order-.2),
            color = "white",
            nudge_y = .43,
            nudge_x = -.05,
            size = 3.5,
            fontface = 2,
            hjust = 0) +
  geom_text(data = rk %>% filter(order == max(order)),
            aes(label = tags),
            color = "gray70",
            nudge_x = .31,
            hjust = 0,
            size = 3,
            fontface = 2) +
  cowplot::theme_minimal_hgrid(font_size = 14) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = .5, color = "white"),
        plot.caption = element_text(hjust = 1, color = "white", size = 8),
        plot.subtitle = element_text(hjust = .5, color = "white", size = 10),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = 2, color = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")) +
  labs(x = NULL,
       title ="My year on Spotify - without Prince",
       subtitle ="Top 5 genre tags spanning the seasons",
       caption = "\nSource:\nPersonal Spotify data\nLast.fm API") +
  scale_colour_manual(values = col_pal$colour,
                      breaks = col_pal$genre) +
  geom_point(data = tibble(x = 0.55, y = 1:5), aes(x = x, y = y), 
             inherit.aes = F,
             color = "white",
             size = 10,
             pch = 21) +
  geom_text(data = tibble(x = .55, y = 1:5), aes(x = x, y = y, label = y), 
            inherit.aes = F,
            color = "white")

p2

ggsave("Spotify_w_Prince.png",
       p)

ggsave("Spotify_no_Prince.png",
       p2)


top_arts = df %>%
  mutate(month = case_when(
    month(month) %in% c(7:8)  ~ "Stuck in UK",
    month(month) %in% c(9:10) ~ "SVD ms",
    month(month) %in% c(11:12) ~ "Roadmap ms",
    month(month) %in% c(2) ~ "Roadmap ms",
    month(month) %in% c(1) ~ "2nd Wave",
    month(month) %in% c(3) ~ "Traitstrap ms",
    month(month) %in% c(4:5) ~ "Transfer learning",
    month(month) %in% c(6) ~ "3rd Wave")) %>%
  group_by(artistName, month) %>% 
  summarise(listens = n()) %>% 
  ungroup() %>% 
  arrange(-listens) %>%
  group_by(month) %>%
  mutate(rank = rank(-listens, ties.method = "random")) %>%
  ungroup() %>%
  group_by(artistName) %>%
  mutate(any_top_5 = any(rank <= 5)) %>% 
  ungroup() %>%
  mutate(rank = if_else(rank > 5,
                        6L,
                        rank)) %>% 
  filter(any_top_5 == TRUE) %>%
  View()