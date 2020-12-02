library(glue)
library(tidyverse)
library(rvest)

raw_df <- read_csv("./data/chopped-raw.csv") %>%
  mutate(episode_name = str_remove_all(episode_name, '"'))

raw_df

# Scrape IMDB -------------------------------------------------------------


test_url <- "https://www.imdb.com/title/tt1353281/episodes?season=1"


get_imdb_data <- function(season){

  # be nice
  Sys.sleep(1)
  cat(
    glue::glue("Scraping S{season}!"),
    "\n"
  )

  raw_url <- glue::glue("https://www.imdb.com/title/tt1353281/episodes?season={season}&ref_=ttep_ep_sn_nx")

  raw_html <- raw_url %>%
    read_html()

  raw_eps <- raw_html %>%
    html_nodes("#episodes_content > div.clear > div.list.detail.eplist") %>%
    html_nodes("div.list_item")

  ep_ct <- if (season != 1) {
    1:(length(raw_eps))
  } else {
    2:(length(raw_eps))
  }

  get_airdate <- function(scrape_number){
    raw_eps[[scrape_number]] %>%
      html_node("div.airdate") %>%
      html_text() %>%
      str_squish() %>%
      str_remove_all("\n")
  }

  get_title <- function(scrape_number){
    raw_eps[[scrape_number]] %>%
      html_node("div.info > strong > a") %>%
      html_attr("title")
  }

  get_rating <- function(scrape_number){
    raw_eps[[scrape_number]] %>%
      html_node("span.ipl-rating-star__rating") %>%
      html_text() %>%
      as.double()
  }

  get_description <- function(scrape_number){
    raw_eps[[scrape_number]] %>%
      html_node("div.item_description") %>%
      html_text() %>%
      str_remove_all("\n") %>%
      str_squish()
  }

  get_episode <- function(scrape_number){
    raw_eps[[scrape_number]] %>%
      html_node("div.image") %>%
      html_text() %>%
      str_remove_all("\n") %>%
      str_remove("Add Image ") %>%
      str_squish()
  }

  tibble(
    scrape_number = ep_ct
  ) %>%
    mutate(
      air_date = map_chr(scrape_number, get_airdate),
      episode_title = map_chr(scrape_number, get_title),
      episode_rating = map_dbl(scrape_number, get_rating),
      episode_description = map_chr(scrape_number, get_description),
      ep_num = map_chr(scrape_number, get_episode)
    ) %>%
    separate(ep_num, into = c("season_num", "episode_num"), sep = ", Ep") %>%
    mutate(season = str_remove(season_num, "S") %>% as.integer(),
           episode = as.integer(episode_num),
           air_date = lubridate::dmy(air_date)) %>%
    select(season, episode, air_date:episode_description)

}

# scrape all the IMDB data
all_ep_ratings <- map_dfr(1:45, get_imdb_data)

joined_df <- raw_df %>%
  left_join(all_ep_ratings %>%
              select(season, season_episode = episode, episode_rating),
            by = c("season", "season_episode")) #%>%
  #mutate(episode_rating = if_else(episode_rating == 0, NA_real_, episode_rating)) %>%
  #select(season:series_episode, episode_rating, everything())
joined_df

joined_df %>%
  write.csv("chopped.csv")

glimpse(joined_df)

joined_df %>%
  ggplot(aes(x = series_episode, y = season_episode)) +
  geom_point()

joined_df %>%
  ggplot(aes(x = series_episode, y = episode_rating.x)) +
  geom_point() +
  geom_smooth()
