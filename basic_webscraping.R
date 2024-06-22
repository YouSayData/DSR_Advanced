library(tidyverse)
library(rvest)


# basic webscraping -------------------------------------------------------

url <- "https://rvest.tidyverse.org/articles/starwars.html"
html <- read_html(url)

html
html |> html_elements("body")
html |> html_elements("h2")
section <- html |> html_elements("section")
section

section |> html_element("h2") |> html_text2()
section |> html_element(".director") |> html_text2()

tibble(
  title = section |> 
    html_element("h2") |> 
    html_text2(),
  released = section |> 
    html_element("p") |> 
    html_text2() |> 
    str_remove("Released: ") |> 
    parse_date(),
  director = section |> 
    html_element(".director") |> 
    html_text2(),
  intro = section |> 
    html_element(".crawl") |> 
    html_text2()
)

# IMDB

url <- "https://web.archive.org/web/20220201012049/https://www.imdb.com/chart/top/"
html <- read_html(url)

table <- html |> 
  html_element("table") |> 
  html_table()
table

ratings <- table |>
  select(
    rank_title_year = `Rank & Title`,
    rating = `IMDb Rating`
  ) |> 
  mutate(
    rank_title_year = str_replace_all(rank_title_year, "\n +", " ")
  ) |> 
  separate_wider_regex(
    rank_title_year,
    patterns = c(
      rank = "\\d+", "\\. ",
      title = ".+", " +\\(",
      year = "\\d+", "\\)"
    )
  )
ratings
html |> 
  html_elements("td strong")

ratings |>
  mutate(
    rating_n = html |> html_elements("td strong") |> html_attr("title")
  ) |> 
  separate_wider_regex(
    rating_n,
    patterns = c(
      "[0-9.]+ based on ",
      number = "[0-9,]+",
      " user ratings"
    )
  ) |> 
  mutate(
    number = parse_number(number)
  )

# Exercise ----------------------------------------------------------------

# Extract the table from this article:
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"


# Using httr2 to interact with APIs ---------------------------------------

library(httr2)

# We start by creating a request that uses the base API url
req <- request("https://fakerapi.it/api/v1")

req |> 
  # Then we add on the users path
  req_url_path_append("users") |> 
  # Add query parameters _width and _quantity
  req_url_query(`_quantity` = 100)


resp <- req |> 
  # Then we add on the users path
  req_url_path_append("users") |> 
  # Add query parameters _width and _quantity
  req_url_query(`_quantity` = 100) |> 
  req_perform()

resp |> resp_body_json() |> str()

resp_data <- resp |> 
  resp_body_json()

resp_data$data |> 
  bind_rows()

# Exercise ----------------------------------------------------------------

# Use the API to retrieve fake products and display them in one ore more tibbles
# ideally it shouldn't be a nested tibble
