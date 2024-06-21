# Exercise I --------------------------------------------------------------

# 1  
measurements <- read_csv("https://education.rstudio.com/blog/2020/08/more-example-exams/measurements.csv")

# 2
cleaned <- measurements |> 
  drop_na()

# 3
cleaned |> 
  group_by(quantity) |>
  count()

# 4
cleaned |> 
  summarise(min = min(reading),
            max = max(reading),
            .by = quantity)

#5

corrected1 <- cleaned |>
  mutate(reading = case_when(quantity == "sal" & reading > 1 ~ reading / 100,
                             !(quantity == "sal" & reading > 1) ~ reading))

corrected2 <- cleaned |>
  mutate(reading = ifelse(quantity == "sal" & reading > 1, reading / 100, reading))


# Exercise 2 --------------------------------------------------------------

person <- read_csv("https://education.rstudio.com/blog/2020/08/more-example-exams/person.csv")

summarize_table <- function(title = character(), tbl = tibble()) {
  if (length(title) != 1 | typeof(title) != "character") {
    stop("invalid title. Did you switch the parameters around?")
  }
  if (!("tbl" %in% class(tbl))) {
    stop("you forgot the table!")
  }
  cat(str_c(title, " has ", dim(tbl)[1], " rows and ", dim(tbl)[2], " columns."))
}

summarize_table("our table", person)

show_columns <- function(title = character(), tbl = tibble()) {
  if (length(title) != 1 | typeof(title) != "character") {
    stop("invalid title. Did you switch the parameters around?")
  }
  if (!("tbl" %in% class(tbl))) {
    stop("you forgot the table!")
  }
  cat(str_c(title, " has the columns ", str_c(colnames(tbl), collapse = ", ")))
}

show_columns("our table", person)


# Exercise 3 --------------------------------------------------------------

import_data <- function(link = character()) {
  read_csv(link,
         na = c("-", ">95%")) |>
  pivot_longer(-ISO3, names_to = "type", values_to = "value", values_drop_na = T) |>
  separate(type, into = c("year", "type"), sep = " ", convert = T) |>
  pivot_wider(names_from = type, values_from = value) |> 
  mutate(across(-c(ISO3, year), \(x) (str_extract(x, "\\d+") |>  as.numeric()) / 100))
}

import_data("https://education.rstudio.com/blog/2020/02/instructor-certification-exams/infant_hiv.csv")
