library(tidyverse)
library(echarts4r)
library(palmerpenguins)

penguins

penguins |>
  ggplot(aes(species)) +
  geom_bar()

penguins |>
  count(species) |>
  ggplot(aes(species, n)) +
  geom_col()

penguins |>
  count(species) |>
  e_charts(species) |>
  e_bar(n)
