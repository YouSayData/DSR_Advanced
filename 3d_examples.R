library(palmerpenguins)
library(echarts4r)
library(tidyverse)
library(plotly)

# pca for penguins
penguins <- penguins |> drop_na()

pca <- prcomp(penguins |> select(bill_length_mm:body_mass_g), scale = TRUE)
pca_df <- as.data.frame(pca$x) |> 
  setNames(c("PC1", "PC2", "PC3", "PC4")) |> 
  select(PC1, PC2, PC3) |> 
  mutate(
    species = penguins$species,
    island = penguins$island,
    sex = penguins$sex
    )

pca_df |> 
  group_by(sex) |> 
  e_charts(PC1) |>
  e_scatter_3d(PC2, PC3, color = sex) |> 
  e_toolbox() |>
  e_toolbox_feature(
    feature = "dataView"
  ) |>
  e_toolbox_feature(
    feature = "restore"
  ) |>
  e_toolbox_feature(
    feature = "saveAsImage"
  ) |>
  e_title("PCA for Palmer Penguins")

# Similar plot in plotly

pca_df |> 
  plot_ly(x = ~PC1, y = ~PC2, z = ~PC3, color = ~sex, type = "scatter3d") |> 
  layout(title = "PCA for Palmer Penguins")
  