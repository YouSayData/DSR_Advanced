library(tidyverse)
library(pheatmap)
library(philentropy)

# mockup data -------------------------------------------------------------

data_matrix <- matrix(runif(400, 0, 1), nrow = 10, byrow = T)
rownames(data_matrix) <- str_c("thing_", LETTERS[1:10])
colnames(data_matrix) <- str_c("var_", 1:ncol(data_matrix))

# base

heatmap(data_matrix)

# pheatmap

pheatmap(data_matrix, cutree_rows = 4)
pheatmap(data_matrix, cutree_rows = 4, cutree_cols = 3)

# ggplot

data_dist_tbl <- distance(data_matrix) %>% as_tibble

colnames(data_dist_tbl) <- rownames(data_matrix)
data_dist_tbl$id <- rownames(data_matrix)

data_dist_tbl <- data_dist_tbl %>%
  pivot_longer(cols = starts_with("thing"), names_to = "id2", values_to = "distance")

data_dist_tbl %>%
  ggplot() +
  geom_tile(aes(id, id2, fill = distance))

