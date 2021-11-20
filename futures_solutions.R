# 1

lotto <- function(week) tibble(week = week,
                               correct = sample(1:49, 6) %in% sample(1:49, 6) %>% sum) 

k <- 100000
weeks_played <- 1:k
lotto_results <- map(weeks_played, lotto)
lotto_results <- bind_rows(lotto_results)

ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }

lotto_results %>% 
  count(correct) %>%
  ggplot(aes(correct, n)) +
  geom_col() +
  scale_y_continuous(labels = ks) +
  labs(title = "Playing Lotto",
       subtitle = str_c("Numbers correct in\n", ks(k), " games")) +
  theme_clean() +
  theme(axis.title = element_blank())
  
