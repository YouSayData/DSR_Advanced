# Exercise II -------------------------------------------------------------


# 1. Add the location of the origin and destination (i.e. the lat and lon) to flights2 (don't save the result though).

flights2 %>% 
  left_join(airports %>% select(faa, lat, lon), by = c("origin" = "faa")) %>%
  left_join(airports %>% select(faa, lat, lon), by = c("dest" = "faa"), suffix = c("_origin", "_dest"))

# 2. Find out: Is there a relationship between the age of a plane and its delays?

flights %>%
  left_join(mutate(planes, age=2021-year), by="tailnum") %>%
  select(age, dep_delay, arr_delay) %>%
  filter(dep_delay > 0, arr_delay > 0) %>%
  pivot_longer(contains("delay"), names_to = "Type of Delay", values_to = "delay") %>%
  ggplot() +
  geom_smooth(aes(age, delay, col = `Type of Delay`)) +
  theme_clean()

planes %>% 
  left_join(flights %>% select(tailnum, contains("delay"))) %>%
  filter(dep_delay > 0, arr_delay > 0) %>%
  ggplot() +
  geom_smooth(aes(year, dep_delay))
