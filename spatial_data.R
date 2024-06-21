dwd <- read_csv2("https://userpage.fu-berlin.de/soga/300/30100_data_sets/DWD.csv") |> 
  dplyr::select(id, `STATION NAME`, LAT, LON, `RECORD LENGTH`) |> 
  drop_na() |> 
  mutate(RECORD.LENGTH.CATEGORY = cut(`RECORD LENGTH`,
                                      breaks = c(-Inf, 10, 30, 60, 90, Inf), 
                                      labels = c("very short (<10)", 
                                                 "short (10-30)",
                                                 "middle (30-60)",
                                                 "long (60-90)", 
                                                 "very long (>90)")))

ggplot() +
  geom_point(data = dwd,
             aes(x = LON, y = LAT, col = RECORD.LENGTH.CATEGORY),
             alpha = .5,
             size = 1.5) +
  coord_map()


geodata::country_codes() |> as_tibble() |> filter(NAME == "Germany")
geodata::country_codes() |> as_tibble() |> filter(NAME == "Cameroon")
geodata::country_codes() |> as_tibble() |> filter(NAME == "Namibia")

germany <- geodata::gadm(country = "DEU", 
                         path = tempdir(),
                         resolution = 2, 
                         level = 1)

germany_sf <- sf::st_as_sf(germany)

germany_sf_tbl <- germany_sf |> as_tibble()

my_data <- tibble(
  state = sample(germany_sf_tbl$NAME_1, 16, replace = F),
  drink = sample(c("tea", "coffee", "beer", "wine", "water"), 16, replace = T)
  )

my_data_sf <-  germany_sf_tbl |> 
  left_join(my_data, by = c("NAME_1" = "state")) |> 
  dplyr::select(NAME_1, drink)

ggplot(germany_sf) +
  geom_sf(aes(fill = my_data_sf$drink), colour = "black") +
  labs(fill = "Favourite Drink") +
  theme_bw()

ggplot(germany_sf) +
  geom_sf(fill = NA, colour = "black") +
  geom_point(data = dwd,
             aes(x = LON, y = LAT, col = RECORD.LENGTH.CATEGORY),
             alpha = .5,
             size = 1.5) +
  theme_bw() +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = 'Record length') + 
  ggtitle('DWD Weather Stations')


world <- world(path = tempdir())

world_sf <- st_as_sf(world)
world_sf <- st_transform(world_sf, crs = 4326)
st_crs(world_sf)

ggplot(world_sf) +
  geom_sf(fill = NA, colour = "black")

st_can_transform(4326, 3035)

world_sf <- st_transform(world_sf, crs = 3035)
st_crs(world)

ggplot(world_sf) +
  geom_sf(fill = NA, colour = "black")





