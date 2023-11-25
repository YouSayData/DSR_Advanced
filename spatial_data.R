dwd <- read_csv2("https://userpage.fu-berlin.de/soga/300/30100_data_sets/DWD.csv") |> 
  select(id, `STATION NAME`, LAT, LON, `RECORD LENGTH`) |> 
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
             size = 1.5)


geodata::country_codes() |> as_tibble() |> filter(NAME == "Germany")

germany <- geodata::gadm(country = "DEU", 
                         path = tempdir(),
                         resolution = 2, 
                         level = 1)

?st_transform

germany_sf <- sf::st_as_sf(germany)

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
