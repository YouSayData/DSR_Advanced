# 1. How does the distribution of flight times within a day change over the course of the year?

flights_dt %>%
  mutate(month = month(dep_time, label = T),
         hour = hour(dep_time)) %>%
  ggplot() +
  geom_density(aes(hour, col = month)) +
  facet_wrap(~month)

# 2. Compare air_time with the duration between the departure and arrival. Explain your findings.

flights_dt %>% 
  drop_na %>%
  mutate(delta = ymd_hms(arr_time) - ymd_hms(dep_time)) %>% 
  ggplot(aes(x = yday(dep_time))) +
  geom_smooth(mapping = aes(y=delta, color="blue")) + 
  geom_smooth(mapping = aes(y=air_time, color="red")) +
  ylab("Time")

flights_dt %>% 
  drop_na %>%
  mutate(delta = ymd_hms(dep_time) + air_time * 60 - ymd_hms(arr_time)) %>% 
  arrange(-delta) %>%
  select(delta, dep_time, arr_time)

flights


# 3. Confirm the hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early.

flights_dt %>% 
  mutate(minute = minute(dep_time),
         early = dep_delay < 0) %>% 
  select(minute, early) %>%
  group_by(minute) %>%
  summarise(prop_early = mean(early)) %>%
  ggplot() +
  geom_line(aes(minute, prop_early))

# 1a. Create a vector of dates giving the first day of every month in 2015. 

make_date(2015,1:12,01)
ymd("2015-01-01") + months(0:11)

# 1b. Create a vector of dates giving the first day of every month in the current year.

make_date(now() %>% year, 1:12, 1)
floor_date(today(), "year") + months(0:11)

# 2. Write a function that given a birthday of a person (as a date), returns how old the person is in years.

age <- function (birthday = date()){
  (today() %--% birthday) / years(1)
}
age(dmy("27-02-1993"))
