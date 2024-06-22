
# Introduction to EDA -----------------------------------------------------


# Exploratory Data Analysis (EDA) is an iterative cycle:
# 1. Generate questions about your data.
# 2. Search for answers by visualising, transforming, and modelling your data.
# 3. Use what you learn to refine your questions and/or generate new questions.

# It's not a formal process though. 
# It's a creative process.

# “There are no routine statistical questions, only questionable statistical routines.” — Sir David Cox
# Although two questions are very often useful:
# 1. What type of variation occurs within my variables?
# 2. What type of covariation occurs between my variables?
# Variation is the tendency of the values of a variable to change from measurement to measurement. 
# If variation describes the behavior within a variable, covariation describes the behavior between variables. 
# Covariation is the tendency for the values of two or more variables to vary together in a related way.

# Some defs
# A variable is a quantity, quality, or property that you can measure.
# A value is the state of a variable when you measure it. The value of a variable may change from measurement to measurement.
# An observation is a set of measurements made under similar conditions (you usually make all of the measurements in an observation at the same time and on the same object). 
# Tabular data is a set of values, each associated with a variable and an observation.

# Libraries Used ----------------------------------------------------------

library(tidyverse)
library(palmerpenguins)

# Variation of 1 variable -------------------------------------------------

# Discrete variables: Distribution ------------------------------------

?palmerpenguins::penguins

# Observe via plot
ggplot(data = penguins) +
  geom_bar(mapping = aes(x = species))

# Observe via table
penguins |> 
  count(species) |>
  rename(count = n) |>
  mutate(perc = count / sum(count))

# Continuous variables: Distribution --------------------------------------

# via plot
ggplot(data = penguins) +
  geom_histogram(mapping = aes(x = body_mass_g), binwidth = 100)

# via table
penguins |> 
  count(cut_width(body_mass_g, 100))


ggplot(data = penguins, mapping = aes(x = body_mass_g)) +
  geom_histogram(binwidth = 500)

ggplot(data = penguins, mapping = aes(x = body_mass_g)) +
  geom_histogram(binwidth = 50)

# by type
ggplot(data = penguins, 
       mapping = aes(x = body_mass_g, colour = species)) +
  geom_freqpoly(binwidth = 100)

# Follow up questions,  one should ask ------------------------------------
# Which values are the most common? Why?
# Which values are rare? Why? Does that match your expectations?
# Can you see any unusual patterns? What might explain them?

# Let's have a look at another example
?faithful
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("Duration of Eruptions")

# maybe the duration of the eruption is affected by another variable?

# Outliers ----------------------------------------------------------------

penguins |> 
  ggplot(aes(body_mass_g, sex)) + 
  geom_violin() + 
  facet_grid(species~island)

# use boxplot to identify outliers

penguins |> 
  ggplot(aes(body_mass_g, sex)) + 
  geom_boxplot() + 
  facet_grid(species~island)

# use dplyr to calculate which body_mass_g values are outliers

potential_outliers <- penguins |> 
  group_by(species, island) |>
  mutate(
    q1 = quantile(body_mass_g, 0.25, na.rm = T),
    q3 = quantile(body_mass_g, 0.75, na.rm = T),
    iqr = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr
  ) |>
  filter(body_mass_g < lower | body_mass_g > upper)

# Dealing with outliers ---------------------------------------------------
# I decided to drop the unusual values, how can I do this?

# 1. Really drop them.
penguins2 <- penguins |> 
  anti_join(potential_outliers)

# 2. NA them
penguins2 <- penguins |> 
  anti_join(potential_outliers) |> 
  bind_rows(potential_outliers |> 
              mutate(body_mass_g = NA) |> 
              select(-q1, -q3, -iqr, -lower, -upper))

# Reason for NAs ----------------------------------------------------------

# often there is a reason for na, which we can use
nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) |> 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_density(mapping = aes(fill = cancelled), alpha = .3) +
  scale_x_continuous(expand = c(0,0))

cancelledFlights <- nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time) * 1,
    not_cancelled = !is.na(dep_time) * 1,
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60,
    quarterlies = cut(sched_dep_time,
                      breaks = seq(from = 0, to = 24, .5),
                      labels = seq(from = .5, to = 24, .5))) |>
  group_by(quarterlies) |>
  summarise(cancelled = sum(cancelled),
            not_cancelled = sum(not_cancelled)) |>
  mutate(count = cancelled + not_cancelled,
         per_cancelled = cancelled / count) |> 
  filter(per_cancelled != 1)

cancelledFlights |>
  ggplot(aes(x = quarterlies)) +
  geom_col(aes(y = count)) +
  geom_line(aes(y = per_cancelled * max(count), group = 1)) +
  scale_y_continuous(
    sec.axis = sec_axis(~./max(cancelledFlights$count))
  )

# Covariation -------------------------------------------------------------

# Categorical and continuous variable -------------------------------------

ggplot(data = penguins, mapping = aes(x = body_mass_g)) + 
  geom_freqpoly(mapping = aes(colour = species), binwidth = 100)

# If there is too much difference in the distribution geom_density might be better
ggplot(data = penguins, mapping = aes(x = body_mass_g)) + 
  geom_density(mapping = aes(colour = species))

# Alternative boxplot
ggplot(data = penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_boxplot()

# Alternative violin
ggplot(data = penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = species), show.legend = F)

# Alternative random-scatter
ggplot(data = penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_jitter(alpha = .2)

#install.packages("ggbeeswarm")
library(ggbeeswarm)
ggplot(data = penguins, mapping = aes(x = species, y = body_mass_g, col = species)) +
  geom_quasirandom()

ggplot(data = penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_beeswarm(size = 1)

# Two categorical values --------------------------------------------------

ggplot(data = penguins) +
  geom_count(mapping = aes(x = species, y = island))

penguins |> 
  count(species, island)

penguins |> 
  count(species, island) |>  
  ggplot(mapping = aes(x = species, y = island)) +
  geom_tile(mapping = aes(fill = n))


# Exercise ----------------------------------------------------------------
# 1. Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. What makes the plot difficult to read? How could you improve it?

# Two continuous variables ------------------------------------------------

ggplot(data = penguins) +
  geom_point(mapping = aes(x = body_mass_g, y = flipper_length_mm))

# changing alpha can help overplotting
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = body_mass_g, y = flipper_length_mm), alpha = 1 / 5)

# other options
ggplot(data = penguins) +
  geom_bin2d(mapping = aes(x = body_mass_g, y = flipper_length_mm))

ggplot(data = penguins) +
  geom_density2d(mapping = aes(x = body_mass_g, y = flipper_length_mm))

ggplot(data = penguins) +
  geom_hex(mapping = aes(x = body_mass_g, y = flipper_length_mm))

# Exercises --------------------------------------------------------------
# 1. Combine two of the techniques you’ve learned to visualise the combined distribution of cut, carat, and price in the diamonds data set.
# 2. Two dimensional plots reveal outliers that are not visible in one dimensional plots. For example, some points in the plot below have an unusual combination of x and y values, which makes the points outliers even though their x and y values appear normal when examined separately. 
# Why is a scatterplot a better display than a binned plot for this case?

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

