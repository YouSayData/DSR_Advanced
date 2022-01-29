# Exercise I --------------------------------------------------------------

# Write loops for (Think about the output, sequence, and body before you start writing the loop.):
# 1. Compute the mean of every column in mtcars.

output <- vector("double", ncol(mtcars))  # 1. output
for (i in seq_along(mtcars)) {            # 2. sequence
  output[[i]] <- mean(mtcars[[i]])      # 3. body
}
output

# 2. Determine the type of each column in nycflights13::flights.

output <- vector("character", ncol(nycflights13::flights))  # 1. output
for (i in seq_along(nycflights13::flights)) {            # 2. sequence
  output[[i]] <- typeof(nycflights13::flights[[i]])      # 3. body
}
output

# 3. Compute the number of unique values in each column of iris.

output <- vector("integer", ncol(iris))  # 1. output
for (i in seq_along(iris)) {            # 2. sequence
  output[[i]] <- iris[[i]] %>% unique %>% length      # 3. body
}
output

# 4. Generate 10 random normals from distributions with means of -10, 0, 10, and 100.

data <- c(-10,0,10,100)
output <- vector("list", length(data))  # 1. output
for (i in seq_along(data)) {            # 2. sequence
  output[[i]] <- rnorm(10, mean = data[[i]])      # 3. body
}
output

# Exercise II -------------------------------------------------------------

# 1. Write a function that prints the mean of each numeric column in a data frame, 
# along with its name. Test it on the iris data set.

mean_name <- function(x) {
  for (i in seq_along(x)) {
    if (is.numeric(x[[i]])) {
      print(str_c(names(x[i]), mean(x[[i]]), sep = ": "))
    }
  }
}
