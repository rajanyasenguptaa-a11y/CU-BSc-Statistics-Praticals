data_vector <- c(1, 2, 3, 4, 4, 4, 5, 5, 6, 9)

mean_value <- mean(data_vector)

median_value <- median(data_vector)

get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mode_value <- get_mode(data_vector)

cat("Data Vector: ", paste(data_vector, collapse = ", "), "\n")
cat("Mean:       ", mean_value, "\n")
cat("Median:     ", median_value, "\n")
cat("Mode:       ", mode_value, "\n")
