# Load the dataset
census_data <- data.frame(
  Year = c(1901, 1911, 1921, 1931, 1941, 1951, 1961, 1971, 1981, 1991, 2001, 2011),
  AP   = c(985, 992, 993, 987, 980, 986, 981, 977, 975, 972, 978, 993),
  KA   = c(983, 981, 969, 965, 960, 966, 959, 957, 963, 960, 965, 973),
  KL   = c(1004, 1008, 1011, 1022, 1027, 1028, 1022, 1016, 1032, 1036, 1058, 1084),
  TN   = c(1044, 1042, 1029, 1027, 1012, 1007, 992, 978, 977, 974, 987, 996),
  India = c(972, 964, 955, 950, 945, 946, 941, 930, 934, 927, 933, 943)
)
# Compute five-number summary
summary_stats <- sapply(census_data[, -1], fivenum)
rownames(summary_stats) <- c("Min", "Q1", "Median", "Q3", "Max")
print(summary_stats)

# Draw separate boxplots
par(mfrow = c(2, 3)) # Set up plotting grid
for (col in colnames(census_data)[-1]) {
  boxplot(census_data[[col]], main = col, ylab = "Sex Ratio", col = "lightblue")
}
cor_matrix <- cor(census_data[, -1])
print(round(cor_matrix, 3))
# Function to extract extremes and calculate IQR
extremes_and_iqr <- function(df) {
  results <- data.frame(State = colnames(df)[-1])
  
  results$Max_Year <- sapply(df[, -1], function(x) df$Year[which.max(x)])
  results$Max_Val  <- sapply(df[, -1], max)
  results$Min_Year <- sapply(df[, -1], function(x) df$Year[which.min(x)])
  results$Min_Val  <- sapply(df[, -1], min)
  results$IQR      <- sapply(df[, -1], IQR, type = 7)
  
  return(results)
}
print(extremes_and_iqr(census_data))
# Reshape data or use basic matplot for visualization
matplot(census_data$Year, census_data[, -1], type = "b", pch = 16, lty = 1,
        col = 1:5, xlab = "Year", ylab = "Sex Ratio", main = "Sex Ratio Trends (1901-2011)")
legend("right", legend = colnames(census_data)[-1], col = 1:5, pch = 16, cex = 0.8)
# Create vectors for t and t+1 across all states/India combined
decade_t   <- as.vector(as.matrix(census_data[1:11, -1]))
decade_t1  <- as.vector(as.matrix(census_data[2:12, -1]))

ttest_result <- t.test(decade_t, decade_t1, paired = TRUE)
print(ttest_result)
# Creating a lagged data layout
lagged_df <- data.frame(
  Y_2011 = as.numeric(census_data[12, -1]),
  X_2001 = as.numeric(census_data[11, -1]),
  X_1991 = as.numeric(census_data[10, -1])
)

model <- lm(Y_2011 ~ X_2001 + X_1991, data = lagged_df)
summary(model)
# Calculate lag-1 correlation across regions
lagged_corrs <- sapply(census_data[, -1], function(x) cor(x[-1], x[-length(x)]))
print(lagged_corrs)
