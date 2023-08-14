results <- read.csv(file = "raw_data.csv", header = TRUE)

# code missing values
results <- replace(results, list = results < 0, values = NA)

# deidentify
results <- subset(results, select = -zipcode)

write.csv(results, file = "clean_data.csv", row.names = FALSE)
