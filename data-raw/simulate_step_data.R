library(data.table)
library(tidyverse)

set.seed(123)  # For reproducibility

# Define test parameters
n_subjects <- 3  # Number of test subjects
n_days <- 2      # Number of days per subject
n_minutes <- 1440 # Total minutes in a day (24 hours)

# Create an empty list to store data
test_data_list <- list()

# Generate test data
for (subject in 1:n_subjects) {
  for (day in 1:n_days) {

    # Create a baseline step pattern
    steps <- rep(0, n_minutes)  # Initialize with zero steps

    # Define peak cadence periods
    steps[100:129] <- 50   # Moderate walking
    steps[200:229] <- 100  # High cadence (30-min region)
    steps[300:329] <- 120  # Higher cadence (30-min region)
    steps[400:429] <- 200  # Highest cadence (30-min region)
    steps[500:504] <- c(10, 20, 30, 40, 50)
    steps[600:604] <- c(200, 190, 180, 170, 160)

    # Create a data table
    dt <- data.table(
      SEQN = paste0("ID_", subject),
      PAXDAYM = day,
      time = 1:n_minutes,
      steps = steps
    )

    test_data_list[[length(test_data_list) + 1]] <- dt
  }
}

# Combine all test data
test_data <- rbindlist(test_data_list)

simulated_nhanes_steps = test_data
usethis::use_data(simulated_nhanes_steps, overwrite = TRUE)

