


# Load necessary library
library(dplyr)

library(purrr)

# Function to generate Poisson samples for each numeric data point in a data frame
generate_poisson_samples <- function(df) {
  # Identify numeric columns
  numeric_cols <- df %>% select(where(is.numeric))
  
  # Apply rpois to each element of the numeric columns
  poisson_samples <- numeric_cols %>% 
    mutate(across(everything(), ~sapply(., function(lambda) rpois(1, lambda))))
  
  # Combine the non-numeric columns with the Poisson samples
  result_df <- bind_cols(df %>% select(where(negate(is.numeric))), poisson_samples)
  
  return(result_df)
}



# Generate Poisson samples for each data point in the data frame
poisson_samples_df <- generate_poisson_samples(filtered_df)

# Print the original data frame and the resulting samples
print("Original Data Frame:")
print(filtered_df)
print("Poisson Samples Data Frame:")
print(poisson_samples_df)



Mder<-poisson_samples_df$CG3 














# Example data frame (simulating your filtered_df), use data frame from the project code though
filtered_df <- tibble(
  ...1 = c("A0A087WZY1", "A0A1B0GU03", "ACTB", "ALB", "AMY1A", "AZGP1", "B2M", "BASP1", "CRISP3", "CST3"),
  CG1 = c(152, 13, 196, 445, 2059, 292, 35, 16, 152, 64),
  CG2 = c(99, 10, 261, 766, 5640, 246, 29, 20, 46, 37),
  CG3 = c(0, 10, 608, 2124, 1059, 311, 15, 36, 55, 74),
  CG4 = c(117, 17, 93, 331, 3745, 248, 21, 0, 174, 70),
  CG5 = c(214, 30, 336, 725, 2536, 447, 38, 17, 203, 134),
  CG6 = c(167, 3, 200, 942, 5143, 139, 10, 32, 28, 22),
  CG7 = c(95, 6, 383, 1687, 914, 252, 13, 51, 120, 46),
  CG8 = c(137, 9, 212, 711, 578, 171, 27, 19, 134, 47),
  CG9 = c(207, 24, 135, 479, 3820, 404, 39, 4, 219, 144),
  CG10 = c(226, 27, 203, 1001, 4876, 438, 31, 12, 101, 57),
  CG11 = c(224, 3, 224, 1141, 3379, 118, 0, 38, 21, 30)
)


