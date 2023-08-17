# Create the dataset
col_1 <- rnorm(100, 1, 0)
x_1 <- rnorm(100, 3, 3)
y <- rnorm(100, 2, 2)

lin_reg_set <- cbind(col_1, x_1, y)

lin_reg_set <- as_tibble(lin_reg_set)

# Define the OLS function
OLS <- function(dataset, x_variable, y_variable) {
  X <- cbind(1, dataset[[x_variable]])  
  coefficients <- solve(t(X) %*% X) %*% (t(X) %*% dataset[[y_variable]])
  print(coefficients)
}

# Call the OLS function
OLS(lin_reg_set, "x_1", "y")  # Use column names as strings