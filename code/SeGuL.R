library(tidyverse)

# Function to generate a case(s)
generate_case <- function(cutoff = "random", n = 1) {
  # Generate 'n' sets of 6 random normal deviates for attributes (x1 to x6)
  x_values <- matrix(rnorm(6 * n), ncol = 6)
  
  # Calculate the sum of x_values for each set
  sums <- rowSums(x_values)
  
  # Determine 'y_values' based on 'cutoff'
  y_values <- switch(
    cutoff,
    "EW50" = as.integer(sums > 0),        # Criterion EW50
    "EW27" = as.integer(sums > 1.5),      # Criterion EW27
    "EW5"  = as.integer(sums > 2.5),      # Criterion EW5
    "XOR" =  {
      result <- numeric(n)
      for (i in 1:n) {
        result[i] <- ifelse(xor(sum(x_values[i, 1:3]) > 1.5, sum(x_values[i, 4:6]) > 1.5), 1, 0)
      }
      result
    },
    "random" = sample(0:1, n, replace = TRUE), # Random data for seeding
    stop("Invalid cutoff specified.")
  )
  
  # Create the resulting data frame
  data <- data.frame(x1 = x_values[, 1],
                     x2 = x_values[, 2],
                     x3 = x_values[, 3],
                     x4 = x_values[, 4],
                     x5 = x_values[, 5],
                     x6 = x_values[, 6],
                     y = y_values)
  
  return(data)
}

# Function to select a case based on the rule
select_case <- function(cases, rule) {
  # Calculate the weighted sum for each row using matrix multiplication
  weighted_sum <- as.matrix(cases[, 1:6]) %*% rule
  
  # Find the row index with the maximum weighted sum
  max_row_index <- which.max(weighted_sum)
  
  # Return the row with the maximum weighted sum
  selected_case <- cases[max_row_index, ]
  
  return(selected_case)
}


# Function to update the rule using linear regression
update_rule <- function(L) {
  # Fit a linear regression model and extract the Rule
  lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = L)$coef[2:7]  # Exclude the intercept (b0)
}



# Initialise
iL <- generate_case("random", 8)  # Seed the database with 8 uninformative cases 
iR <- update_rule(iL)  # Generate an initial (uninformed) rule 
n <- 5  # Evaluate 5 cases at a time
number_of_iterations <- 600  

# For generating the results, rather than a core part of SeGuL
# Keep a copy of the data generated on the fly
db <- iL[, 1:6]  # Start with the 8 uninformative seed cases
# Keep a copy of the Rules
ia_weights <- as.data.frame(matrix(iR, ncol = 6))
colnames(ia_weights) <- paste0("a", 1:6)
# create an empty list to store outputs
outputs <- list()



# Main loop for SeGuL algorithm
# "EW50","EW27", "EW5", 
for(i in c("XOR")){
  set.seed(9361) # This ensures that the "database" of cases will be the same for each i
  L <- iL  # set the initial condition for the seed prior to each model EW50, EW27, EW5
  R <- iR  # set the initial rule
  a_weights <- ia_weights  # Create the a_weights record
  
  
  for (j in 1:number_of_iterations) {
    # "Read" next n cases from the database
    cases <- generate_case(i, n)
    
    # Select case
    selected_case <- select_case(cases, R)
    L <- rbind(L, selected_case)
    
    # Update rule
    R <- update_rule(L)
    a_weights <- rbind(a_weights, R)  # Record changing rules
    
    # Save new cases into the "database"
    if(i == "EW50"){
      db <- rbind(db, cases[, 1:6])
    }
  }
  # Add outputs to a list
  outputs[[i]] <- list(L = L, a_weights = a_weights)
}


sum(outputs$XOR$L$y[9:608])/600
