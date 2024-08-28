#FUNCTION CREATED! ----

# no_normal() function ----

# You need package e1071 for use skewness and kurtosis functions
if (!require(e1071)) {
  install.packages("e1071")
}
library(e1071)

no_normal <- function(data, variables) {
  # Initialize an empty dataframe for storing results
  results <- data.frame(Variable = character(), Asimetría = numeric(), Curtosis = numeric(), stringsAsFactors = FALSE)
  
  for (var in variables) {
    #  Calculating skewness and kurtosis
    skewness_value <- skewness(data[[var]], na.rm = TRUE)
    kurtosis_value <- kurtosis(data[[var]], na.rm = TRUE) - 3  # Adjustment: Kurtosis is usually measured as excess, we subtract 3 to compare correctly.
    # Check if any of the values is greater than 10
    if (abs(skewness_value) > 10 || abs(kurtosis_value) > 10) {
      # Adding the variable and its skewness and kurtosis values to the results dataframe
      results <- rbind(results, data.frame(Variable = var, Asimetría = round(skewness_value, 2), Curtosis = round(kurtosis_value, 2)))
    }
  }
  
  # Check if the results dataframe is empty
  if (nrow(results) > 0) {
    print(results)
  } else {
    cat("No variables with skewness or kurtosis greater than 10.\n")
  }
}


# example of data frame with no normal values in skewness and/or kurtosis
bd_problem <- data.frame(var1 = rnorm(100), 
                         var2 = c(rnorm(99, mean = 0, sd = 1), 1000), 
                         var3 = c(rep(0, 95), 10, 20, 30, 40, 50))

no_normal(data = bd_problem, variables = c('var1','var2','var3'))

describe(bd_problem) #check that is correct

# example of data frame with normal values in skewness and/or kurtosis
bd_normal <- data.frame(var1 = c(1, 2, 3, 4, 5),
                 var2 = c(10, 15, 20, 25, 1000),
                 var3 = c(1, 1, 1, 1, 2)) 

no_normal(data = bd_normal, variables = c('var1', 'var2', 'var3'))

describe(bd_normal) #check that is correct

