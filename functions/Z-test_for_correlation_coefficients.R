
# This function analyses the statistical significance of the difference between correlation coefficients across species, data= data source (penguins_clean), var1= variable 1 (culmen length), var2= variable 2 (culmen depth), and species_col= species column name (species).

compare_species_correlations <- function(data, var1, var2, species_col) {
  
  # Creating a new variable (correlation_results)  that contains a species' Z score, and it's respective SE.
  
  correlation_results <- data %>%
    group_by(!!sym(species_col)) %>%
    summarise(
      correlation = cor(!!sym(var1), !!sym(var2), use = "complete.obs"),
      n = n()  
    ) %>%
    # Calculates a correlation coefficient between variable 1 and 2 for each species, calculating sample size for each species
    mutate(
      z = 0.5 * log((1 + correlation) / (1 - correlation)),  
      # New column, transforming correlation coefficients into a Z score
      se = 1 / sqrt(n - 3)  
      # New column containing the standard error for each Z score
    )
  
  
  results_comparison <- combn(correlation_results[[species_col]], 2, simplify = FALSE) %>%
    # Generates a list of all possible pairs of species in correlation_results.
    lapply(function(pair) {
      species_1 <- correlation_results %>% filter(!!sym(species_col) == pair[1])
      species_2 <- correlation_results %>% filter(!!sym(species_col) == pair[2])
      # Extract species 1 and 2 from each pair.
      z_statistic <- abs(species_1$z - species_2$z) / sqrt(species_1$se^2 + species_2$se^2)
      p_value <- 2 * (1 - pnorm(z_statistic))  
      
      # Calculates z-statistics between species pairs, calculating a two-tailed p-value corresponding with this statistic for every pair.
      
      data.frame(
        species_1 = pair[1],
        species_2 = pair[2],
        correlation_diff = round(abs(species_1$correlation - species_2$correlation),3),
        z_statistic = round(z_statistic, 3),
        p_value = round(p_value, 3))
    }) %>%
    
    # Input results into a data frame, naming pair 1 as species 1, pair 2 as species 2, the difference in correlation coefficients as correlation_diff, z-statistic remains the same, along with with p_value. All these values are rounded to 3 decimal places.
    
    bind_rows() %>%
    # Combining multiple rows, collecting results from multiple species pairs inputting them into a single table.
    arrange(p_value)  
  # Sorts rows of the data, ordering by ascending p-value.
  
  
  # Format the ordered data frame, changing column names to be more reader-friendly.
  
  formatted_results <- results_comparison %>%
    rename(
      "Species 1" = species_1,
      "Species 2" = species_2,
      "Difference in Correlation Coefficient" = correlation_diff,
      "Z-Statistic" = z_statistic,
      "P-value" = p_value
    )
  
  return(formatted_results)
}