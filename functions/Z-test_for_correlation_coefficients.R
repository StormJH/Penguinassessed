compare_species_correlations <- function(data, var1, var2, species_col) {
  
  # Calculating a species' Fisher's Z score, and it's respective SE.
  correlation_results <- data %>%
    group_by(!!sym(species_col)) %>%
    summarise(
      correlation = cor(!!sym(var1), !!sym(var2), use = "complete.obs"),
      n = n()  # Sample size for each species
    ) %>%
    mutate(
      z = 0.5 * log((1 + correlation) / (1 - correlation)),  
      # Fisher's Z transformation
      se = 1 / sqrt(n - 3)  
      # Standard error for Fisher's Z
    )
  
  # Comparing correlation coeffecients between all possible pairs of different species
  results_comparison <- combn(correlation_results[[species_col]], 2, simplify = FALSE) %>%
    lapply(function(pair) {
      species_1 <- correlation_results %>% filter(!!sym(species_col) == pair[1])
      species_2 <- correlation_results %>% filter(!!sym(species_col) == pair[2])
      
      # Calculate the Z-statistic and p-value for every possible pair
      z_statistic <- abs(species_1$z - species_2$z) / sqrt(species_1$se^2 + species_2$se^2)
      p_value <- 2 * (1 - pnorm(z_statistic))  
      # Generating a two-tailed p-value
      
      # Input results into a dataframe
      data.frame(
        species_1 = pair[1],
        species_2 = pair[2],
        correlation_diff = round(abs(species_1$correlation - species_2$correlation), 3),
        z_statistic = round(z_statistic, 3),
        p_value = round(p_value, 3)
      )
    }) %>%
    bind_rows() %>%
    arrange(p_value)  # Sort by p-value
  
  # Format the results, changing column names to be more reader-friendly.
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