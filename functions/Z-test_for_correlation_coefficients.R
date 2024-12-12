# Defining a function (saved in functions folder, "Z-test_for_correlation_coefficients.R") to analyse statistical significance of the difference between correlation coefficients across species, data= data source (penguins_clean), var1= variable 1 (culmen length), var2= variable 2 (culmen depth), and species= species column (species).

compare_species_correlations <- function(data, var1, var2, species) {
  
  # Creating a new variable (correlation_results)  that contains a species' Z score, and it's respective SE.
  Z_stat_results <- data %>%
    group_by({{species}}) %>%
    summarise(correlation = cor({{var1}}, {{var2}}, use = "complete.obs"),
              n = n()) %>%
    # Calculates a correlation coefficient between variable 1 and 2 for each species, only using complete rows and columns, calculating sample size for each species
    mutate(
      z = 0.5 * log((1 + correlation) / (1 - correlation)),  
      # Creates a new column, transforming correlation coefficients into a Z score
      se = 1 / sqrt(n - 3)  
      # Creates a new column containing the standard error for each Z score
    )
  
  species_list <- Z_stat_results %>% pull({{species}}) %>% unique()
  # Identifies all the unique species names in a data frame, and stores them in the species_list vector ("Adelie", "Chinstrap", "Gentoo")
  
  Z_stat_results_comparison <- combn(species_list, 2, simplify = FALSE) %>%
    # Generates a list of all possible pairs of species from the species_list vector, ["Adelie", "Chinstrap"], etc...
    lapply(function(pair) {
      species_1 <- Z_stat_results %>% filter({{species}} == pair[1])
      species_2 <- Z_stat_results %>% filter({{species}} == pair[2])
      # Extracts species 1 and 2 from each pair (e.g, pair[1]=["Adelie"] pair[2]=["Chinstrap"]) assigning them to separate vectors (species_1 and species_2).
      z_stat <- abs(species_1$z - species_2$z) / sqrt(species_1$se^2 + species_2$se^2)
      p_value <- 2 * (1 - pnorm(z_stat))  
      # Calculates the absolute difference in z-statistic between every species pair, calculating a two-tailed p-value corresponding with this difference.
      data.frame(
        `Species 1` = pair[1],
        `Species 2` = pair[2],
        `Difference in Correlation Coefficient` = round(abs(species_1$correlation - species_2$correlation), 3),
        `Z-Statistic` = round(z_stat, 3),
        `p-value` = round(p_value, 3))
      # Generates a data frame for the outputted data, altered column titles and rounded numbers to enhance readability.
    }) %>%
    bind_rows() 
  # Combines rows relating to different species pairs into a single data frame.
  return(Z_stat_results_comparison)
  # Send result of the function to the user, enabling its use outside of the function.
}
