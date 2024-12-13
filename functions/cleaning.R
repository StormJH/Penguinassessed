# Defining a function to clean data, removes spaces, empty rows and columns, columns with titles beginning with delta or comments, and any NA values:

cleaning_penguin_columns <- function(penguins_raw) {
  penguins_raw %>%
    clean_names() %>%
    # Removes spaces between column titles.
    
    remove_empty(c("rows", "cols")) %>%
    # Removes empty columns and rows
    
    select(-starts_with("delta")) %>%
    # Selectively removes columns with titles beginning with "delta"
    
    select(-"comments") %>%
    # Selectively removes the "comments" columns
    
    drop_na()  
  # Removes NA values
}