
#This function removes spaces and capitals in column names, removes empty rows and columns, removing columns beginning with delta, and comments column.

cleaning_penguin_columns <- function(penguins_raw){
  penguins_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>% 
    select(-starts_with("delta")) %>%   
    select(-"comments")                   
}