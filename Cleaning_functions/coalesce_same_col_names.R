coalesce_joined_columns <- function(df) {
  # Identify columns ending in .x
  x_cols <- grep("\\.x$", names(df), value = TRUE)
  
  # Get base names without the .x suffix
  base_cols <- str_remove(x_cols, "\\.x$")
  
  for (base in base_cols) {
    y_cols <- grep(paste0("^", base, "\\.y"), names(df), value = TRUE)
    all_cols <- c(paste0(base, ".x"), y_cols)
    
    df <- df %>%
      mutate(!!base := reduce(select(df, all_cols), coalesce)) %>%
      select(-any_of(all_cols)) # remove the original .x, .y columns after coalescing
  }
  return(df)
}
