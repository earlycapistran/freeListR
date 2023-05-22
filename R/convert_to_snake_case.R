# =============================================================================
# Convert text in dataframe to snake case
# earlyc@stanford.edu - May, 2023
# ========================================================

#' This function replaces spaces with underscores and
#' converts all strings in a dataframe.to lowercase
#'
#' @param df The input dataframe.
#'
#' @return A new dataframe with spaces replaced
#' by underscores and all strings converted to lowercase.
#'
#' @examples
#' input_df <- data.frame(
#'   Name = c("Fulano A", "Sutano B", "Merengano C"),
#'   Age = c(25, 30, 35),
#'   City = c("New York", "Mexico City", "Lima"),
#'   "df
#' )
#'
#' # Replace spaces and convert to lowercase in the dataframe
#' output_df <- replace_spaces_lowercase_df(input_df)
#' print(output_df)
#'
#' @export

convert_to_snake_case <- function(df) {
  # Function to convert a string to snake case
  to_snake_case <- function(my_text) {
    # Split words by spaces
    words <- gsub(" ", "_", my_text)
    # Convert to lowercase
    words <- tolower(words)
    return(words)
  }

  # Convert column names to snake case
  colnames(df) <- sapply(colnames(df), to_snake_case)

  # Iterate over all columns
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      # Convert values in the current column to snake case
      df[[col]] <- sapply(df[[col]], to_snake_case)
    }
  }

  # Convert the updated data back to a dataframe
  updated_df <- as.data.frame(df)
  return(updated_df)
}
