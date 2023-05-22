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
#'   City = c("New York", "Mexico City", "Lima")
#' )
#'
#' # Replace spaces and convert to lowercase in the dataframe
#' output_df <- replace_spaces_lowercase_df(input_df)
#' print(output_df)
#'
#' @export

convert_to_snake_case <- function(df) {
  # Replace spaces with underscores and convert to lowercase
  updated_df <- lapply(df, function(my_text) {
    my_text <- gsub(" ", "_", my_text)
    my_text <- tolower(my_text)
    return(my_text)
  })

  # Convert the updated data back to a dataframe
  updated_df <- as.data.frame(updated_df)

  cat("Spaces replaced with underscores and converted
      to lowercase in dataframe successfully!\n")

  return(updated_df)
}
