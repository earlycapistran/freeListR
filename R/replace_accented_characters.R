#' Replace Accented Characters with Non-Accented Characters in a Dataframe
#'
#' This function replaces accented characters with their
#' non-accented counterparts in all columns that contain
#' strings of a dataframe. Contains accents and special
#' characters common across European languages.
#'
#' @param df The input dataframe.
#'
#' @return A new dataframe with accented characters
#' replaced by non-accented characters in the respective
#' columns.
#'
#' @examples
#' # Example dataframe
#' df <- data.frame(
#'   Name = c("Café", "Héllo", "Ëxample"),
#'   Age = c(25, 30, 35),
#'   City = c("México", "São Paulo", "Ñuñoa")
#' )
#'
#' Replace accented characters in all columns containing
#' strings
#' df_modified <- replace_accented_characters(df)
#' print(df_modified)
#'
#' @export

replace_accented_characters <- function(df) {
  # Define the accented characters and their non-accented
  #counterparts
  accented_chars <- c("á", "é", "í", "ó", "ú", "ü", "ñ",
                      "à", "è", "ì", "ò", "ù", "â", "ê",
                      "î", "ô", "û", "ä", "ë", "ï", "ö",
                      "ü", "ÿ", "ç", "Á", "É", "Í", "Ó",
                      "Ú", "Ü", "Ñ", "À", "È", "Ì", "Ò",
                      "Ù", "Â", "Ê", "Î", "Ô", "Û", "Ä",
                      "Ë", "Ï", "Ö", "Ü", "Ÿ", "Ç", "ã",
                      "õ", "Ã", "Õ")
  non_accented_chars <- c("a", "e", "i", "o", "u", "u",
                          "n", "a", "e", "i", "o", "u",
                          "a", "e", "i", "o", "u", "a",
                          "e", "i", "o", "u", "y", "c",
                          "A", "E", "I", "O", "U", "U",
                          "N", "A", "E", "I", "O", "U",
                          "A", "E", "I", "O", "U", "A",
                          "E", "I", "O", "U", "Y", "C",
                          "a", "o", "A", "O")


  # Iterate over all columns
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      # Replace accented characters with non-accented characters in the current column
      df[[col]] <- sapply(df[[col]], function(text) {
        for (i in 1:length(accented_chars)) {
          text <- gsub(accented_chars[i],
                       non_accented_chars[i],
                       text,
                       fixed = TRUE)
        }
        return(text)
      })
    }
  }

  return(df)
}
