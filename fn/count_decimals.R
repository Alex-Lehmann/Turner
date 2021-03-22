library(tidyverse)

count_decimals = function(x) {
  # Check that input is numeric
  if (!is.numeric(x) | is.na(x)) {
    message("Input not numeric; returning NA")
    return(NA)
  }
  
  # Count nutmber of characters after decimal point
  l = x %>%
    as.character() %>%
    str_extract("(?<=\\.).*") %>%
    str_length()
  return(l)
}
