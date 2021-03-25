library(tidyverse)

count_decimals = function(x) {
  # Count number of characters after decimal point
  l = x %>%
    as.character() %>%
    str_extract("(?<=\\.).*") %>%
    sapply(nchar) %>%
    replace_na(0)
  
  return(l)
}
