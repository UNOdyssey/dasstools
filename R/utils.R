str_split_1 <- function(string, pattern) {
  stringr::str_split(string, pattern, n = 2, simplify = TRUE)
}
