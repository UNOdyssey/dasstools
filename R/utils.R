#' @keywords internal
"_PACKAGE"

#' @importFrom data.table is.data.table haskey key copy setkeyv setorderv shift setnames fifelse
#' @importFrom stringr str_count str_extract str_split regex
NULL

# Silence data.table NSE notes in R CMD check
utils::globalVariables(c(
  ".SD", ".N", "i.to", "from", "to",
  "dimension", "code", "label",
  "sdmx_dictionary",
  ".dup_check", ".campaign_year",
  "prev_year", "prev_value",
  "value_computed", "d_abs", "d_rel",
  "check", "campaign"
))
str_split_1 <- function(string, pattern) {
  stringr::str_split(string, pattern, n = 2, simplify = TRUE)
}
