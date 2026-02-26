
###################################################################################
#' Add aggregated groups to an SDMX data.table
#'
#' Adds a new category (code) to a dimension column by aggregating existing
#' categories according to a rule (e.g., "_T:=M+F" or "_T:=sum(M,F)").
#'
#' @param x Keyed data.table.
#' @param col Dimension column to aggregate (e.g., sex).
#' @param rule Aggregation rule (e.g., "_T:=M+F", "_T:=sum(M,F)", "_T:=...").
#' @param filter Optional filter expression (string).
#' @return data.table including the new aggregated rows.
#' @examples
#' library(data.table)
#' dt <- data.table(
#'   geo = c("AGO","AGO"),
#'   year = c("2023","2023"),
#'   indicator = c("drug use","drug use"),
#'   sex = c("M","F"),
#'   unit_measure = c("NB","NB"),
#'   value = c(40, 60)
#' )
#' setkey(dt, geo, year, indicator, sex)
#' add_group(dt, sex, "_T2:=M+F")
#' @export
add_group <- function(x, col, rule = "", filter = NULL) {

  if (!is.data.table(x) || !haskey(x)) {
    warning(sprintf("Error: '%s' must be a keyed data.table.", deparse(substitute(x))), call. = FALSE)
    return(NA)
  }
  x_key <- key(x)

  vars_num <- names(x)[sapply(x, is.numeric)]
  num <- setdiff(vars_num, c("upper_bound", "lower_bound"))
  if (length(num) != 1) {
    warning(sprintf("Error: Expected one numeric column (excluding 'upper_bound' and 'lower_bound'), found %d.", length(num)), call. = FALSE)
    return(NA)
  }

  vars_char <- names(x)[sapply(x, is.character)]
  vars_char <- setdiff(vars_char, c("obs_status", "obs_valid", key(x)))

  col <- deparse(substitute(col))
  if (missing(col) || !(col %in% names(x))) {
    warning(sprintf("Error: Column '%s' is missing or not found in data.table.", col), call. = FALSE)
    return(NA)
  }

  str_parts <- str_split_1(rule, ":=")
  str_left  <- str_parts[1]
  str_right <- str_parts[2]

  check_type <- str_extract(rule, "max|min|sum")
  if (is.na(check_type)) check_type <- "sum"
  str_right_clean <- gsub("\\(|\\)|sum", "", str_right)

  codes <- if (str_right_clean == "...") {
    setdiff(unique(x[[col]]), str_left)
  } else {
    str_split_1(
      str_right_clean,
      if (check_type != "sum" || (check_type == "sum" && !grepl("+", str_right_clean, fixed = TRUE))) "," else "\\+"
    )
  }

  temp <- copy(x)

  if (!missing(filter)) {
    temp <- temp[eval(parse(text = filter))]
    if (nrow(temp) == 0L) {
      warning(sprintf("No records left after applying the filter: %s", filter), call. = FALSE)
      out <- temp[0]
      if (length(x_key)) setkeyv(out, x_key)
      return(out)
    }
  }

  if (temp[, any(get(col) == str_left, na.rm = TRUE)]) {
    message(sprintf("The category '%s' already exists! No row has been added.", str_left))
    if (length(x_key)) setkeyv(temp, x_key)
    return(temp)
  } else {
    new_code <- str_left
  }

  by <- key(temp)

  if (!missing(filter)) {
    temp <- temp[eval(parse(text = filter))]
    if (nrow(temp) == 0L) {
      warning(sprintf("No records left after applying the filter: %s", filter), call. = FALSE)
      out <- temp[0]
      if (length(x_key)) setkeyv(out, x_key)
      return(out)
    }
  }

  temp_right <- temp[get(col) %chin% codes]
  if (nrow(temp_right) == 0L) {
    message("No records to aggregate found.")
    out <- temp[0]
    if (length(x_key)) setkeyv(out, x_key)
    return(out)
  }

  temp_right[, (col) := new_code]

  obs_status_e <- intersect("obs_status", names(temp_right))
  obs_valid_e  <- intersect("obs_valid", names(temp_right))

  fun_collapse_char <- function(x) {
    x <- unique(x[!is.na(x)])
    if (length(x) == 0) return(NA_character_)
    paste(x, collapse = ", ")
  }
  fun_obs_status <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    if (all(x == "M")) "M" else "T"
  }
  fun_obs_valid <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    if (all(x == "P")) "P" else "_U"
  }

  agg_result <- temp_right[
    , c(
      lapply(.SD[, ..vars_num], sum, na.rm = TRUE),
      lapply(.SD[, ..vars_char], fun_collapse_char),
      if (length(obs_status_e)) lapply(.SD[, ..obs_status_e], fun_obs_status),
      if (length(obs_valid_e))  lapply(.SD[, ..obs_valid_e],  fun_obs_valid)
    ),
    by = by,
    .SDcols = c(vars_num, vars_char, obs_status_e, obs_valid_e)
  ]

  x <- rbind(x, agg_result, fill = TRUE)
  setkeyv(x, x_key)
  return(x[])
}
