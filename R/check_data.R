###################################################################################
## Functions for the SDMX data management of UNODC data collections
## The functions require an SDMX input file following a standardized SDMX structure
###################################################################################

#' Validate SDMX data with logical and time-series checks
#'
#' @title check.data – Data validation for SDMX datasets
#'
#' @description
#' This function checks the internal consistency of SDMX‑structured data.
#' It supports two types of validation:
#'
#' 1) Logical checks (cross-sectional):
#'    These checks compare categories inside the same SDMX group (that is,
#'    all key columns except the column provided in `col`). They allow you to verify
#'    consistency across dimensions such as sex, age, education, etc.
#'
#'    Typical examples:
#'      - "_T==M+F"
#'      - "_T==sum(M,F)"
#'      - "_T>=max(M,F)"
#'      - "_T==100"
#'      - "_Z==..."  (sum of all categories except "_Z")
#'
#'    Logical equality checks use a default tolerance of c(0, 0.1):
#'      absolute difference ≤ 0 OR relative difference ≤ 0.1%
#'
#'    For these equality checks, only compatible unit_measure values are used:
#'    NB, NB_SAMPLE, PCT_DIST.
#'
#'
#' 2) Time-series checks:
#'    These checks compare a value to earlier periods.
#'
#'    The supported rules are:
#'      - "yoy"   (one period back)
#'      - "yoNy"  (N periods back, e.g., "yo2y")
#'
#'    Time-series checks require the user to supply a tolerance such as c(10, 25),
#'    meaning absolute change ≤ 10 OR relative change ≤ 25%.
#'
#'    Non-ratio-scale unit_measure values are removed automatically:
#'    YES_NO_UNK, SCALE_L5, SCALE_10, PCT_BOUND.
#'
#'
#' @details
#' The rule always refers to the values of the dimension supplied in `col`.
#'
#' For example, if col = sex, then "_T==M+F" means:
#'   "Within each SDMX key-group, the row where sex == '_T' should equal
#'    the sum of rows where sex == 'M' and sex == 'F'."
#'
#' Accepted logical rule forms include:
#'   - Equality with sums: "_T==M+F" or "_T==sum(M,F)"
#'   - Maximum or minimum bounds: "_T>=max(M,F)"
#'   - Constants: "_T==100"
#'   - Wildcard sums: "_Z==..."  (sum of all categories except "_Z")
#'
#' For time-series checks, the column provided in `col` must be convertible to numeric.
#'
#'
#' @param x A keyed data.table in SDMX long format containing exactly one numeric measure.
#' @param col The dimension to which the rule refers; or the time column for time-series checks.
#' @param rule A logical expression (e.g. "_T==M+F") or a time rule ("yoy", "yo2y").
#' @param tolerance A numeric vector of length 2: c(abs_tolerance, rel_tolerance).
#'   The default is c(0, 0.1). Any user-supplied tolerance overrides the default.
#' @param label Optional descriptive label for the rule.
#' @param id Optional identifier for the rule.
#' @param filter Optional expression to subset the data before the check.
#' @param keep_valid If FALSE (default), only failing rows are returned. If TRUE, all rows are returned.
#'
#' @return
#' A data.table with:
#'   - value_computed (for logical checks),
#'   - absolute and relative change columns (for time-series checks),
#'   - check (TRUE or FALSE),
#'   - rule, label, id (metadata columns documenting the validation performed).
#'
#'
#' @section Rule syntax summary:
#' - The rule must contain exactly one comparison operator among: <=, >=, !=, ==, >, <
#' - Subtraction, multiplication, and division are not allowed in the rule.
#' - Sums can be written using commas or plus signs: "_T==sum(M,F)" or "_T==M+F"
#'
#'
#' @examples
#' ## Example 1: Logical (cross-sectional) check
#' library(data.table)
#' dt <- data.table(
#'   geo = "AGO",
#'   year = "2023",
#'   indicator = "drug use",
#'   sex = c("M","F","_T"),
#'   unit_measure = "NB",
#'   value = c(40, 60, 100)
#' )
#' setkey(dt, geo, year, indicator, sex)
#'
#' check.data(
#'   dt,
#'   sex,
#'   "_T==M+F",
#'   keep_valid = TRUE
#' )
#'
#'
#' ## Example 2: Time-series (YoY) check
#' dt2 <- data.table(
#'   geo = "AGO",
#'   year = c("2020", "2021"),   # years must be strings
#'   indicator = "drug use",
#'   sex = "_T",
#'   unit_measure = "NB",
#'   value = c(100, 130)
#' )
#' setkey(dt2, geo, year, indicator, sex)
#'
#' check.data(
#'   dt2,
#'   year,
#'   "yoy",
#'   tolerance = c(10, 0.25),
#'   keep_valid = TRUE
#' )
#'
#' @export
check.data <- function(x, col,
                       rule = "", tolerance = c(0, 0.1),
                       label = NULL, id = NULL,
                       filter = NULL, keep_valid = FALSE) {

  #### ---- Input checks ----

  if (!is.data.table(x) || !haskey(x)) {
    warning(sprintf("Error: '%s' must be a keyed data.table.", 
      deparse(substitute(x))), call. = FALSE)
    return(NA)
  }
  num <- setdiff(names(x)[sapply(x, is.numeric)], c("upper_bound", 
    "lower_bound"))
  if (length(num) != 1) {
    warning(sprintf("Error: Expected one numeric column (excluding 'upper_bound' and 'lower_bound'), found %d: %s.", 
      length(num), paste(num, collapse = ", ")), call. = FALSE)
    return(NA)
  }
  col <- deparse(substitute(col))
  if (!(col %in% names(x))) {
    warning(sprintf("Error: Column '%s' not found in data.table.", 
      col), call. = FALSE)
    return(NA)
  }
  if (!is.numeric(tolerance) || length(tolerance) != 2) {
    warning("Error: 'tolerance' must be numeric c(abs, rel).", 
      call. = FALSE)
    return(NA)
  }

  #### ---- Validate rule syntax ----

  rule <- gsub(" ", "", rule, fixed = TRUE)
  operator_pattern <- "(<=|>=|!=|==|>|<)"
  bad_syntax <- (!grepl(operator_pattern, rule, perl = TRUE) && 
    !grepl("^yoy$|^yo[0-9]+y$", rule)) || str_count(rule, 
    operator_pattern) > 1
  if (bad_syntax) {
    warning(sprintf("Error: The syntax of the validation rule '%s' is wrong.", 
      rule), call. = FALSE)
    return(NA)
  }
  if (grepl("-|\\*|:", rule)) {
    warning(sprintf("Error: Operator '%s' is not allowed.", 
      str_extract(rule, "-|\\*|:")), call. = FALSE)
    return(NA)
  }

  #### ---- Determine type of check ----

  if (grepl("^yoy$|^yo[0-9]+y$", rule)) {
    check_type <- "timeseries"
  }
  else {
    parts <- str_split_1(rule, operator_pattern)
    op <- str_extract(rule, operator_pattern)
    left <- parts[1]
    right <- parts[2]
    check_type <- str_extract(rule, "max|min|sum")
    if (is.na(check_type)) 
      check_type <- "sum"
    right_clean <- gsub("\\(|\\)|max|min|sum", "", right)
    codes <- if (right_clean == "...") {
      setdiff(unique(x[[col]]), left)
    }
    else {
      if (check_type == "sum") 
        str_split_1(right_clean, "[,\\+]")
      else str_split_1(right_clean, "\\+")
    }
    if (check_type == "sum" && length(codes) == 1 && !is.na(suppressWarnings(as.numeric(codes)))) {
      codes <- as.numeric(codes)
      check_type <- "const"
    }
    new_code <- left
  }

  #### ---- Apply check ----

   temp <- copy(x)
  by <- setdiff(key(temp), col)
  if (!is.null(filter)) {
    temp <- temp[eval(parse(text = filter))]
    if (nrow(temp) == 0L) {
     out <- temp[0][, `:=`(
  value_computed = NA_real_,
  check          = NA,
  d_abs          = NA_real_,
  d_rel          = NA_real_,
  rule           = NA
)]
      setkeyv(out, key(x))
      return(out)
    }
  }

  #### ---- Logical checks ----
  
  is_sum_like <- exists("op") && op == "==" && check_type %in% 
    c("sum", "const")
  if (is_sum_like && "unit_measure" %in% names(temp)) {
    allowed <- c("NB", "NB_SAMPLE", "PCT_DIST")
    before <- nrow(temp)
    temp <- temp[unit_measure %chin% allowed]
    after <- nrow(temp)
    if (before != after) {
      message(sprintf("Equality check on totals: kept only rows with unit_measure {%s}. %d/%d rows retained.", 
        paste(allowed, collapse = ", "), after, before))
    }
    if (after == 0L) {
     out <- temp[0][, `:=`(
  value_computed = NA_real_,
  check          = NA,
  d_abs          = NA_real_,
  d_rel          = NA_real_,
  rule           = NA
)]
      setkeyv(out, key(x))
      return(out)
    }
  }
  if (check_type %in% c("sum", "max", "min", "const")) {
    temp_left <- temp[get(col) %chin% new_code]
    if (nrow(temp_left) == 0L) {
      out <- temp[0][, `:=`(
  value_computed = NA_real_,
  check          = NA,
  d_abs          = NA_real_,
  d_rel          = NA_real_,
  rule           = NA
)]
      setkeyv(out, key(x))
      return(out)
    }
    temp_right <- if (check_type != "const") 
      temp[get(col) %chin% codes]
    else temp
    if (nrow(temp_right) == 0L) {
    out <- temp[0][, `:=`(
  value_computed = NA_real_,
  check          = NA,
  d_abs          = NA_real_,
  d_rel          = NA_real_,
  rule           = NA
)]
      setkeyv(out, key(x))
      return(out)
    }
    temp_right[[col]] <- new_code
    agg <- temp_right[, .(value_computed = switch(check_type, 
      sum = sum(get(num)), min = min(get(num)), max = max(get(num)), 
      const = codes)), by = by]
    result <- merge(temp_left, agg, by = by, all = TRUE)
    if (op == "==") {
      abs_tol <- tolerance[1]
      rel_tol <- tolerance[2]
      result[, `:=`(d_abs, abs(get(num) - value_computed))]
      result[, `:=`(d_rel, d_abs/pmax(abs(value_computed), 
        1e-12) * 100)]
      result[, `:=`(check, (d_abs <= abs_tol) | (d_rel <= 
        rel_tol))]
    }
else {
  result[, `:=`(
    check = eval(parse(text = sprintf("%s %s value_computed", num, op))),
    d_abs = NA_real_,
    d_rel = NA_real_
  )]
}
  }

  #### ---- Time-series checks ----
  if (check_type == "timeseries") {
    years_back <- if (rule == "yoy") 
      1
    else as.numeric(sub("yo([0-9]+)y", "\\1", rule))
    if ("unit_measure" %in% names(temp)) {
      excluded <- c("YES_NO_UNK", "SCALE_L5", "SCALE_10", 
        "PCT_BOUND")
      before <- nrow(temp)
      temp <- temp[!(unit_measure %chin% excluded)]
      after <- nrow(temp)
      if (before != after) {
        message(sprintf("Time-series check: removed categorical or bounded unit_measure {%s}. %d/%d rows kept.", 
          paste(excluded, collapse = ", "), after, before))
      }
      if (after == 0L) {
       out <- temp[0][, `:=`(
  value_computed = NA_real_,
  check          = NA,
  d_abs          = NA_real_,
  d_rel          = NA_real_,
  rule           = NA
)]
        setkeyv(out, key(x))
        return(out)
      }
    }
    if (!is.numeric(temp[[col]])) {
      temp[, `:=`((col), as.numeric(get(col)))]
      if (all(is.na(temp[[col]]))) {
        warning(sprintf("Error: Column '%s' cannot be converted to numeric.", 
          col), call. = FALSE)
        return(NA)
      }
    }
setorderv(temp, c(by, col))

temp[, .count := .N, by = by]
temp <- temp[.count >= (years_back + 1L)]
temp[, .count := NULL]

temp[, prev_year  := shift(get(col)), by = by]
temp[, prev_value := shift(get(num)), by = by]
temp <- temp[!is.na(prev_year)]

# --- absolute and relative change (canonical columns) ---
temp[, d_abs := abs(get(num) - prev_value)]

temp[, d_rel := fifelse(
  prev_value != 0,
  round(abs((get(num) - prev_value) / prev_value) * 100, 0),
  NA_real_
)]

abs_tol <- tolerance[1]
rel_tol <- tolerance[2]

temp[, check := fifelse(
  is.na(prev_value),
  NA,
  d_abs <= abs_tol | d_rel <= rel_tol
)]

result <- temp
  }
  
  #### ---- Add metadata ----
  result[, `:=`(rule, paste0(col, ": ", rule))]
  if (!is.null(label)) 
    result[, `:=`(label, label)]
  if (!is.null(id)) 
    result[, `:=`(id, id)]
  if (!keep_valid) 
    result <- result[check == FALSE]

  #### ---- Restore original key ----
  setkeyv(result, key(x))
  return(result[])
}
