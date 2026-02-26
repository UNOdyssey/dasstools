
###################################################################################
#' Replace SDMX codes with labels
#'
#' Recode SDMX codes using a mapping table (by default `sdmx_dictionary`).
#'
#' @param x data.table (keyed recommended).
#' @param show "label" (default) to display labels, or "code" to reverse-map.
#' @param language Language code (e.g., "en", "fr").
#' @param mapping_table Table with columns: dimension, code, label_<language>.
#' @return data.table with recoded values.
#' @examples
#' library(data.table)
#' dt <- data.table(sex = c("M","_T"), value = c(1, 2))
#' dict <- data.table(
#'   dimension = c("sex","sex"),
#'   code = c("M","_T"),
#'   label_en = c("Male","Total"),
#'   label_fr = c("Hommes","Total")
#' )
#' show_labels(dt, language = "en", mapping_table = dict)
#' dt_fr <- show_labels(dt, language = "fr", mapping_table = dict)
#' show_labels(dt_fr, show = "code", language = "fr", mapping_table = dict)
#' @export
show_labels <- function(x, show = "label", language = "en", mapping_table = sdmx_dictionary, ...) {

  mt <- copy(mapping_table)
  temp <- copy(x)

  if (!is.data.table(mt)) setDT(mt)
  if (!is.data.table(temp)) setDT(temp)

  dim_sdmx <- colnames(temp)

  label_col <- paste0("label_", language)
  mt <- mt[, c("dimension", "code", label_col), with = FALSE]
  setnames(mt, label_col, "label")

  if (show == "code") setnames(mt, c("code", "label"), c("label", "code"))

  dim_list <- lapply(dim_sdmx, function(dim) mt[dimension == dim, .(from = code, to = label)])
  names(dim_list) <- dim_sdmx

  for (v in intersect(names(dim_list), colnames(temp))) {
    temp[dim_list[[v]], on = paste0(v, "==from"), (v) := i.to]
  }

  if (haskey(x)) setkeyv(temp, key(x))
  return(temp)
}
