#' Function assign_sensitivity_label
#'v1.02
#' This function assigns an UN sensitivity label to a Excel file.
#' It launches through PowerShell from R to run an Excel VBA macro inside a
#' specified .xlsx workbook. It opens the workbook, executes the macro with
#' user-provided parameters, and then closes Excel cleanly.
#'
#' @param target_file Character string. Path to the input file that the VBA
#'   macro should process. The file must exist.
#'
#' @param label Character string. Sensitivity label to apply. Must be one of:
#'   "Public", "Unclassified", "Confidential", or "Strictly Confidential".
#'
#' @param macro_host Character string. Path to the Excel .xlsm file containing
#'   the VBA macro to run.
#'
#' @param macro_name Character string. The full VBA macro name to execute,
#'   including module name if required (e.g., "Module1.MyMacro").

#' @return A list with:
#'   \item{output}{Character vector of PowerShell output (stdout/stderr).}
#'   \item{status}{Exit status code from system2 (0 = success).}
#'
#' @details
#' The function builds a PowerShell script that:
#'   - launches Excel via COM automation,
#'   - runs the specified VBA macro with arguments,
#'   - closes Excel and cleans up orphan processes.
#'
#' @examples
#' \dontrun{
#' assign_sensitivity_label(
#'   target_file = "C:/mypath/myfile.xlsx",
#'   label = "Confidential",
#'   macro_host = "C:/mypath/apply_sensitivity_labels.xlsm",
#'   macro_name = "ApplySensitivityLabelToFileMain"
#' )
#' }


#' @export
assign_sensitivity_label <- function(
    target_file,
    label,
    macro_host = NULL,
    macro_name = "ApplySensitivityLabelToFileMain"
) {

  if (Sys.info()[["sysname"]] != "Windows") {
    stop("assign_sensitivity_label() only works on Windows (PowerShell + Excel COM required).")
  }

  if (is.null(macro_host)) {
    stop("Please provide macro_host path (Excel .xlsm file containing the macro).")
  }

  if (!file.exists(macro_host)) {
    stop(sprintf("macro_host file not found: %s", macro_host))
  }
  if (!grepl("\\.xlsm$", macro_host, ignore.case = TRUE)) {
    stop("macro_host must be an Excel .xlsm file.")
  }


  error_message <- function(msg) {
    # Build a call string like base R's "Error in <call>: ..."
    call <- deparse(sys.call(-1))
    # ANSI codes: red = \033[31m, bold red = \033[1;31m, reset = \033[0m
    red   <- "\033[31m"
    reset <- "\033[0m"
    message(sprintf("%sError in %s: %s%s", red, call, msg, reset))
  }


  #bulk processing will be implemented later
  if (length(target_file)>1) {
    error_message("You supplied more than one file. Please provide exactly one file for processing.")
    return(invisible(NULL))
  }

  #validate the sensitivity label
  valid_labels <- c("Public","Unclassified","Confidential","Strictly Confidential")
  if (!(label %in% valid_labels)) {
    error_message(paste0(
      "The sensitivity label '", label, "' isn't supported.\n",
      "Valid options: ", paste(valid_labels, collapse = ", "), ".\n",
      "Note: 'Strictly Confidential - Additional Protection' is not implemented.\n",
      "More info: https://iseek.un.org/department/m365-information-sensitivity-labels."
    ))
    return(invisible(NULL))
  }

  #validate the list of files extensions
  invalid_ext <- !grepl("\\.xlsx$", target_file, ignore.case = TRUE)
  if (any(invalid_ext)) {
    error_message(paste0(
      "The following file(s) are not valid Excel .xlsx files: \n - ",
      paste(target_file[invalid_ext], collapse = "\n  - ")
    ))
    return(invisible(NULL))
  }

  #confirm file existence
  missing_files <- !file.exists(target_file)
  if (any(missing_files)) {
    error_message(paste0(
      "The following file(s) do not exist: \n - ",
      paste(target_file[missing_files], collapse = "\n - "),
      sep = "\n  - "
    ))
    return(invisible(NULL))
  }

  keep <- !(invalid_ext | missing_files)
  target_file <- target_file[keep]

  # If macro_host contains "[user]" placeholder, replace it with the Windows username (capitalized)
  if (grepl("\\[user\\]", macro_host, fixed = FALSE)) {
    ucfirst <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
    user_uc <- ucfirst(Sys.getenv("USERNAME"))
    macro_host <- sub("\\[user\\]", user_uc, macro_host)
  }


  # Use Windows backslashes for Excel
  macro_host  <- normalizePath(macro_host, winslash = "\\", mustWork = TRUE)
  target_file <- normalizePath(target_file, winslash = "\\", mustWork = TRUE)

  ps <- paste0(
    "$ErrorActionPreference = 'Stop'; ",
    '$excel = New-Object -ComObject Excel.Application; ',
    '$excel.Visible = $false; $excel.DisplayAlerts = $false; ',
    '$wb = $excel.Workbooks.Open(', "'", macro_host, "'", '); ',
    '$excel.Run(', "'", macro_name, "'", ', ', "'", target_file, "'", ', ', "'", label, "'", '); ',
    '$wb.Close($false); ',
    '$excel.Quit(); ',
    '[System.Runtime.InteropServices.Marshal]::ReleaseComObject($excel) | Out-Null; ',
    'Start-Sleep -Milliseconds 200; ',
    'Get-Process -Name EXCEL -ErrorAction SilentlyContinue | ForEach-Object { if ($_.MainWindowHandle -eq 0) { $_ | Stop-Process -Force } }'
  )

  out <- system2(
    command = "powershell.exe",
    args    = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-Command", ps),
    stdout  = TRUE,
    stderr  = TRUE
  )

  status <- attr(out, "status"); if (is.null(status)) status <- 0
  if (status!=0) list(output = out, status = status)

  if (status==0) message(paste0("Sensitivity label '",label,"' successfully assigned to : ", target_file))
  return(invisible(NULL))

}
