#' Assign a UN Sensitivity Label to an Excel Workbook
#'
#' This function embeds a UN-compliant Microsoft Information Protection (MIP)
#' sensitivity label directly into an Excel `.xlsx` file by injecting the
#' required XML metadata. Unlike previous implementations that relied on
#' PowerShell, COM automation, or external VBA macros, this version applies
#' sensitivity labels natively using the **openxlsx2** engine.
#'
#' The function loads an existing workbook, adds the appropriate MIP XML
#' corresponding to the selected label ("Unclassified", "Public", or
#' "Confidential"), and then saves the workbook in-place. No external
#' dependencies, macros, or Windows-only features are required.
#'
#' @param filename_full_dir Character string. The full file path of the Excel
#'   workbook to modify (e.g., `"C:/folder/myfile.xlsx"`). The file must already
#'   exist and be a valid `.xlsx` file.
#'
#' @param selected_label Character string. The sensitivity label to assign.
#'   Must be one of:
#'   - `"Unclassified"`
#'   - `"Public"`
#'   - `"Confidential"`
#'
#' @details
#' This function works by injecting a `<clbl:labelList>` XML element compliant
#' with the Microsoft Information Protection (MIP) schema. Each supported label
#' corresponds to a predefined GUID and metadata block that Excel and other
#' Office applications recognize as an applied sensitivity classification.
#'
#' Only the metadata is written; no content scanning or encryption is performed.
#' The file is overwritten in-place using `openxlsx2::wb_save()`.
#'
#' @return
#' Invisibly returns `TRUE` on success. The function is called for its side
#' effects (writing the label metadata to the file).
#'
#' @examples
#' \dontrun{
#' assign_label(
#'   filename_full_dir = "C:/data/myworkbook.xlsx",
#'   selected_label    = "Confidential"
#' )
#' }
#'
#' @export

# Add label
assign_label <- function(filename_full_dir, selected_label){

  # filename_full_dir  ful path to the file and the file name
  # Step 1 create a data frame with the labels and their metadata
  labels_class <- data.frame(c("Unclassified", "Public", "Confidential"),
                             c('<clbl:labelList xmlns:clbl=\"http://schemas.microsoft.com/office/2020/mipLabelMetadata\"><clbl:label id=\"{8b77875e-5908-45a0-9cb4-dec9ae074618}\" enabled=\"1\" method=\"Privileged\" siteId=\"{0f9e35db-544f-4f60-bdcc-5ea416e6dc70}\" removed=\"0\"/></clbl:labelList>',
                               '<clbl:labelList xmlns:clbl=\"http://schemas.microsoft.com/office/2020/mipLabelMetadata\"><clbl:label id=\"{606bed3f-efae-4d70-a15b-866bb27c918d}\" enabled=\"1\" method=\"Privileged\" siteId=\"{0f9e35db-544f-4f60-bdcc-5ea416e6dc70}\" contentBits=\"0\" removed=\"0\"/></clbl:labelList>' ,
                               '<clbl:labelList xmlns:clbl=\"http://schemas.microsoft.com/office/2020/mipLabelMetadata\"><clbl:label id=\"{7eb58d0f-f804-411f-a20e-09ebfae62b4c}\" enabled=\"1\" method=\"Privileged\" siteId=\"{0f9e35db-544f-4f60-bdcc-5ea416e6dc70}\" contentBits=\"0\" removed=\"0\"/></clbl:labelList>'
                             ))

  colnames(labels_class)[] <- c("label", "label_codes")
  # Step 2 Upload the existing workbook using openxlsx
  new_wb <-  openxlsx2::wb_load(filename_full_dir)

  selected_label_code <- labels_class$label_codes[labels_class$label==selected_label]

  # --- STEP 3: Apply the label ---
  # This injects the proprietary XML metadata into the new file
  new_wb <-openxlsx2:: wb_add_mips(new_wb, xml = selected_label_code)


  # --- STEP 4: Save the file ---
  openxlsx2::wb_save(new_wb, paste0(filename_full_dir),  overwrite = TRUE)


}
