# =============================================================================
# Create Testfile with random data for testing Data Management.R
# =============================================================================
# Copies the NULL template Excel file and fills the Data sheet with random
# values appropriate for each column's data type. Overwrites existing Testfile.

library(openxlsx)
library(readxl)

path <- "C:/Users/nepet/Documents/Data Management Sahlgrenska Academy"
template_file <- file.path(path, "NULLdata_MenWomen_Baseline_Facebook_250401.xlsx")
output_file   <- file.path(path, "Testfile.xlsx")

# Auto-delete existing Testfile if present
if (file.exists(output_file)) {
  result <- tryCatch({
    file.remove(output_file)
    cat("Removed existing Testfile.xlsx\n")
    TRUE
  }, warning = function(w) {
    cat("Warning: Could not remove Testfile.xlsx - file may be open in Excel.\n")
    cat("Close Excel and re-run this script.\n")
    FALSE
  })
  if (!result) stop("Cannot overwrite Testfile.xlsx - close Excel first")
}

# Read template
lookup_sheet <- as.data.frame(read_excel(template_file, sheet = "VariableView"))
data_template <- as.data.frame(read_excel(template_file, sheet = "Data"))

col_names <- colnames(data_template)
num_cols  <- length(col_names)

# Build lookup from VariableView: variable name -> value codes
var_names_vv <- lookup_sheet[["Variable Name"]]
value_codes  <- lookup_sheet[["Value Codes"]]
data_types   <- lookup_sheet[["Data Type"]]

# Create named vectors for lookup
var_codes <- setNames(value_codes, var_names_vv)
var_types <- setNames(data_types, var_names_vv)

# Function to parse Value Codes and extract valid numeric values
parse_value_codes <- function(codes) {
  if (is.na(codes) || codes == "none" || codes == "") return(NULL)
  # Extract numbers before " = " (e.g., "1 = Ja\r\n2 = Nej" -> c(1, 2))
  matches <- gregexpr("(\\d+)\\s*=", codes)
  nums <- regmatches(codes, matches)[[1]]
  nums <- as.integer(gsub("\\s*=", "", nums))
  if (length(nums) > 0) return(nums)
  return(NULL)
}

# Settings
set.seed(2026)
num_rows <- 100

# Generate random data for each column
data_sheet <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
colnames(data_sheet) <- col_names

for (i in seq_along(col_names)) {
  var_name  <- col_names[i]
  codes     <- var_codes[var_name]
  data_type <- var_types[var_name]

  # Parse valid values from Value Codes
  valid_values <- parse_value_codes(codes)

  # Special cases first
  if (var_name == "ID") {
    data_sheet[[i]] <- 1:num_rows

  } else if (var_name == "Svarstillfälle" || (!is.na(data_type) && data_type == "Date")) {
    # Date column
    random_days <- sample(0:364, num_rows, replace = TRUE)
    data_sheet[[i]] <- as.Date("2025-01-01") + random_days

  } else if (!is.na(data_type) && data_type == "String") {
    # String columns
    if (grepl("^VAR19", var_name)) {
      data_sheet[[i]] <- ifelse(runif(num_rows) < 0.2,
                                sample(c("Paracetamol", "Ibuprofen", "Tramadol", "Morfin"),
                                       num_rows, replace = TRUE), "")
    } else if (var_name == "VAR01") {
      data_sheet[[i]] <- paste0("test", 1:num_rows, "@example.com")
    } else if (var_name == "VAR03") {
      data_sheet[[i]] <- sample(c("Stockholm", "Gothenburg", "Malmo", "Uppsala"),
                                num_rows, replace = TRUE)
    } else {
      data_sheet[[i]] <- ""
    }

  } else if (var_name == "VAR02") {
    # Personnummer
    birth_years <- sample(1950:2005, num_rows, replace = TRUE)
    birth_months <- sprintf("%02d", sample(1:12, num_rows, replace = TRUE))
    birth_days <- sprintf("%02d", sample(1:28, num_rows, replace = TRUE))
    has_full <- runif(num_rows) < 0.7
    suffix <- ifelse(has_full, sprintf("%04d", sample(1000:9999, num_rows, replace = TRUE)), "")
    data_sheet[[i]] <- as.numeric(paste0(birth_years, birth_months, birth_days, suffix))

  } else if (var_name == "VAR06") {
    # Length in cm
    data_sheet[[i]] <- round(rnorm(num_rows, mean = 170, sd = 10))

  } else if (var_name == "VAR07") {
    # Weight in kg
    data_sheet[[i]] <- round(rnorm(num_rows, mean = 75, sd = 15))

  } else if (!is.null(valid_values)) {
    # Use parsed values from Value Codes
    data_sheet[[i]] <- sample(valid_values, num_rows, replace = TRUE)

  } else if (grepl("^VAR10_", var_name)) {
    # Employment matrix: 1-4
    data_sheet[[i]] <- sample(1:4, num_rows, replace = TRUE)

  } else if (grepl("^VAR21_", var_name)) {
    # PSQ: 0-10 scale
    data_sheet[[i]] <- sample(0:10, num_rows, replace = TRUE)

  } else if (grepl("^VAR22_", var_name)) {
    # CSI part A: 1-5 scale
    data_sheet[[i]] <- sample(1:5, num_rows, replace = TRUE)

  } else if (grepl("^VAR23_|^VAR24_", var_name)) {
    # Diagnoses: 1 or 2
    data_sheet[[i]] <- sample(1:2, num_rows, replace = TRUE)

  } else if (grepl("^VAR39_|^VAR40_", var_name)) {
    # SCI-93 and ISI: 1-5 scale
    data_sheet[[i]] <- sample(1:5, num_rows, replace = TRUE)

  } else {
    # Default: 1 or 2
    data_sheet[[i]] <- sample(1:2, num_rows, replace = TRUE)
  }
}

# Write to new Excel file
wb <- createWorkbook()
addWorksheet(wb, "VariableView")
addWorksheet(wb, "Data")
writeData(wb, "VariableView", lookup_sheet)
writeData(wb, "Data", data_sheet)
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Created", output_file, "with", num_rows, "rows of random data\n")
cat("Columns filled:", num_cols, "\n")
