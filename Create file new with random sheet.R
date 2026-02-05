# =============================================================================
# Create Testfile with random data for testing Data Management.R
# =============================================================================
# Copies the NULL template Excel file and fills the Data sheet with random
# values appropriate for each column's data type. Overwrites existing Testfile.

library(openxlsx)
library(readxl)  # for reading (preserves column names with spaces)

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

# Read template (use readxl to preserve column names with spaces)
lookup_sheet <- as.data.frame(read_excel(template_file, sheet = "VariableView"))
data_template <- as.data.frame(read_excel(template_file, sheet = "Data"))

col_names <- colnames(data_template)
num_cols  <- length(col_names)

# Build lookup: variable name -> data type and value codes
# Use exact column names from VariableView (may have spaces)
var_name_col <- colnames(lookup_sheet)[grepl("Variable", colnames(lookup_sheet))][1]
type_col <- colnames(lookup_sheet)[grepl("Data.*Type|^Type$", colnames(lookup_sheet))][1]
codes_col <- colnames(lookup_sheet)[grepl("Value", colnames(lookup_sheet))][1]

var_types <- setNames(lookup_sheet[[type_col]], lookup_sheet[[var_name_col]])
var_codes <- setNames(lookup_sheet[[codes_col]], lookup_sheet[[var_name_col]])

# Settings
set.seed(2026)
num_rows <- 100

# Generate random data for each column based on its type
data_sheet <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
colnames(data_sheet) <- col_names

for (i in seq_along(col_names)) {
  var_name  <- col_names[i]
  data_type <- var_types[var_name]
  codes     <- var_codes[var_name]

  if (length(data_type) == 0 || is.na(data_type)) {
    # Unknown variable (not in VariableView), default to 1/2
    data_sheet[[i]] <- sample(1:2, num_rows, replace = TRUE)

  } else if (data_type == "Date") {
    # Random dates in 2025
    random_days <- sample(0:364, num_rows, replace = TRUE)
    data_sheet[[i]] <- as.Date("2025-01-01") + random_days

  } else if (data_type == "String") {
    # Placeholder text or empty
    if (grepl("^VAR19", var_name)) {
      # Medication names - mostly empty, some with text
      data_sheet[[i]] <- ifelse(runif(num_rows) < 0.2,
                                 sample(c("Paracetamol", "Ibuprofen", "Tramadol", "Morfin"),
                                        num_rows, replace = TRUE), "")
    } else if (var_name == "VAR01") {
      # Email placeholder
      data_sheet[[i]] <- paste0("test", 1:num_rows, "@example.com")
    } else if (var_name == "VAR03") {
      # Kommun/city
      data_sheet[[i]] <- sample(c("Stockholm", "Gothenburg", "Malmo", "Uppsala"),
                                 num_rows, replace = TRUE)
    } else {
      data_sheet[[i]] <- ""
    }

  } else if (data_type == "Numeric") {
    # Numeric variables - check what kind

    if (var_name == "ID") {
      # Sequential IDs
      data_sheet[[i]] <- 1:num_rows

    } else if (var_name == "VAR02") {
      # Personnummer: 10 or 12 digits, birth years 1950-2005
      birth_years <- sample(1950:2005, num_rows, replace = TRUE)
      birth_months <- sprintf("%02d", sample(1:12, num_rows, replace = TRUE))
      birth_days <- sprintf("%02d", sample(1:28, num_rows, replace = TRUE))
      # Some with full personnummer (12 digits), some with just birthdate (8 digits)
      has_full <- runif(num_rows) < 0.7
      suffix <- ifelse(has_full, sprintf("%04d", sample(1000:9999, num_rows, replace = TRUE)), "")
      data_sheet[[i]] <- as.numeric(paste0(birth_years, birth_months, birth_days, suffix))

    } else if (var_name == "VAR06") {
      # Length in cm (150-200)
      data_sheet[[i]] <- round(rnorm(num_rows, mean = 170, sd = 10))

    } else if (var_name == "VAR07") {
      # Weight in kg (50-120)
      data_sheet[[i]] <- round(rnorm(num_rows, mean = 75, sd = 15))

    } else if (grepl("^VAR10_", var_name)) {
      # Employment matrix: 1-4 (25/50/75/100%)
      data_sheet[[i]] <- sample(1:4, num_rows, replace = TRUE)

    } else if (grepl("^VAR21_", var_name)) {
      # PSQ: 0-10 scale
      data_sheet[[i]] <- sample(0:10, num_rows, replace = TRUE)

    } else if (grepl("^VAR22_", var_name)) {
      # CSI part A: 1-5 scale (Aldrig-Alltid)
      data_sheet[[i]] <- sample(1:5, num_rows, replace = TRUE)

    } else if (grepl("^VAR23_", var_name) || grepl("^VAR24_", var_name)) {
      # CSI part B diagnoses / Physician diagnoses: 1=No/Yes, 2=Yes/No
      data_sheet[[i]] <- sample(1:2, num_rows, replace = TRUE)

    } else if (grepl("^VAR39_", var_name) || grepl("^VAR40_", var_name)) {
      # SCI-93 and ISI matrix items: 1-5 scale
      data_sheet[[i]] <- sample(1:5, num_rows, replace = TRUE)

    } else if (!is.na(codes) && codes == "1 = Ja\r\n2 = Nej") {
      # Standard Ja/Nej dummy: 1 or 2
      data_sheet[[i]] <- sample(1:2, num_rows, replace = TRUE)

    } else {
      # Default numeric: 1 or 2 (most dummies)
      data_sheet[[i]] <- sample(1:2, num_rows, replace = TRUE)
    }
  }
}

# Add Svarstillfalle column if not present (survey response date)
if ("Svarstillfälle" %in% col_names) {
  idx <- which(col_names == "Svarstillfälle")
  random_days <- sample(0:364, num_rows, replace = TRUE)
  data_sheet[[idx]] <- as.Date("2025-01-01") + random_days
}

# Write to new Excel file
wb <- createWorkbook()
addWorksheet(wb, "VariableView")
addWorksheet(wb, "Data")
writeData(wb, "VariableView", lookup_sheet)
writeData(wb, "Data", data_sheet)
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Created", output_file, "with", num_rows, "rows\n")
