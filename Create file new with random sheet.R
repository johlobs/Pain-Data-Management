# =============================================================================
# Create Testfile with random data for testing Data Management.R
# =============================================================================
# Copies the NULL template Excel file and fills the Data sheet with random
# values appropriate for each column's data type. Overwrites existing Testfile.
#
# For EXCLUSIVE groups (only one option can be selected), exactly one dummy
# column gets 1 (Ja) and the rest get 2 (Nej) per row.

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

# =============================================================================
# EXCLUSIVE GROUPS - only one option can be selected per row
# =============================================================================
# These match the classification in Data Management.R
# To adjust, edit this list (must match Data Management.R)

exclusive_groups <- c(
  "VAR04", "VAR05", "VAR08", "VAR09", "VAR11",
  "VAR13", "VAR14", "VAR15", "VAR16", "VAR17", "VAR18",
  paste0("VAR", 25:38),   # HAD items
  paste0("VAR", 41:44),   # ISI items
  paste0("VAR", 45:48),   # Physical activity
  paste0("VAR", 49:52),   # Smoking/snus
  "VAR53"
)

# NON-EXCLUSIVE GROUPS - multiple options can be selected
non_exclusive_groups <- c("VAR12", "VAR20")

# MATRIX GROUPS - independent items with their own scales
matrix_groups <- c("VAR10", "VAR19", "VAR21", "VAR22", "VAR23", "VAR24", "VAR39", "VAR40")

# =============================================================================

# Function to parse Value Codes and extract valid numeric values
parse_value_codes <- function(codes) {
  if (is.na(codes) || codes == "none" || codes == "") return(NULL)
  matches <- gregexpr("(\\d+)\\s*=", codes)
  nums <- regmatches(codes, matches)[[1]]
  nums <- as.integer(gsub("\\s*=", "", nums))
  if (length(nums) > 0) return(nums)
  return(NULL)
}

# Find all columns belonging to each exclusive group
get_group_columns <- function(base_name, col_names) {
  pattern <- paste0("^", base_name, "_\\d+$")
  col_names[grepl(pattern, col_names)]
}

# Settings
set.seed(2026)
num_rows <- 1000

# Generate random data for each column
data_sheet <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
colnames(data_sheet) <- col_names

# Track which columns have been filled (for exclusive groups)
filled_cols <- rep(FALSE, num_cols)
names(filled_cols) <- col_names

# =============================================================================
# STEP 1: Fill exclusive groups (one Ja per row, rest Nej)
# =============================================================================
for (grp in exclusive_groups) {
  grp_cols <- get_group_columns(grp, col_names)
  if (length(grp_cols) == 0) next

  n_options <- length(grp_cols)
  col_indices <- match(grp_cols, col_names)

  # For each row, randomly select one column to be 1 (Ja), rest are 2 (Nej)
  for (row in 1:num_rows) {
    selected <- sample(1:n_options, 1)
    for (j in seq_along(col_indices)) {
      data_sheet[row, col_indices[j]] <- ifelse(j == selected, 1, 2)
    }
  }

  filled_cols[grp_cols] <- TRUE
}

# =============================================================================
# STEP 2: Fill non-exclusive groups (each column independently 1 or 2)
# =============================================================================
for (grp in non_exclusive_groups) {
  grp_cols <- get_group_columns(grp, col_names)
  if (length(grp_cols) == 0) next

  for (col in grp_cols) {
    idx <- match(col, col_names)
    data_sheet[[idx]] <- sample(1:2, num_rows, replace = TRUE)
    filled_cols[col] <- TRUE
  }
}

# =============================================================================
# STEP 3: Fill remaining columns based on type
# =============================================================================
for (i in seq_along(col_names)) {
  if (filled_cols[i]) next  # Already filled

  var_name  <- col_names[i]
  codes     <- var_codes[var_name]
  data_type <- var_types[var_name]
  valid_values <- parse_value_codes(codes)

  if (var_name == "ID") {
    data_sheet[[i]] <- 1:num_rows

  } else if (var_name == "Svarstillfälle" || (!is.na(data_type) && data_type == "Date")) {
    random_days <- sample(0:364, num_rows, replace = TRUE)
    data_sheet[[i]] <- as.Date("2025-01-01") + random_days

  } else if (!is.na(data_type) && data_type == "String") {
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
    birth_years <- sample(1950:2005, num_rows, replace = TRUE)
    birth_months <- sprintf("%02d", sample(1:12, num_rows, replace = TRUE))
    birth_days <- sprintf("%02d", sample(1:28, num_rows, replace = TRUE))
    has_full <- runif(num_rows) < 0.7
    suffix <- ifelse(has_full, sprintf("%04d", sample(1000:9999, num_rows, replace = TRUE)), "")
    data_sheet[[i]] <- as.numeric(paste0(birth_years, birth_months, birth_days, suffix))

  } else if (var_name == "VAR06") {
    data_sheet[[i]] <- round(rnorm(num_rows, mean = 170, sd = 10))

  } else if (var_name == "VAR07") {
    data_sheet[[i]] <- round(rnorm(num_rows, mean = 75, sd = 15))

  } else if (grepl("^VAR10_", var_name)) {
    # Employment matrix: 1-4 (25/50/75/100%)
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

  } else if (!is.null(valid_values)) {
    data_sheet[[i]] <- sample(valid_values, num_rows, replace = TRUE)

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
cat("Exclusive groups:", length(exclusive_groups), "- one selection per row\n")
cat("Non-exclusive groups:", length(non_exclusive_groups), "- independent selections\n")
