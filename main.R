# =============================================================================
# PainSense - Main script
#
# Runs the whole pipeline in order:
#   1. Data cleaning        (docs/Data Cleaning.qmd)
#   2. Data transformation  (PainSense - Data Transformation.qmd)
#   3. Export               (docs/PainSense_processed_data.csv)
#
# Both .qmd files are run in this same R session, so the cleaned data frame
# `df_clean` from step 1 is picked up directly by step 2 (see Chapter 1 in the
# transformation document). Nothing is written to disk between the steps.
#
# Run with:  source("main.R")   or   Rscript main.R
# =============================================================================

library(knitr)
library(readr)


# -----------------------------------------------------------------------------
# File paths
# -----------------------------------------------------------------------------

project_dir <- "C:/Users/nepet/Documents/Data Management Sahlgrenska Academy"

cleaning_qmd       <- file.path(project_dir, "docs", "Data Cleaning.qmd")
transformation_qmd <- file.path(project_dir, "PainSense - Data Transformation.qmd")
output_csv         <- file.path(project_dir, "docs", "PainSense_processed_data.csv")


# -----------------------------------------------------------------------------
# Step 1: Data cleaning
# -----------------------------------------------------------------------------
# knitr::purl() pulls the R code out of the .qmd file into a plain .R file,
# which is then run with source(). The text between the code chunks is skipped.

cat("\n=== Step 1: Data cleaning ===\n")

cleaning_r <- tempfile(fileext = ".R")
purl(cleaning_qmd, output = cleaning_r, quiet = TRUE)
source(cleaning_r, encoding = "UTF-8")

cat(sprintf("Data cleaning done: df_clean has %d rows and %d columns.\n",
            nrow(df_clean), ncol(df_clean)))


# -----------------------------------------------------------------------------
# Step 2: Data transformation
# -----------------------------------------------------------------------------
# Uses df_clean from step 1 and ends with the data frame `final`.

cat("\n=== Step 2: Data transformation ===\n")

transformation_r <- tempfile(fileext = ".R")
purl(transformation_qmd, output = transformation_r, quiet = TRUE)
source(transformation_r, encoding = "UTF-8")

cat(sprintf("Data transformation done: final has %d rows and %d columns.\n",
            nrow(final), ncol(final)))


# -----------------------------------------------------------------------------
# Step 3: Export to a CSV file that opens like a spreadsheet in Excel
# -----------------------------------------------------------------------------
# write_excel_csv2() writes the format Swedish Excel expects:
#   - semicolon (;) between columns, so Excel splits the columns by itself
#   - comma (,) as decimal sign, so 23,5 is read as a number
#   - UTF-8 with a byte order mark (BOM), so å, ä and ö show correctly
# Missing values are written as empty cells instead of the text "NA".

cat("\n=== Step 3: Export ===\n")

write_excel_csv2(final, output_csv, na = "")

cat(sprintf("Exported %d rows and %d variables to %s\n",
            nrow(final), ncol(final), output_csv))
