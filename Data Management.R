
# =============================================================================
# PainSensPROM Baseline — Data Management
# =============================================================================
#
# PURPOSE
# -------
# This script loads survey data exported from Sunet Survey (Excel format) and
# restructures dummy-coded variables into clean numeric columns.
#
# The raw data encodes many single-choice questions as sets of dummy columns:
#   e.g. VAR04_1 (Man) and VAR04_2 (Kvinna), each coded 1 = Ja, 2 = Nej.
# This script combines those into a single column (e.g. VAR04 = 1 or 2) and
# builds a codebook so each numeric level can be traced back to its label.
#
# THREE TYPES OF VARIABLE GROUPS
# ------------------------------
# Variables with the pattern VARnn_nn are grouped by their base name (VARnn).
# Each group falls into one of three categories:
#
#   1. EXCLUSIVE  — Only one option can be selected (radio-button logic).
#      The dummy columns are collapsed into a single numeric variable.
#      Example: VAR04_1=Ja, VAR04_2=Nej  =>  VAR04 = 1  (Man)
#
#   2. NON-EXCLUSIVE — Multiple options can be selected (checkbox logic).
#      The dummy columns are kept as separate binary (0/1) columns.
#      Example: VAR12_1..VAR12_45 for pain locations (multiple body sites).
#
#   3. MATRIX — Each sub-item is an independent question with its own scale.
#      These are not dummies at all; they are left unchanged.
#      Example: VAR21_1..VAR21_17 (PSQ items, each rated 0–10).
#
# CLASSIFICATION SOURCE
# ---------------------
# The classification of each group is based on the questionnaire design
# documented in Enkät_T1a_Gen_230102.pdf and verified against the
# PainSensPROM_Baseline_DataManagement docx.
#
# HOW TO USE
# ----------
#   1. Point 'f' to the Excel file containing your data.
#   2. Run the script. Check the verification output for warnings.
#   3. Inspect codebook_exclusive and codebook_non_exclusive for value mappings.
#   4. If a group is misclassified, move it between the exclusive,
#      non_exclusive, and matrix_groups vectors below.
#
# =============================================================================

library(dplyr)
library(readxl)
library(labelled)
library(tidyr)
library(stringr)

# == Load data ================================================================

path <- "C:/Users/nepet/Documents/Data Management Sahlgrenska Academy"
f    <- file.path(path, "NULLdata_MenWomen_Baseline_Facebook_250401.xlsx")

# VariableView sheet: codebook with variable names, descriptive labels,
# question types (Check Boxes / Matrix / etc.), value codes, and missing codes.
vv <- read_excel(f, sheet = "VariableView")

# Data sheet: the actual responses. The NULL template has 0 rows;
# replace the file path with the real dataset when available.
d1 <- read_excel(f, sheet = "Data")

# == Build look-up table from VariableView ====================================
#
# Extracts one row per variable with all metadata needed for classification,
# codebook generation, and labelling.

label_table <- tibble(
  variable  = vv[["Variable Name"]],
  label     = vv[["Label"]],
  type      = vv[["Type"]],       # e.g. "Multiple-Choice - Check Boxes"
  data_type = vv[["Data Type"]],   # e.g. "Numeric", "String", "Date"
  values    = vv[["Value Codes"]], # e.g. "1 = Ja\r\n2 = Nej"
  missing   = vv[["Missing Code"]]
)

# == Correct Type column: "Multiple-Choice" -> "One-Choice" for exclusive ======
#
# The Sunet Survey export marks all checkbox-style questions as "Multiple-Choice"
# even when the questionnaire only allows one selection. This section creates a
# corrected Type column based on researcher verification of the questionnaire.
#
# To adjust classifications, edit the exclusive_groups vector below.

# Groups confirmed as EXCLUSIVE (only one option can be selected):
# - VAR04: Q1a juridiskt kön (Man/Kvinna)
# - VAR05: Q1b könsidentitet (Man/Kvinna/Annat)
# - VAR08: Q4 utbildning
# - VAR09: Q5 födelseland
# - VAR11: Q7a smärta senaste 7 dagar (Ja/Nej)
# - VAR13: Q7c NRS medel 7d (0-10 scale)
# - VAR14: Q8 NRS vila 7d (0-10 scale)
# - VAR15: Q9 NRS rörelse 7d (0-10 scale)
# - VAR16: Q10 smärtfrekvens
# - VAR17: Q11 smärtduration
# - VAR18: Q12 periodisk/ihållande
# - VAR25-VAR38: HAD items (4 Likert options each)
# - VAR41-VAR44: ISI items (5 options each)
# - VAR45-VAR48: Physical activity items
# - VAR49-VAR52: Smoking/snus items
# - VAR53: Q33 kontakt framtida studier

exclusive_groups <- c(
  "VAR04", "VAR05", "VAR08", "VAR09", "VAR11",
  "VAR13", "VAR14", "VAR15", "VAR16", "VAR17", "VAR18",
  paste0("VAR", 25:38),   # HAD items
  paste0("VAR", 41:44),   # ISI items
  paste0("VAR", 45:48),   # Physical activity
  paste0("VAR", 49:52),   # Smoking/snus
  "VAR53"
)

# Create Type_corrected: change "Multiple-Choice - Check Boxes" to
# "One-Choice - Check Boxes" for variables in exclusive groups
label_table <- label_table %>%
  mutate(
    base = sub("_.*", "", variable),
    type_corrected = ifelse(
      base %in% exclusive_groups & grepl("Multiple-Choice", type),
      gsub("Multiple-Choice", "One-Choice", type),
      type
    )
  ) %>%
  select(-base)

# == Identify dummy groups (VARnn_nn columns) =================================
#
# Columns named VARnn (no underscore) are standalone variables.
# Columns named VARnn_mm belong to a group identified by the base name VARnn.
# This step finds all groups and attaches their base name.

dummy_cols <- label_table %>%
  filter(grepl("^VAR[0-9]+_", variable))

dummy_cols <- dummy_cols %>%
  mutate(base = sub("_.*", "", variable))

# == Classify groups: mutually exclusive vs non-exclusive =====================
#
# Manual classification based on PainSensPROM_Baseline_DataManagement docx
# and verified against Enkät_T1a_Gen_230102.pdf.
#
# To reclassify a group, move it between the three vectors below.

# EXCLUSIVE: only one option can be selected per respondent.
# These will be collapsed into a single numeric column.
exclusive <- c(
  "VAR04",          # Q1a juridiskt kön (sex): Man / Kvinna
  "VAR05",          # Q1b könsidentitet (sex_id): Man / Kvinna / Annat
  "VAR08",          # Q4 utbildning (edu): "Ange endast ett alternativ"
  "VAR09",          # Q5 födelseland (origin): "Ange endast ett alternativ"
  "VAR11",          # Q7a smärta senaste 7 dagar (pain7days): Ja / Nej
  "VAR13",          # Q7c NRS medel 7d (nrs_mean7d): scale 0-10, pick one
  "VAR14",          # Q8 NRS vila 7d (nrs_rest7d): scale 0-10, pick one
  "VAR15",          # Q9 NRS rörelse 7d (nrs_move7d): scale 0-10, pick one
  "VAR16",          # Q10 smärtfrekvens (pain_freq): 5 options, pick one
  "VAR17",          # Q11 smärtduration (pain_duration): 5 options, pick one
  "VAR18",          # Q12 periodisk/ihållande (persistent): 2 options
  "VAR25",          # HAD Q19a - oro: 4 Likert options
  "VAR26",          # HAD Q19b - uppskattar saker
  "VAR27",          # HAD Q19c - hemskt
  "VAR28",          # HAD Q19d - skratta
  "VAR29",          # HAD Q19e - bekymrar
  "VAR30",          # HAD Q19f - gott humör
  "VAR31",          # HAD Q19g - avslappnad
  "VAR32",          # HAD Q19h - trögt
  "VAR33",          # HAD Q19i - fjärilar
  "VAR34",          # HAD Q19j - utseende
  "VAR35",          # HAD Q19k - rastlös
  "VAR36",          # HAD Q19l - glädje
  "VAR37",          # HAD Q19m - panik
  "VAR38",          # HAD Q19n - bok/TV/radio
  "VAR41",          # ISI Q23 nöjd sömnmönster: 5 options
  "VAR42",          # ISI Q24 funktionsförmåga: 5 options
  "VAR43",          # ISI Q25 märkbart livskvalitet: 5 options
  "VAR44",          # ISI Q26 bekymrad sömn: 5 options
  "VAR45",          # Q27 tid fysisk träning/vecka (PE_time): 6 options
  "VAR46",          # Q28 dagar fysisk träning (PE_days): 0-7
  "VAR47",          # Q29 tid vardagsmotion/vecka (PA_time): 7 options
  "VAR48",          # Q30 dagar vardagsmotion (PA_days): 0-7
  "VAR49",          # Q31a rökning (smoke): 4 options
  "VAR50",          # Q31b antal cigaretter/dag (ant_smoke): 3 options
  "VAR51",          # Q32a snus (snuff): 4 options
  "VAR52",          # Q32b antal dosor/vecka (ant_snuff): 3 options
  "VAR53"           # Q33 kontakt framtida studier: Ja / Nej
)

# NON-EXCLUSIVE: multiple options can be selected simultaneously.
# These stay as separate binary columns, recoded from 1=Ja/2=Nej to 1/0.
non_exclusive <- c(
  "VAR12",          # Q7b smärtlokalisationer (45 body regions, mark all)
  "VAR20"           # Q14 behandlingsåtgärder (multiple treatments possible)
)

# MATRIX: each sub-item is an independent variable with its own response scale.
# These are NOT dummies — they are left completely unchanged.
matrix_groups <- c(
  "VAR10",          # Q6 arbetslivssituation: each sub-item rated 25/50/75/100%
  "VAR19",          # Q13 smärtläkemedel: free text per medication
  "VAR21",          # Q15 PSQ smärtkänslighet: each scenario rated 0-10
  "VAR22",          # Q16 CSI del A: each item rated 1-5 (Aldrig–Alltid)
  "VAR23",          # Q17 CSI del B diagnoser: each diagnosis Nej(1)/Ja(2)
  "VAR24",          # Q18 sjukdomar/skador: each condition Ja(1)/Nej(2)
  "VAR39",          # Q20-21 SCI-93: each symptom rated 1-5
  "VAR40"           # Q22 sömnproblem ISI: each sleep item rated 1-5
)

# Summarise each group: count sub-items, record type, extract question stem.
group_info <- dummy_cols %>%
  group_by(base) %>%
  summarise(
    n_items    = n(),
    first_type = first(type),
    # Question stem = label text before the " - option" suffix
    question   = sub(" - .*", "", first(label)) %>%
                 str_replace_all("\\r\\n", " ") %>%
                 str_trim(),
    .groups = "drop"
  ) %>%
  mutate(class = case_when(
    base %in% exclusive     ~ "exclusive",
    base %in% non_exclusive ~ "non_exclusive",
    base %in% matrix_groups ~ "matrix",
    TRUE                    ~ "UNCLASSIFIED"  # triggers warning below
  ))

# == Verify classifications ===================================================
#
# Cross-checks the manual classification against VariableView metadata to
# catch potential mistakes. Produces warnings (not errors) so the script
# still runs, but issues are visible in the console.
#
# Checks performed:
#   1. Any VARnn group not listed in any of the three vectors.
#   2. Exclusive group whose VariableView Type is "Matrix" (contradicts
#      combining, since matrix sub-items are independent questions).
#   3. Exclusive group whose label contains "Flera alternativ" (the
#      questionnaire explicitly allows multiple selections).
#   4. Matrix group whose VariableView Type is "Check Boxes" (might
#      actually be dummies that should be exclusive or non-exclusive).
#   5. Exclusive group with >10 sub-items (unusual for single-select;
#      legitimate for NRS 0-10 scales, but worth a second look).

verify_classification <- function(group_info, dummy_cols) {
  warnings <- character(0)

  uncl <- group_info %>% filter(class == "UNCLASSIFIED")
  if (nrow(uncl) > 0) {
    warnings <- c(warnings, paste0(
      "UNCLASSIFIED groups (add to exclusive/non_exclusive/matrix_groups): ",
      paste(uncl$base, collapse = ", ")))
  }

  matrix_as_excl <- group_info %>%
    filter(class == "exclusive", grepl("Matrix", first_type, ignore.case = TRUE))
  if (nrow(matrix_as_excl) > 0) {
    warnings <- c(warnings, paste0(
      "Classified as exclusive but Type is Matrix (review!): ",
      paste(matrix_as_excl$base, collapse = ", ")))
  }

  excl_multi <- group_info %>%
    filter(class == "exclusive", grepl("[Ff]lera alternativ", question))
  if (nrow(excl_multi) > 0) {
    warnings <- c(warnings, paste0(
      "Classified as exclusive but label says 'Flera alternativ' (review!): ",
      paste(excl_multi$base, collapse = ", ")))
  }

  cb_as_matrix <- group_info %>%
    filter(class == "matrix", grepl("Check Boxes", first_type, ignore.case = TRUE))
  if (nrow(cb_as_matrix) > 0) {
    warnings <- c(warnings, paste0(
      "Classified as matrix but Type is Check Boxes (review!): ",
      paste(cb_as_matrix$base, collapse = ", ")))
  }

  many_excl <- group_info %>%
    filter(class == "exclusive", n_items > 10)
  if (nrow(many_excl) > 0) {
    warnings <- c(warnings, paste0(
      "Classified as exclusive with >10 sub-items (verify single-select): ",
      paste(many_excl$base, " (", many_excl$n_items, ")", sep = "", collapse = ", ")))
  }

  if (length(warnings) == 0) {
    cat("Verification passed: no issues found.\n")
  } else {
    cat("Verification warnings:\n")
    for (w in warnings) {
      cat("  ! ", w, "\n")
    }
  }

  invisible(warnings)
}

verification_warnings <- verify_classification(group_info, dummy_cols)

cat("\nGroup classification:\n")
print(as.data.frame(group_info[, c("base", "n_items", "class")]), row.names = FALSE)

# == Build codebook for exclusive groups ======================================
#
# Creates a lookup table mapping each numeric level to its descriptive label.
# After combining, VAR04 = 1 means "Man", VAR04 = 2 means "Kvinna", etc.
#
# The sub_index is the number after the underscore in the original column name
# (e.g. VAR04_1 -> sub_index 1, VAR04_2 -> sub_index 2).
#
# The option_label is extracted from the VariableView label: the text after
# the last " - " separator (e.g. "...juridiskt kön? - Man" -> "Man").

build_codebook_exclusive <- function(dummy_cols, group_info) {
  exclusive_bases <- group_info$base[group_info$class == "exclusive"]

  codebook <- dummy_cols %>%
    filter(base %in% exclusive_bases) %>%
    mutate(
      sub_index = as.integer(sub("^VAR[0-9]+_", "", variable)),
      option_label = str_replace_all(label, "\\r\\n", " ") %>%
                     sub("^.* - ", "", .)
    ) %>%
    select(base, sub_index, option_label, variable)

  return(codebook)
}

codebook_exclusive <- build_codebook_exclusive(dummy_cols, group_info)

# == Build codebook for non-exclusive groups ==================================
#
# These columns stay in the dataset as separate binary variables, but the
# codebook documents what each column represents (e.g. VAR12_1 = body region 1).

build_codebook_non_exclusive <- function(dummy_cols, group_info) {
  ne_bases <- group_info$base[group_info$class == "non_exclusive"]

  codebook <- dummy_cols %>%
    filter(base %in% ne_bases) %>%
    mutate(
      option_label = str_replace_all(label, "\\r\\n", " ") %>%
                     sub("^.* - ", "", .)
    ) %>%
    select(base, variable, option_label)

  return(codebook)
}

codebook_non_exclusive <- build_codebook_non_exclusive(dummy_cols, group_info)

# == Combine mutually exclusive dummies into single columns ===================
#
# For each exclusive group (e.g. VAR04_1, VAR04_2):
#   - Scans each row to find which sub-column has value 1 (= Ja).
#   - Writes the sub-index of that column into the new combined column.
#   - If no column has Ja (all Nej or all NA), the result is NA.
#   - If multiple columns have Ja (data error), the result is -1 so
#     conflicts are easy to find and investigate.
#   - Removes the original dummy columns after combining.

combine_exclusive <- function(d1, codebook_exclusive, group_info) {
  exclusive_bases <- group_info$base[group_info$class == "exclusive"]

  for (b in exclusive_bases) {
    grp <- codebook_exclusive %>% filter(base == b)
    member_cols <- grp$variable

    present <- member_cols[member_cols %in% names(d1)]
    if (length(present) == 0) next

    sub_indices <- grp$sub_index[match(present, grp$variable)]

    d1[[b]] <- apply(d1[, present, drop = FALSE], 1, function(row) {
      selected <- which(row == 1)
      if (length(selected) == 1) {
        return(sub_indices[selected])
      } else if (length(selected) > 1) {
        return(-1)  # conflict flag
      } else {
        return(NA_real_)
      }
    })

    d1 <- d1 %>% select(-all_of(present))
  }

  return(d1)
}

# == Recode non-exclusive dummies: 1=Ja -> 1, 2=Nej -> 0 =====================
#
# The raw data uses 1 = Ja, 2 = Nej for checkbox items. Standard binary
# coding is 1 = yes, 0 = no. This function recodes accordingly.
# Any other value (e.g. NA or 999) is left unchanged.

recode_non_exclusive <- function(d1, codebook_non_exclusive) {
  for (col in codebook_non_exclusive$variable) {
    if (col %in% names(d1)) {
      d1[[col]] <- ifelse(d1[[col]] == 1, 1,
                   ifelse(d1[[col]] == 2, 0, d1[[col]]))
    }
  }
  return(d1)
}

# == Apply labels to remaining columns ========================================
#
# Uses the labelled package to attach descriptive labels to each column.
# - Standalone and matrix columns get the full label from VariableView.
# - Combined exclusive columns get the question stem as their label.

apply_labels <- function(d1, label_table, codebook_exclusive, group_info) {
  lookup <- setNames(
    str_replace_all(label_table$label, "\\r\\n", " "),
    label_table$variable
  )
  matched <- intersect(names(d1), names(lookup))
  var_label(d1[matched]) <- lookup[matched]

  exclusive_bases <- group_info$base[group_info$class == "exclusive"]
  for (b in exclusive_bases) {
    if (b %in% names(d1)) {
      q <- group_info$question[group_info$base == b]
      var_label(d1[[b]]) <- q
    }
  }

  return(d1)
}

# == Data-level verification: check exclusive groups in actual data ===========
#
# After combining, any row where multiple sub-columns had Ja (= 1) in an
# exclusive group will have value -1. This function counts those conflicts
# per variable and prints a summary.
#
# With 0 data rows (NULL template), this check is skipped.

verify_exclusive_data <- function(d1, group_info) {
  exclusive_bases <- group_info$base[group_info$class == "exclusive"]
  present <- intersect(exclusive_bases, names(d1))

  if (length(present) == 0 || nrow(d1) == 0) {
    cat("Data verification skipped (no data rows or no exclusive columns).\n")
    return(invisible(NULL))
  }

  conflict_counts <- d1 %>%
    select(all_of(present)) %>%
    summarise(across(everything(), ~ sum(. == -1, na.rm = TRUE)))

  any_conflicts <- any(conflict_counts > 0)
  if (any_conflicts) {
    cat("DATA WARNING: rows with multiple 'Ja' in exclusive groups:\n")
    flagged <- names(conflict_counts)[conflict_counts > 0]
    for (col in flagged) {
      cat(sprintf("  %s: %d conflicting rows\n", col, conflict_counts[[col]]))
    }
    cat("  These rows have value -1. Inspect and resolve manually.\n")
  } else {
    cat("Data verification passed: no conflicts in exclusive groups.\n")
  }

  invisible(conflict_counts)
}

# =============================================================================
# VARIABLE CREATION
# =============================================================================
#
# Derived research variables from PainSensPROM_Baseline_DataManagement.docx.
# These MUST run BEFORE the combining pipeline because the formulas use raw
# dummy columns (1 = Ja, 2 = Nej). The pipeline removes/recodes those columns.
# =============================================================================

# == 1. Data adjustments =====================================================
# Row-level corrections identified during data quality checks.

d1$VAR07[d1$ID == 7]     <- 95   # weight 995 -> 95 kg
d1$VAR15_8[d1$ID == 2005] <- 2   # old_ID 4: value 21 -> 2 (Nej)

# == 2. Demographics =========================================================

# age: from personnummer (VAR02) and survey date (Svarstillfälle).
# BIN_persnr distinguishes full personnummer (12+ digits) from birth date only.
d1$BIN_persnr <- ifelse(d1$VAR02 < 99999999999, 0, 1)
d1$birthyear  <- d1$BIN_persnr * trunc((d1$BIN_persnr * d1$VAR02) / 1e8) +
  (1 - d1$BIN_persnr) * trunc(d1$VAR02 / 1e4)
current_date <- as.POSIXlt(d1[["Svarstillfälle"]])
current_year <- 1900 + current_date$year
d1$age <- current_year - d1$birthyear
d1$BIN_persnr <- NULL
d1$birthyear  <- NULL

# sex: 0 = man, 1 = woman (VAR04)
d1$sex <- (2 - d1$VAR04_1) + 2 * (2 - d1$VAR04_2) - 1
d1$sex[d1$ID == 207] <- 1   # woman (from personnummer)
d1$sex[d1$ID == 281] <- 0   # man   (from personnummer)
d1$sex[d1$ID == 590] <- 1   # woman (from personnummer)

# sex_id: 0 = man, 1 = woman, 2 = other (VAR05)
d1$sex_id <- (2 - d1$VAR05_1) + 2 * (2 - d1$VAR05_2) +
  3 * (2 - d1$VAR05_3) - 1

# length (cm), weight (kg)
d1$length <- d1$VAR06
d1$weight <- d1$VAR07

# BMI and categories
d1$BMI <- d1$weight / (d1$length / 100)^2

# BMI_6cat: 1=underweight(<18.5) 2=normal(18.5-25) 3=overweight(25-30)
#           4=obesity I(30-35) 5=obesity II(35-40) 6=obesity III(40+)
d1$BMI_6cat <- as.numeric(cut(d1$BMI,
  breaks = c(-Inf, 18.5, 25, 30, 35, 40, Inf), labels = 1:6, right = FALSE))

# BMI_4cat: 1=underweight 2=normal 3=overweight 4=obesity(30+)
d1$BMI_4cat <- as.numeric(cut(d1$BMI,
  breaks = c(-Inf, 18.5, 25, 30, Inf), labels = 1:4, right = FALSE))

# origin: 1=Sweden, 2=other Nordic, 3=European non-Nordic, 4=outside Europe
d1$origin <- (2 - d1$VAR09_1) + 2 * (2 - d1$VAR09_2) +
  3 * (2 - d1$VAR09_3) + 4 * (2 - d1$VAR09_4)

# == 3. Employment (VAR10 matrix) =============================================
# Recode from 1-4 scale to percentage (25/50/75/100).

var10_cols <- grep("^VAR10_", names(d1), value = TRUE)
for (col in var10_cols) d1[[col]] <- d1[[col]] * 25

# == 4. Pain variables ========================================================

# pain7days: 1 = yes, 0 = no (VAR11)
d1$pain7days <- 2 - d1$VAR11_1

# nrs_mean7d: mean pain intensity last 7 days, 0-10 (VAR13)
d1$nrs_mean7d <- (2 - d1$VAR13_1) + 2 * (2 - d1$VAR13_2) +
  3 * (2 - d1$VAR13_3) + 4 * (2 - d1$VAR13_4) + 5 * (2 - d1$VAR13_5) +
  6 * (2 - d1$VAR13_6) + 7 * (2 - d1$VAR13_7) + 8 * (2 - d1$VAR13_8) +
  9 * (2 - d1$VAR13_9) + 10 * (2 - d1$VAR13_10) + 11 * (2 - d1$VAR13_11) - 1

# nrs_rest7d: pain at rest last 7 days, 0-10 (VAR14)
d1$nrs_rest7d <- (2 - d1$VAR14_1) + 2 * (2 - d1$VAR14_2) +
  3 * (2 - d1$VAR14_3) + 4 * (2 - d1$VAR14_4) + 5 * (2 - d1$VAR14_5) +
  6 * (2 - d1$VAR14_6) + 7 * (2 - d1$VAR14_7) + 8 * (2 - d1$VAR14_8) +
  9 * (2 - d1$VAR14_9) + 10 * (2 - d1$VAR14_10) + 11 * (2 - d1$VAR14_11) - 1

# nrs_move7d: pain during movement last 7 days, 0-10 (VAR15)
# (VAR15_8 already corrected for ID 2005 in data adjustments)
d1$nrs_move7d <- (2 - d1$VAR15_1) + 2 * (2 - d1$VAR15_2) +
  3 * (2 - d1$VAR15_3) + 4 * (2 - d1$VAR15_4) + 5 * (2 - d1$VAR15_5) +
  6 * (2 - d1$VAR15_6) + 7 * (2 - d1$VAR15_7) + 8 * (2 - d1$VAR15_8) +
  9 * (2 - d1$VAR15_9) + 10 * (2 - d1$VAR15_10) + 11 * (2 - d1$VAR15_11) - 1

# pain_freq: 1=every day .. 5=seldom (VAR16)
d1$pain_freq <- (2 - d1$VAR16_1) + 2 * (2 - d1$VAR16_2) +
  3 * (2 - d1$VAR16_3) + 4 * (2 - d1$VAR16_4) + 5 * (2 - d1$VAR16_5)

# pain_duration: 0=<1mo, 1=<3mo, 2=<12mo, 3=<2yr, 4=>2yr (VAR17)
d1$pain_duration <- (2 - d1$VAR17_1) + 2 * (2 - d1$VAR17_2) +
  3 * (2 - d1$VAR17_3) + 4 * (2 - d1$VAR17_4) + 5 * (2 - d1$VAR17_5) - 1

# chronic_pain: 1 if pain7days==1 AND pain_duration >= 2 (>= 12 months)
d1$chronic_pain <- NA_real_
d1$chronic_pain[d1$pain7days == 0] <- 0
d1$chronic_pain[d1$pain7days == 1 & d1$pain_duration < 2]  <- 0
d1$chronic_pain[d1$pain7days == 1 & d1$pain_duration >= 2] <- 1

# persistent: 0 = periodvis, 1 = ihållande (VAR18)
d1$persistent <- 2 - d1$VAR18_2

# == 5. Pain locations & widespread pain ======================================
# VAR12 uses raw 1=Ja / 2=Nej.  (2*N)-sum(items) counts "yes" responses.

# nr_sites: number of pain sites, 0-45
d1$nr_sites <- ifelse(d1$pain7days == 1,
  (2 * 45) - (d1$VAR12_1 + d1$VAR12_2 + d1$VAR12_3 + d1$VAR12_4 +
    d1$VAR12_5 + d1$VAR12_6 + d1$VAR12_7 + d1$VAR12_8 + d1$VAR12_9 +
    d1$VAR12_10 + d1$VAR12_11 + d1$VAR12_12 + d1$VAR12_13 + d1$VAR12_14 +
    d1$VAR12_15 + d1$VAR12_16 + d1$VAR12_17 + d1$VAR12_18 + d1$VAR12_19 +
    d1$VAR12_20 + d1$VAR12_21 + d1$VAR12_22 + d1$VAR12_23 + d1$VAR12_24 +
    d1$VAR12_25 + d1$VAR12_26 + d1$VAR12_27 + d1$VAR12_28 + d1$VAR12_29 +
    d1$VAR12_30 + d1$VAR12_31 + d1$VAR12_32 + d1$VAR12_33 + d1$VAR12_34 +
    d1$VAR12_35 + d1$VAR12_36 + d1$VAR12_37 + d1$VAR12_38 + d1$VAR12_39 +
    d1$VAR12_40 + d1$VAR12_41 + d1$VAR12_42 + d1$VAR12_43 + d1$VAR12_44 +
    d1$VAR12_45), NA)

# --- ACR90 widespread pain ---
# Quadrant counts: (2*N) - sum gives count of "yes" (1=Ja) in each quadrant

# q1: upper left (8 sites)
d1$q1 <- (2 * 8) - (d1$VAR12_5 + d1$VAR12_7 + d1$VAR12_9 + d1$VAR12_11 +
  d1$VAR12_26 + d1$VAR12_28 + d1$VAR12_30 + d1$VAR12_32)

# q2: upper right (8 sites)
d1$q2 <- (2 * 8) - (d1$VAR12_4 + d1$VAR12_6 + d1$VAR12_8 + d1$VAR12_10 +
  d1$VAR12_27 + d1$VAR12_29 + d1$VAR12_31 + d1$VAR12_33)

# q3: lower left (7 sites)
d1$q3 <- (2 * 7) - (d1$VAR12_18 + d1$VAR12_20 + d1$VAR12_22 +
  d1$VAR12_38 + d1$VAR12_40 + d1$VAR12_42 + d1$VAR12_44)

# q4: lower right (7 sites)
d1$q4 <- (2 * 7) - (d1$VAR12_17 + d1$VAR12_19 + d1$VAR12_21 +
  d1$VAR12_39 + d1$VAR12_41 + d1$VAR12_43 + d1$VAR12_45)

# q5: axial (8 sites)
d1$q5 <- (2 * 8) - (d1$VAR12_3 + d1$VAR12_12 + d1$VAR12_13 + d1$VAR12_25 +
  d1$VAR12_34 + d1$VAR12_35 + d1$VAR12_36 + d1$VAR12_37)

# Contralateral patterns
d1$contralat_1 <- ifelse(d1$q5 > 0 & d1$q4 > 0 & d1$q1 > 0, 1, 0)
d1$contralat_2 <- ifelse(d1$q5 > 0 & d1$q3 > 0 & d1$q2 > 0, 1, 0)

# ACR90: widespread pain if either contralateral pattern is met
d1$ACR90    <- ifelse(d1$contralat_1 == 1 | d1$contralat_2 == 1, 1, 0)
d1$CP_ACR90 <- ifelse(d1$chronic_pain == 1 & d1$ACR90 == 1, 1, 0)

# --- WP2019_AGE widespread pain (2019 criteria) ---
# 5 body regions; each = 1 if any site in that region has pain (1 = Ja)

region1 <- ifelse(d1$VAR12_2 == 1 | d1$VAR12_23 == 1 | d1$VAR12_5 == 1 |
  d1$VAR12_26 == 1 | d1$VAR12_7 == 1 | d1$VAR12_28 == 1 | d1$VAR12_9 == 1 |
  d1$VAR12_11 == 1 | d1$VAR12_30 == 1 | d1$VAR12_32 == 1, 1, 0)

region2 <- ifelse(d1$VAR12_1 == 1 | d1$VAR12_24 == 1 | d1$VAR12_4 == 1 |
  d1$VAR12_27 == 1 | d1$VAR12_6 == 1 | d1$VAR12_29 == 1 | d1$VAR12_8 == 1 |
  d1$VAR12_10 == 1 | d1$VAR12_31 == 1 | d1$VAR12_33 == 1, 1, 0)

region3 <- ifelse(d1$VAR12_18 == 1 | d1$VAR12_38 == 1 | d1$VAR12_40 == 1 |
  d1$VAR12_20 == 1 | d1$VAR12_22 == 1 | d1$VAR12_42 == 1 |
  d1$VAR12_44 == 1, 1, 0)

region4 <- ifelse(d1$VAR12_17 == 1 | d1$VAR12_39 == 1 | d1$VAR12_41 == 1 |
  d1$VAR12_19 == 1 | d1$VAR12_21 == 1 | d1$VAR12_43 == 1 |
  d1$VAR12_45 == 1, 1, 0)

region5 <- ifelse(d1$VAR12_3 == 1 | d1$VAR12_25 == 1 | d1$VAR12_34 == 1 |
  d1$VAR12_35 == 1 | d1$VAR12_36 == 1 | d1$VAR12_37 == 1 |
  d1$VAR12_12 == 1 | d1$VAR12_13 == 1 | d1$VAR12_14 == 1 |
  d1$VAR12_15 == 1, 1, 0)

nr_regions <- region1 + region2 + region3 + region4 + region5

# WPI_AGE: Widespread Pain Index, 19 anatomical locations (0-19)
d1$WPI_AGE <-
  ifelse(d1$VAR12_2  == 1 | d1$VAR12_23 == 1, 1, 0) +
  ifelse(d1$VAR12_5  == 1 | d1$VAR12_26 == 1, 1, 0) +
  ifelse(d1$VAR12_7  == 1 | d1$VAR12_28 == 1, 1, 0) +
  ifelse(d1$VAR12_9  == 1 | d1$VAR12_11 == 1 |
    d1$VAR12_30 == 1 | d1$VAR12_32 == 1, 1, 0) +
  ifelse(d1$VAR12_1  == 1 | d1$VAR12_24 == 1, 1, 0) +
  ifelse(d1$VAR12_4  == 1 | d1$VAR12_27 == 1, 1, 0) +
  ifelse(d1$VAR12_6  == 1 | d1$VAR12_29 == 1, 1, 0) +
  ifelse(d1$VAR12_8  == 1 | d1$VAR12_10 == 1 |
    d1$VAR12_31 == 1 | d1$VAR12_33 == 1, 1, 0) +
  ifelse(d1$VAR12_18 == 1 | d1$VAR12_38 == 1, 1, 0) +
  ifelse(d1$VAR12_18 == 1 | d1$VAR12_40 == 1, 1, 0) +
  ifelse(d1$VAR12_20 == 1 | d1$VAR12_22 == 1 |
    d1$VAR12_42 == 1 | d1$VAR12_44 == 1, 1, 0) +
  ifelse(d1$VAR12_17 == 1 | d1$VAR12_39 == 1, 1, 0) +
  ifelse(d1$VAR12_17 == 1 | d1$VAR12_41 == 1, 1, 0) +
  ifelse(d1$VAR12_19 == 1 | d1$VAR12_21 == 1 |
    d1$VAR12_43 == 1 | d1$VAR12_45 == 1, 1, 0) +
  ifelse(d1$VAR12_3  == 1 | d1$VAR12_25 == 1, 1, 0) +
  ifelse(d1$VAR12_34 == 1 | d1$VAR12_35 == 1, 1, 0) +
  ifelse(d1$VAR12_36 == 1 | d1$VAR12_37 == 1, 1, 0) +
  ifelse(d1$VAR12_12 == 1 | d1$VAR12_13 == 1, 1, 0) +
  ifelse(d1$VAR12_14 == 1 | d1$VAR12_15 == 1, 1, 0)

d1$WPI_AGE <- ifelse(is.na(d1$pain7days), NA, d1$WPI_AGE)
d1$WPI_AGE <- ifelse(is.na(d1$WPI_AGE) & d1$pain7days == 0, 0, d1$WPI_AGE)

# WP2019_AGE: 1 if WPI > 6 AND pain in >= 4 of 5 regions
d1$WP2019_AGE <- ifelse(d1$WPI_AGE > 6 & nr_regions > 3, 1, 0)

# CP_WP2019_AGE: chronic + widespread (2019)
d1$CP_WP2019_AGE <- ifelse(d1$chronic_pain == 1 & d1$WP2019_AGE == 1, 1, 0)

# Pain3cat_CWSP2019: 0=not chronic, 1=chronic not CWSP, 2=chronic CWSP
temp <- ifelse(d1$chronic_pain == 1, 1, 0)
d1$Pain3cat_CWSP2019 <- ifelse(
  d1$chronic_pain == 1 & d1$CP_WP2019_AGE == 1, 2, temp)
rm(region1, region2, region3, region4, region5, nr_regions, temp)

# == 6. Treatments (VAR20 non-exclusive, raw 1=Ja/2=Nej) =====================
# NA treated as 0 (no treatment).

d1$trt_mmr       <- 2 - d1$VAR20_1
d1$trt_mmr[is.na(d1$VAR20_1)] <- 0

d1$trt_neurostim <- 2 - d1$VAR20_2
d1$trt_neurostim[is.na(d1$VAR20_2)] <- 0

d1$trt_surgery   <- 2 - d1$VAR20_3
d1$trt_surgery[is.na(d1$VAR20_3)] <- 0

d1$trt_opioid    <- 2 - d1$VAR20_5
d1$trt_opioid[is.na(d1$VAR20_5)] <- 0

d1$trt_other     <- 2 - d1$VAR20_6
d1$trt_other[is.na(d1$VAR20_6)] <- 0

# == 7. Questionnaire scores ==================================================

# PSQ-14: mean of 14 pain sensitivity items (excludes items 5, 9, 13)
d1$psq14 <- (d1$VAR21_1 + d1$VAR21_2 + d1$VAR21_3 + d1$VAR21_4 +
  d1$VAR21_6 + d1$VAR21_7 + d1$VAR21_8 + d1$VAR21_10 + d1$VAR21_11 +
  d1$VAR21_12 + d1$VAR21_14 + d1$VAR21_15 + d1$VAR21_16 + d1$VAR21_17) / 14

# PSQ-17: mean of all 17 pain sensitivity items
d1$psq17 <- (d1$VAR21_1 + d1$VAR21_2 + d1$VAR21_3 + d1$VAR21_4 +
  d1$VAR21_5 + d1$VAR21_6 + d1$VAR21_7 + d1$VAR21_8 + d1$VAR21_9 +
  d1$VAR21_10 + d1$VAR21_11 + d1$VAR21_12 + d1$VAR21_13 + d1$VAR21_14 +
  d1$VAR21_15 + d1$VAR21_16 + d1$VAR21_17) / 17

# CSI: Central Sensitization Inventory part A (0-100, 25 items scored 0-4)
d1$csi <- (d1$VAR22_1 + d1$VAR22_2 + d1$VAR22_3 + d1$VAR22_4 + d1$VAR22_5 +
  d1$VAR22_6 + d1$VAR22_7 + d1$VAR22_8 + d1$VAR22_9 + d1$VAR22_10 +
  d1$VAR22_11 + d1$VAR22_12 + d1$VAR22_13 + d1$VAR22_14 + d1$VAR22_15 +
  d1$VAR22_16 + d1$VAR22_17 + d1$VAR22_18 + d1$VAR22_19 + d1$VAR22_20 +
  d1$VAR22_21 + d1$VAR22_22 + d1$VAR22_23 + d1$VAR22_24 + d1$VAR22_25) - 25

# CSI part B diagnoses (VAR23, matrix: 1=No -> 0, 2=Yes -> 1)
d1$restlesslegs     <- d1$VAR23_1 - 1
d1$chronic_fatigue  <- d1$VAR23_2 - 1
d1$fibromyalgia     <- d1$VAR23_3 - 1
d1$TMD              <- d1$VAR23_4 - 1
d1$headache_migraine <- d1$VAR23_5 - 1
d1$ibs              <- d1$VAR23_6 - 1
d1$chemical_sens    <- d1$VAR23_7 - 1
d1$neckpain         <- d1$VAR23_8 - 1
d1$anxiety          <- d1$VAR23_9 - 1
d1$depression       <- d1$VAR23_10 - 1

# Physician diagnoses (VAR24, matrix: 1=Yes -> 1, 2=No -> 0)
d1$RA_arthrosis  <- 2 - d1$VAR24_1
d1$burnout       <- 2 - d1$VAR24_2
d1$cardiovasc    <- 2 - d1$VAR24_3
d1$diabetes12    <- 2 - d1$VAR24_4
d1$respiratory   <- 2 - d1$VAR24_5
d1$dermatol      <- 2 - d1$VAR24_6
d1$cancer_tumor  <- 2 - d1$VAR24_7
d1$postcovid     <- 2 - d1$VAR24_8

# == 8. HAD (Hospital Anxiety and Depression Scale) ===========================
# 14 items: anxiety = odd (VAR25,27,29,31,33,35,37),
#           depression = even (VAR26,28,30,32,34,36,38).
# Each item has 4 options scored: a=3, b=2, c=1, d=0.
# Formula per item: 3*(2-VAR_1) + 2*(2-VAR_2) + 1*(2-VAR_3)
# (the 0*(2-VAR_4) term is omitted since it is always zero)

# had_a: anxiety subscale, 0-21
d1$had_a <-
  (3 * (2 - d1$VAR25_1) + 2 * (2 - d1$VAR25_2) + (2 - d1$VAR25_3)) +
  (3 * (2 - d1$VAR27_1) + 2 * (2 - d1$VAR27_2) + (2 - d1$VAR27_3)) +
  (3 * (2 - d1$VAR29_1) + 2 * (2 - d1$VAR29_2) + (2 - d1$VAR29_3)) +
  (3 * (2 - d1$VAR31_1) + 2 * (2 - d1$VAR31_2) + (2 - d1$VAR31_3)) +
  (3 * (2 - d1$VAR33_1) + 2 * (2 - d1$VAR33_2) + (2 - d1$VAR33_3)) +
  (3 * (2 - d1$VAR35_1) + 2 * (2 - d1$VAR35_2) + (2 - d1$VAR35_3)) +
  (3 * (2 - d1$VAR37_1) + 2 * (2 - d1$VAR37_2) + (2 - d1$VAR37_3))

# had_d: depression subscale, 0-21
d1$had_d <-
  (3 * (2 - d1$VAR26_1) + 2 * (2 - d1$VAR26_2) + (2 - d1$VAR26_3)) +
  (3 * (2 - d1$VAR28_1) + 2 * (2 - d1$VAR28_2) + (2 - d1$VAR28_3)) +
  (3 * (2 - d1$VAR30_1) + 2 * (2 - d1$VAR30_2) + (2 - d1$VAR30_3)) +
  (3 * (2 - d1$VAR32_1) + 2 * (2 - d1$VAR32_2) + (2 - d1$VAR32_3)) +
  (3 * (2 - d1$VAR34_1) + 2 * (2 - d1$VAR34_2) + (2 - d1$VAR34_3)) +
  (3 * (2 - d1$VAR36_1) + 2 * (2 - d1$VAR36_2) + (2 - d1$VAR36_3)) +
  (3 * (2 - d1$VAR38_1) + 2 * (2 - d1$VAR38_2) + (2 - d1$VAR38_3))

# == 9. SCI-93 ================================================================
# 35 items (VAR39_1..35), stored 1-5, rescaled to 0-4.  Sum - 35, range 0-140.

d1$sci93 <- (d1$VAR39_1 + d1$VAR39_2 + d1$VAR39_3 + d1$VAR39_4 +
  d1$VAR39_5 + d1$VAR39_6 + d1$VAR39_7 + d1$VAR39_8 + d1$VAR39_9 +
  d1$VAR39_10 + d1$VAR39_11 + d1$VAR39_12 + d1$VAR39_13 + d1$VAR39_14 +
  d1$VAR39_15 + d1$VAR39_16 + d1$VAR39_17 + d1$VAR39_18 + d1$VAR39_19 +
  d1$VAR39_20 + d1$VAR39_21 + d1$VAR39_22 + d1$VAR39_23 + d1$VAR39_24 +
  d1$VAR39_25 + d1$VAR39_26 + d1$VAR39_27 + d1$VAR39_28 + d1$VAR39_29 +
  d1$VAR39_30 + d1$VAR39_31 + d1$VAR39_32 + d1$VAR39_33 + d1$VAR39_34 +
  d1$VAR39_35) - 35

# == 10. ISI (Insomnia Severity Index) ========================================
# VAR40_1..3: direct 1-5 values, subtract 1 -> 0-4
# VAR41..44:  exclusive dummies (raw), standard pattern -> 0-4
# Total range: 0-28

d1$ISI <-
  (d1$VAR40_1 - 1) + (d1$VAR40_2 - 1) + (d1$VAR40_3 - 1) +
  ((2 - d1$VAR41_1) + 2 * (2 - d1$VAR41_2) + 3 * (2 - d1$VAR41_3) +
    4 * (2 - d1$VAR41_4) + 5 * (2 - d1$VAR41_5) - 1) +
  ((2 - d1$VAR42_1) + 2 * (2 - d1$VAR42_2) + 3 * (2 - d1$VAR42_3) +
    4 * (2 - d1$VAR42_4) + 5 * (2 - d1$VAR42_5) - 1) +
  ((2 - d1$VAR43_1) + 2 * (2 - d1$VAR43_2) + 3 * (2 - d1$VAR43_3) +
    4 * (2 - d1$VAR43_4) + 5 * (2 - d1$VAR43_5) - 1) +
  ((2 - d1$VAR44_1) + 2 * (2 - d1$VAR44_2) + 3 * (2 - d1$VAR44_3) +
    4 * (2 - d1$VAR44_4) + 5 * (2 - d1$VAR44_5) - 1)

# == 11. Physical activity ====================================================

# PE_time: time per week for physical exercise, 0-5 (VAR45, 6 options)
d1$PE_time <- (-1) + (2 - d1$VAR45_1) + 2 * (2 - d1$VAR45_2) +
  3 * (2 - d1$VAR45_3) + 4 * (2 - d1$VAR45_4) + 5 * (2 - d1$VAR45_5) +
  6 * (2 - d1$VAR45_6)

# PE_days: days per week with >= 30 min exercise, 0-7 (VAR46, 8 options)
d1$PE_days <- (-1) + (2 - d1$VAR46_1) + 2 * (2 - d1$VAR46_2) +
  3 * (2 - d1$VAR46_3) + 4 * (2 - d1$VAR46_4) + 5 * (2 - d1$VAR46_5) +
  6 * (2 - d1$VAR46_6) + 7 * (2 - d1$VAR46_7) + 8 * (2 - d1$VAR46_8)

# PA_time: time per week for everyday physical activity, 0-6 (VAR47, 7 options)
d1$PA_time <- (-1) + (2 - d1$VAR47_1) + 2 * (2 - d1$VAR47_2) +
  3 * (2 - d1$VAR47_3) + 4 * (2 - d1$VAR47_4) + 5 * (2 - d1$VAR47_5) +
  6 * (2 - d1$VAR47_6) + 7 * (2 - d1$VAR47_7)

# PA_days: days per week with >= 30 min activity, 0-7 (VAR48, 8 options)
d1$PA_days <- (-1) + (2 - d1$VAR48_1) + 2 * (2 - d1$VAR48_2) +
  3 * (2 - d1$VAR48_3) + 4 * (2 - d1$VAR48_4) + 5 * (2 - d1$VAR48_5) +
  6 * (2 - d1$VAR48_6) + 7 * (2 - d1$VAR48_7) + 8 * (2 - d1$VAR48_8)

# == 12. Smoking and snus =====================================================

# smoke: 0=not smoking/quit>6mo, 1=quit<6mo, 2=not daily, 3=daily (VAR49)
d1$smoke <- (-1) + (2 - d1$VAR49_1) + 2 * (2 - d1$VAR49_2) +
  3 * (2 - d1$VAR49_3) + 4 * (2 - d1$VAR49_4)

# ant_smoke: cigarettes/day (1-3 categories), 0 if not daily smoker (VAR50)
d1$ant_smoke <- (2 - d1$VAR50_1) + 2 * (2 - d1$VAR50_2) +
  3 * (2 - d1$VAR50_3)
d1$ant_smoke[d1$smoke < 3] <- 0

# snuff: 0=not using/quit>6mo, 1=quit<6mo, 2=not daily, 3=daily (VAR51)
d1$snuff <- (-1) + (2 - d1$VAR51_1) + 2 * (2 - d1$VAR51_2) +
  3 * (2 - d1$VAR51_3) + 4 * (2 - d1$VAR51_4)

# ant_snuff: boxes/week (1-3 categories), 0 if not daily snuffer (VAR52)
d1$ant_snuff <- (2 - d1$VAR52_1) + 2 * (2 - d1$VAR52_2) +
  3 * (2 - d1$VAR52_3)
d1$ant_snuff[d1$snuff < 3] <- 0

rm(current_date, current_year, var10_cols, col)

# == Run pipeline =============================================================

d1 <- combine_exclusive(d1, codebook_exclusive, group_info)
d1 <- recode_non_exclusive(d1, codebook_non_exclusive)
d1 <- apply_labels(d1, label_table, codebook_exclusive, group_info)
verify_exclusive_data(d1, group_info)

# == Inspect results ==========================================================
#
# codebook_exclusive     — what each numeric level means per combined variable
#                          e.g. VAR04: 1 = Man, 2 = Kvinna
# codebook_non_exclusive — what each binary column represents
#                          e.g. VAR12_1: body region 1
# group_info             — summary of all groups and their classification
# d1                     — the transformed dataset

# View(codebook_exclusive)
# View(codebook_non_exclusive)
# View(group_info)
# View(d1)
