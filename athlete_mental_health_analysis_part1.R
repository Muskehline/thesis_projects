# ===============================================================
# ATHLETE MENTAL HEALTH ANALYSIS - PART 1
# Data Preparation, Descriptive Analysis, and Basic Visualizations
# ===============================================================

# Install required packages (run once)
# install.packages(c("tidyverse", "readr", "dplyr", "tidyr", "ggplot2", 
#                    "lubridate", "forcats", "plotly", "FSA", "ggcorrplot"))

# Load libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(plotly)
library(FSA)
library(ggcorrplot)

# ===============================================================
# 1. DATA LOADING AND PREPARATION
# ===============================================================

cat("\n", strrep("=", 80), "\n", sep = "")
cat("ATHLETE MENTAL HEALTH ANALYSIS - PART 1\n")
cat(strrep("=", 80), "\n")

# ---------- Load data ----------
cat("\nLoading dataset...\n")
final_dataset2 <- read_csv("final_dataset2.csv", show_col_types = FALSE, guess_max = 100000)
combined_data  <- final_dataset2
cat("Dataset loaded with", nrow(combined_data), "rows and", ncol(combined_data), "columns\n")

# ---------- Helper functions ----------
to_int_safe <- function(x) suppressWarnings(as.integer(as.character(x)))
cat_levels  <- 1:7
cat_labels  <- c("<1","1–2","3–5","6–10","11–15","16–20",">20")
safe_mean   <- function(x) mean(x, na.rm = TRUE)

# ---------- Core derived columns ----------
cat("\nCreating derived variables...\n")
combined_data <- combined_data %>%
  mutate(
    sp_cat    = as.integer(sp_time),
    sp_hours = case_when(
      sp_cat == 1 ~ 0.5,  sp_cat == 2 ~ 1.5,  sp_cat == 3 ~ 4,
      sp_cat == 4 ~ 8,    sp_cat == 5 ~ 13,   sp_cat == 6 ~ 18,
      sp_cat == 7 ~ 22,   TRUE ~ NA_real_
    ),
    anx_num  = suppressWarnings(as.numeric(anx_score)),
    dep_num  = suppressWarnings(as.numeric(deprawsc)),
    sex_birth_num = to_int_safe(sex_birth),
    gender_bin = case_when(
      sex_birth_num == 1 ~ "Female",
      sex_birth_num == 2 ~ "Male",
      TRUE ~ NA_character_
    ) %>% factor(c("Female","Male")),
    
    # Financial stress
    fincur_num = to_int_safe(fincur),
    fincur_lab = factor(fincur_num, levels = 1:5,
                        labels = c("Always stressful","Often stressful",
                                   "Sometimes stressful","Rarely stressful",
                                   "Never stressful")),
    
    # Division
    ath_div_num = to_int_safe(ath_div),
    ath_div_lab = factor(ath_div_num, levels = 1:7,
                         labels = c("NCAA D1","NCAA D2","NCAA D3","NAIA",
                                    "NJCAA D1","NJCAA D2","NJCAA D3")),
    
    # Elite status (RQ1)
    elite_status = case_when(
      ath_div_num == 1 ~ "Elite (NCAA D1)",
      !is.na(ath_div_num) ~ "Non-elite (Other divisions)",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Elite (NCAA D1)", "Non-elite (Other divisions)")),
    
    # Financial stress binary (RQ5)
    financial_stress_binary = case_when(
      fincur_num %in% c(1, 2) ~ "High financial stress",
      fincur_num %in% c(4, 5) ~ "Low financial stress",
      fincur_num == 3 ~ "Moderate financial stress",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Low financial stress", "Moderate financial stress", 
                            "High financial stress"))
  )

# ---------- SCOFF (0–5) ----------
scoff_vars <- c("scoff_1","scoff_2","scoff_3","scoff_4","scoff_5")
if (all(scoff_vars %in% names(combined_data))) {
  combined_data <- combined_data %>%
    mutate(across(all_of(scoff_vars), ~ suppressWarnings(as.numeric(.)))) %>%
    rowwise() %>%
    mutate(ed_scoff = if (any(is.na(c_across(all_of(scoff_vars))))) NA_real_
           else sum(c_across(all_of(scoff_vars)))) %>%
    ungroup() %>%
    mutate(ed_pos2 = ifelse(!is.na(ed_scoff) & ed_scoff >= 2, 1L, 
                            ifelse(is.na(ed_scoff), NA_integer_, 0L)))
} else if ("ed_scoff" %in% names(combined_data)) {
  combined_data <- combined_data %>% 
    mutate(ed_scoff = suppressWarnings(as.numeric(ed_scoff)))
}

# ---------- APSQ (mean 1–5; require >=7 answered) ----------
apsq_vars <- paste0("ath_psych_", 1:10)
if (all(apsq_vars %in% names(combined_data))) {
  combined_data <- combined_data %>%
    mutate(across(all_of(apsq_vars), ~ suppressWarnings(as.numeric(.)))) %>%
    mutate(
      apsq_n    = rowSums(!is.na(across(all_of(apsq_vars)))),
      apsq_mean = ifelse(apsq_n >= 7, rowMeans(across(all_of(apsq_vars)), na.rm = TRUE), NA_real_),
      apsq_sum  = ifelse(apsq_n >= 7, rowSums(across(all_of(apsq_vars)),  na.rm = TRUE), NA_real_)
    )
}

# ---------- Additional demographic variables ----------
cat("Creating demographic variables...\n")

# Year in program
if ("yr_sch" %in% names(combined_data)) {
  combined_data <- combined_data %>% mutate(
    yr_sch_num = to_int_safe(yr_sch),
    yr_sch_lab = factor(yr_sch_num, levels = 1:7,
                        labels = c("1st","2nd","3rd","4th","5th","6th","7th+")))
}

# International vs Domestic
if ("international" %in% names(combined_data)) {
  combined_data <- combined_data %>% mutate(
    intl_flag = case_when(
      to_int_safe(international) == 1 ~ "International",
      to_int_safe(international) == 0 ~ "Domestic",
      TRUE ~ NA_character_
    ) %>% factor(c("Domestic","International"))
  )
}

# Race/Ethnicity (collapsed)
race_cols <- c("race_white","race_asian","race_black","race_his","race_pi","race_mides")
has_race  <- intersect(race_cols, names(combined_data))
if (length(has_race) > 0) {
  combined_data <- combined_data %>%
    mutate(across(all_of(has_race), ~ as.integer(suppressWarnings(as.numeric(.))))) %>%
    mutate(
      race_count = rowSums(across(all_of(has_race)) == 1L, na.rm = TRUE),
      race_cat = case_when(
        race_count > 1 ~ "Multiracial",
        TRUE ~ case_when(
          `race_white` == 1 ~ "White",
          `race_black` == 1 ~ "Black",
          `race_asian` == 1 ~ "Asian",
          `race_his`   == 1 ~ "Hispanic/Latinx",
          `race_pi`    == 1 ~ "NH/PI",
          `race_mides` == 1 ~ "Middle Eastern/Arab",
          TRUE ~ NA_character_
        )
      ) %>% factor()
    )
}

# Sexual orientation (Heterosexual vs LGBTQ+)
sex_orient_cols <- c("sexual_h","sexual_l","sexual_g","sexual_bi","sexual_queer",
                     "sexual_quest","sexual_asexual","sexual_pan","sexual_selfID")
has_sex <- intersect(sex_orient_cols, names(combined_data))
if (length(has_sex) > 0) {
  combined_data <- combined_data %>%
    mutate(across(all_of(has_sex), ~ as.integer(suppressWarnings(as.numeric(.))))) %>%
    mutate(
      lgbtq_any = (rowSums(across(all_of(setdiff(has_sex,"sexual_h"))) == 1L, na.rm = TRUE) > 0),
      orient_grp = case_when(
        lgbtq_any ~ "LGBTQ+",
        `sexual_h` == 1 ~ "Heterosexual",
        TRUE ~ NA_character_
      ) %>% factor(c("Heterosexual","LGBTQ+"))
    )
}

# Socioeconomic status (Pell)
if ("pellgrant" %in% names(combined_data)) {
  combined_data <- combined_data %>%
    mutate(socioecon_status = case_when(
      to_int_safe(pellgrant) == 1 ~ "Lower income (Pell recipient)",
      to_int_safe(pellgrant) == 2 ~ "Higher income (No Pell)",
      TRUE ~ NA_character_
    ) %>% factor(c("Lower income (Pell recipient)", "Higher income (No Pell)")))
}

# ---------- Palettes ----------
pal_intl   <- c("Domestic"="#4C78A8","International"="#72B7B2")
pal_orient <- c("Heterosexual"="#8E6C8A","LGBTQ+"="#E45756")
pal_ses    <- c("Lower income (Pell recipient)"="#54A24B","Higher income (No Pell)"="#E39C37")
pal_gender <- c("Female"="#E15759", "Male"="#76B7B2")
pal_elite  <- c("Elite (NCAA D1)"="#4E79A7", "Non-elite (Other divisions)"="#F28E2B")

# ---------- Division filter 2023–2025 ----------
restrict_div_2325 <- function(df) {
  base <- df %>% filter(!is.na(ath_div_lab))
  if ("survey_year" %in% names(df)) base <- base %>% filter(survey_year >= 2023, survey_year <= 2025)
  base
}

div_df <- restrict_div_2325(combined_data)
div_title_suffix <- if ("survey_year" %in% names(combined_data)) "(2023–2025)" else "(where available; 2023–2025)"

cat("\nDivision-restricted dataset (2023-2025):", nrow(div_df), "observations\n")

# ---------- Create division-specific datasets ----------
d1_data <- div_df %>% filter(ath_div_lab == "NCAA D1")
njcaa_data <- div_df %>% filter(grepl("NJCAA", ath_div_lab))

cat("\nDivision-specific sample sizes:\n")
cat("  NCAA D1:", nrow(d1_data), "\n")
cat("  NJCAA (all divisions):", nrow(njcaa_data), "\n")

# ===============================================================
# 2. DESCRIPTIVE STATISTICS AND SHARE TABLES
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("DESCRIPTIVE STATISTICS\n")
cat(strrep("=", 80), "\n")

# Function for share tables
share_table <- function(df, var) {
  df %>% mutate(.g = .data[[var]]) %>%
    filter(!is.na(.g)) %>% count(.g, name = "n") %>%
    mutate(total = sum(n), pct = 100 * n / total) %>%
    arrange(desc(n)) %>% rename(category = .g) %>%
    select(category, n, pct, total)
}

# Overall mental health score summaries
cat("\nMental Health Outcome Summary Statistics:\n")
outcome_summary <- combined_data %>%
  summarise(
    Anxiety_mean = mean(anx_num, na.rm = TRUE),
    Anxiety_sd = sd(anx_num, na.rm = TRUE),
    Anxiety_n = sum(!is.na(anx_num)),
    Depression_mean = mean(dep_num, na.rm = TRUE),
    Depression_sd = sd(dep_num, na.rm = TRUE),
    Depression_n = sum(!is.na(dep_num)),
    SCOFF_mean = mean(ed_scoff, na.rm = TRUE),
    SCOFF_sd = sd(ed_scoff, na.rm = TRUE),
    SCOFF_n = sum(!is.na(ed_scoff)),
    APSQ_mean = mean(apsq_mean, na.rm = TRUE),
    APSQ_sd = sd(apsq_mean, na.rm = TRUE),
    APSQ_n = sum(!is.na(apsq_mean))
  )

print(outcome_summary)

# Demographic distributions
demogs <- list(
  "International vs Domestic" = "intl_flag",
  "Sexual orientation" = "orient_grp",
  "Gender" = "gender_bin",
  "Year in program" = "yr_sch_lab",
  "Race / Ethnicity" = "race_cat",
  "Socioeconomic status" = "socioecon_status",
  "Financial stress" = "fincur_lab",
  "Elite status" = "elite_status"
)

cat("\n\nDemographic Distributions:\n")
cat(strrep("-", 60), "\n")

overall_tables <- list()
for (label in names(demogs)) {
  var <- demogs[[label]]
  if (var %in% names(combined_data)) {
    tab <- share_table(combined_data, var)
    cat("\n", label, ":\n", sep = "")
    print(tab)
    overall_tables[[label]] <- tab %>% mutate(demographic = label, .before = 1)
  }
}

# Division counts
cat("\n\nDivision Counts ", div_title_suffix, ":\n", sep = "")
div_counts <- div_df %>%
  mutate(ath_div_lab = forcats::fct_drop(ath_div_lab)) %>%
  count(ath_div_lab, name = "n") %>%
  arrange(desc(n))
print(div_counts)

# ===============================================================
# 3. BASIC VISUALIZATIONS
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("BASIC VISUALIZATIONS\n")
cat(strrep("=", 80), "\n")

# Generic plotting helper
plot_lines_by_group_generic <- function(df, outcome, group_var, ttl, ylab, legend_ttl){
  s <- df %>%
    filter(!is.na(sp_cat), sp_cat %in% 1:7,
           !is.na(.data[[outcome]]), !is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]], sp_cat) %>%
    summarise(mean = mean(.data[[outcome]]), .groups = "drop")
  p <- ggplot(s, aes(x = factor(sp_cat, levels = cat_levels, labels = cat_labels),
                     y = mean, color = .data[[group_var]], group = .data[[group_var]])) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    labs(title = ttl, x = "Weekly sport hours (categories)", y = ylab, color = legend_ttl) +
    theme_minimal()
  if (group_var == "intl_flag") p <- p + scale_color_manual(values = pal_intl)
  if (group_var == "orient_grp") p <- p + scale_color_manual(values = pal_orient)
  if (group_var == "socioecon_status") p <- p + scale_color_manual(values = pal_ses)
  if (group_var == "gender_bin") p <- p + scale_color_manual(values = pal_gender)
  p
}

# Simple gender lines
cat("\nCreating gender comparison plots...\n")
anx_gender_means <- combined_data %>%
  filter(gender_bin %in% c("Female","Male"),
         !is.na(sp_cat), sp_cat %in% 1:7, !is.na(anx_num)) %>%
  group_by(gender_bin, sp_cat) %>% summarise(mean = mean(anx_num), .groups = "drop")

p_anx_gender <- ggplot(anx_gender_means,
                       aes(x = factor(sp_cat, levels = cat_levels, labels = cat_labels),
                           y = mean, color = gender_bin, group = gender_bin)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title = "Anxiety (GAD-7) by Weekly Training Category",
       x = "Weekly sport hours (categories)", y = "Mean GAD-7", color = "Gender") +
  theme_minimal() +
  scale_color_manual(values = pal_gender)
print(p_anx_gender)

dep_gender_means <- combined_data %>%
  filter(gender_bin %in% c("Female","Male"),
         !is.na(sp_cat), sp_cat %in% 1:7, !is.na(dep_num)) %>%
  group_by(gender_bin, sp_cat) %>% summarise(mean = mean(dep_num), .groups = "drop")

p_dep_gender <- ggplot(dep_gender_means,
                       aes(x = factor(sp_cat, levels = cat_levels, labels = cat_labels),
                           y = mean, color = gender_bin, group = gender_bin)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title = "Depression (PHQ-9) by Weekly Training Category",
       x = "Weekly sport hours (categories)", y = "Mean PHQ-9", color = "Gender") +
  theme_minimal() +
  scale_color_manual(values = pal_gender)
print(p_dep_gender)

# Financial stress plots
cat("\nCreating financial stress plots...\n")
p_anx_fs <- plot_lines_by_group_generic(
  combined_data, "anx_num", "fincur_lab",
  "Anxiety (GAD-7) by Weekly Training Category", "Mean GAD-7", "Financial stress")
p_dep_fs <- plot_lines_by_group_generic(
  combined_data, "dep_num", "fincur_lab",
  "Depression (PHQ-9) by Weekly Training Category", "Mean PHQ-9", "Financial stress")
print(p_anx_fs)
print(p_dep_fs)

# Division plots
cat("\nCreating division comparison plots...\n")
p_anx_div <- plot_lines_by_group_generic(
  div_df, "anx_num", "ath_div_lab",
  paste("Anxiety (GAD-7) by Weekly Training Category", div_title_suffix),
  "Mean GAD-7", "Division")
p_dep_div <- plot_lines_by_group_generic(
  div_df, "dep_num", "ath_div_lab",
  paste("Depression (PHQ-9) by Weekly Training Category", div_title_suffix),
  "Mean PHQ-9", "Division")
print(p_anx_div)
print(p_dep_div)

# Bar plots for overall patterns
cat("\nCreating bar plots...\n")
build_outcomes_long <- function(df) {
  outcome_cols <- intersect(c("anx_num","dep_num","ed_scoff","apsq_mean"), names(df))
  df %>%
    mutate(
      sp_cat = factor(sp_cat, levels = 1:7, labels = cat_labels),
      ath_div_lab = fct_drop(ath_div_lab),
      gender_bin  = fct_drop(gender_bin)
    ) %>%
    pivot_longer(
      cols = all_of(outcome_cols),
      names_to = "outcome", values_to = "value"
    ) %>%
    mutate(outcome_lab = recode(outcome,
                                anx_num   = "Anxiety (GAD-7)",
                                dep_num   = "Depression (PHQ-9)",
                                ed_scoff  = "Eating disorder (SCOFF)",
                                apsq_mean = "APSQ (1–5)"
    ))
}

long_all <- build_outcomes_long(combined_data)

# Overall mental health by training time
bars_sp <- long_all %>%
  filter(!is.na(sp_cat), !is.na(value)) %>%
  group_by(sp_cat, outcome_lab) %>%
  summarise(mean_value = safe_mean(value), n = n(), .groups = "drop") %>%
  ggplot(aes(x = sp_cat, y = mean_value)) +
  geom_col(fill = "#59A14F", alpha = 0.8) +
  labs(title = "Mental Health Outcomes by Weekly Training Category",
       x = "Weekly sport hours (category)", y = "Mean score") +
  facet_wrap(~ outcome_lab, scales = "free_y") + 
  theme_minimal()
print(bars_sp)

# Gender comparison bar plot
bars_gender <- long_all %>%
  filter(!is.na(gender_bin), gender_bin %in% c("Female","Male"), !is.na(value)) %>%
  group_by(gender_bin, outcome_lab) %>%
  summarise(mean_value = safe_mean(value), n = n(), .groups = "drop") %>%
  ggplot(aes(x = gender_bin, y = mean_value, fill = gender_bin)) +
  geom_col(position = "dodge") +
  labs(title = "Mental Health Outcomes by Gender",
       x = "Gender", y = "Mean score", fill = "Gender") +
  facet_wrap(~ outcome_lab, scales = "free_y") + 
  theme_minimal() +
  scale_fill_manual(values = pal_gender)
print(bars_gender)

# Elite vs Non-elite bar plot
if ("elite_status" %in% names(long_all)) {
  bars_elite <- long_all %>%
    filter(!is.na(elite_status), !is.na(value)) %>%
    group_by(elite_status, outcome_lab) %>%
    summarise(mean_value = safe_mean(value), n = n(), .groups = "drop") %>%
    ggplot(aes(x = elite_status, y = mean_value, fill = elite_status)) +
    geom_col(position = "dodge") +
    labs(title = "Mental Health Outcomes by Elite Status",
         x = "Athlete Status", y = "Mean score", fill = "Status") +
    facet_wrap(~ outcome_lab, scales = "free_y") + 
    theme_minimal() +
    scale_fill_manual(values = pal_elite)
  print(bars_elite)
}

# ===============================================================
# 4. SAVE DATA FOR PART 2
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("SAVING DATA FOR STATISTICAL ANALYSIS\n")
cat(strrep("=", 80), "\n")

# Save all datasets for Part 2
save(combined_data, div_df, d1_data, njcaa_data, 
     pal_intl, pal_orient, pal_ses, pal_gender, pal_elite,
     cat_levels, cat_labels,
     file = "athlete_data_prepared.RData")

# Also save descriptive tables
descriptive_tables <- list(
  outcome_summary = outcome_summary,
  division_counts = div_counts,
  overall_tables = overall_tables
)
save(descriptive_tables, file = "descriptive_tables.RData")

cat("\nData saved for Part 2 analysis.\n")
cat("Files created:\n")
cat("1. athlete_data_prepared.RData - Prepared datasets\n")
cat("2. descriptive_tables.RData - Descriptive statistics\n\n")

cat("Next steps: Run 'athlete_mental_health_analysis_part2.R' for statistical testing\n")