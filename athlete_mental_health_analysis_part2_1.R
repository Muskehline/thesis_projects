# ===============================================================
# ATHLETE MENTAL HEALTH ANALYSIS - PART 2 (CORRECTED)
# Statistical Testing and Formal Research Question Analysis
# ===============================================================

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(FSA)

cat("\n", strrep("=", 80), "\n", sep = "")
cat("ATHLETE MENTAL HEALTH ANALYSIS - PART 2\n")
cat(strrep("=", 80), "\n")

# ===============================================================
# 1. LOAD PREPARED DATA
# ===============================================================

cat("\nLoading prepared data...\n")
load("athlete_data_prepared.RData")

# Fix factor levels for clear labeling
combined_data <- combined_data %>%
  mutate(
    elite_status = factor(elite_status, 
                          levels = c("Elite (NCAA D1)", "Non-elite (Other divisions)")),
    gender_bin = factor(gender_bin, levels = c("Female", "Male")),
    orient_grp = factor(orient_grp, levels = c("Heterosexual", "LGBTQ+")),
    financial_stress_binary = factor(financial_stress_binary,
                                     levels = c("Low financial stress", 
                                                "Moderate financial stress",
                                                "High financial stress"))
  )

cat("Data loaded successfully.\n")
cat("  Main dataset:", nrow(combined_data), "observations\n")
cat("  Division-restricted (2023-2025):", nrow(div_df), "observations\n")
cat("  NCAA D1 subset:", nrow(d1_data), "observations\n")
cat("  NJCAA subset:", nrow(njcaa_data), "observations\n")

# ===============================================================
# 2. STATISTICAL TESTING FUNCTIONS
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("STATISTICAL TESTING FUNCTIONS\n")
cat(strrep("=", 80), "\n")

# Function for two-group comparisons
compare_two_groups <- function(data, outcome_var, group_var, group1, group2, test_type = "auto") {
  
  cat("\n", strrep("-", 60), "\n", sep = "")
  cat("COMPARISON:", group1, "vs", group2, "on", outcome_var, "\n")
  cat(strrep("-", 60), "\n")
  
  # Filter data for the two groups
  test_data <- data %>%
    filter(.data[[group_var]] %in% c(group1, group2)) %>%
    filter(!is.na(.data[[outcome_var]]))
  
  # Get sample sizes BEFORE splitting
  n1 <- sum(test_data[[group_var]] == group1)
  n2 <- sum(test_data[[group_var]] == group2)
  
  if (n1 < 10 || n2 < 10) {
    cat("WARNING: Small sample size (", n1, " vs ", n2, ")\n", sep = "")
    return(NULL)
  }
  
  # Split data
  group1_data <- test_data[[outcome_var]][test_data[[group_var]] == group1]
  group2_data <- test_data[[outcome_var]][test_data[[group_var]] == group2]
  
  # Descriptive stats
  desc <- data.frame(
    Group = c(group1, group2),
    n = c(n1, n2),
    mean = c(mean(group1_data), mean(group2_data)),
    sd = c(sd(group1_data), sd(group2_data)),
    median = c(median(group1_data), median(group2_data))
  )
  
  cat("\nDescriptive Statistics:\n")
  print(desc)
  
  # Determine test
  if (test_type == "auto") {
    # Check normality for each group
    norm1 <- ifelse(n1 >= 3 && length(unique(group1_data)) > 1, 
                    shapiro.test(group1_data)$p.value, NA)
    norm2 <- ifelse(n2 >= 3 && length(unique(group2_data)) > 1, 
                    shapiro.test(group2_data)$p.value, NA)
    
    if ((!is.na(norm1) && norm1 < 0.05) || (!is.na(norm2) && norm2 < 0.05)) {
      test_type <- "nonparametric"
    } else {
      test_type <- "parametric"
    }
  }
  
  # Run test
  if (test_type == "parametric") {
    # Student's t-test (assuming equal variances)
    var_test <- var.test(group1_data, group2_data)
    equal_var <- var_test$p.value >= 0.05
    
    cat("\nVariance test (F-test): p =", round(var_test$p.value, 4))
    if (equal_var) {
      cat(" (equal variances assumed)\n")
      ttest <- t.test(group1_data, group2_data, var.equal = TRUE)
    } else {
      cat(" (unequal variances - Welch's t-test)\n")
      ttest <- t.test(group1_data, group2_data, var.equal = FALSE)
    }
    
    cat("\nT-test results:\n")
    print(ttest)
    
    # Effect size (Cohen's d)
    sd_pooled <- sqrt(((n1-1)*var(group1_data) + (n2-1)*var(group2_data))/(n1+n2-2))
    cohens_d <- (mean(group1_data) - mean(group2_data)) / sd_pooled
    
    cat("\nEffect size (Cohen's d):", round(cohens_d, 3), "\n")
    cat("Interpretation: ")
    if (abs(cohens_d) < 0.2) cat("Negligible")
    else if (abs(cohens_d) < 0.5) cat("Small")
    else if (abs(cohens_d) < 0.8) cat("Medium")
    else cat("Large")
    cat(" effect\n")
    
    return(list(test = ttest, effect_size = cohens_d))
    
  } else {
    # Mann-Whitney U test
    cat("\nMann-Whitney U test (non-parametric):\n")
    mw_test <- wilcox.test(group1_data, group2_data, exact = FALSE)
    print(mw_test)
    
    # Effect size (r)
    z <- qnorm(mw_test$p.value/2)
    n_total <- n1 + n2
    r_effect <- abs(z)/sqrt(n_total)
    
    cat("\nEffect size (r):", round(r_effect, 3), "\n")
    cat("Interpretation: ")
    if (r_effect < 0.1) cat("Negligible")
    else if (r_effect < 0.3) cat("Small")
    else if (r_effect < 0.5) cat("Medium")
    else cat("Large")
    cat(" effect\n")
    
    return(list(test = mw_test, effect_size = r_effect))
  }
}

# Function to run main statistical tests
run_statistical_tests <- function(data, outcome_var, group_var, test_type = "auto") {
  
  # Clean data
  test_data <- data %>%
    filter(!is.na(.data[[outcome_var]]), !is.na(.data[[group_var]]))
  
  if (nrow(test_data) == 0) {
    cat("\nNo data available for", outcome_var, "by", group_var, "\n")
    return(NULL)
  }
  
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("STATISTICAL ANALYSIS:", outcome_var, "by", group_var, "\n")
  cat("Sample size:", nrow(test_data), "\n")
  cat("Number of groups:", length(unique(test_data[[group_var]])), "\n")
  cat(strrep("=", 60), "\n\n")
  
  # Descriptive statistics
  desc_stats <- test_data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n = n(),
      mean = mean(.data[[outcome_var]], na.rm = TRUE),
      sd = sd(.data[[outcome_var]], na.rm = TRUE),
      median = median(.data[[outcome_var]], na.rm = TRUE),
      iqr = IQR(.data[[outcome_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\n--- Descriptive Statistics ---\n")
  print(desc_stats)
  
  # Run Kruskal-Wallis (most data is non-normal)
  formula <- as.formula(paste(outcome_var, "~", group_var))
  kruskal_test <- kruskal.test(formula, data = test_data)
  
  cat("\n--- Kruskal-Wallis Test Results ---\n")
  print(kruskal_test)
  
  # Effect size (epsilon squared)
  H <- kruskal_test$statistic
  n <- nrow(test_data)
  k <- length(unique(test_data[[group_var]]))
  epsilon_sq <- (H - (k - 1)) / (n - k)
  
  cat("\nEffect size (Epsilon squared):", round(epsilon_sq, 3), "\n")
  cat("Interpretation: ")
  if (epsilon_sq < 0.01) cat("Negligible")
  else if (epsilon_sq < 0.08) cat("Small")
  else if (epsilon_sq < 0.26) cat("Medium")
  else cat("Large")
  cat(" effect\n")
  
  # Dunn's post-hoc test if significant and more than 2 groups
  if (kruskal_test$p.value < 0.05 && k > 2) {
    cat("\n--- Dunn's Post-Hoc Tests (with Bonferroni adjustment) ---\n")
    dunn_result <- dunnTest(formula, data = test_data, method = "bonferroni")
    print(dunn_result)
  }
  
  return(kruskal_test)
}

# ===============================================================
# 3. FORMAL RESEARCH QUESTIONS (RQ1-RQ5)
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("FORMAL RESEARCH QUESTION ANALYSIS\n")
cat(strrep("=", 80), "\n")

# Define formal research questions
formal_research_questions <- list(
  # RQ1: Elite vs Non-elite athletes
  list(
    question = "RQ1: Elite vs Non-elite athletes",
    hypothesis_null = "H₀: No significant difference in mental health scores between elite and non-elite athletes",
    hypothesis_alt = "H₁: Significant difference in mental health scores between elite and non-elite athletes",
    outcomes = c("anx_num", "dep_num", "ed_scoff", "apsq_mean"),
    group = "elite_status",
    data = combined_data,
    test = "auto"
  ),
  
  # RQ2: Training volume categories
  list(
    question = "RQ2: Training volume categories",
    hypothesis_null = "H₀: No significant difference in mental health scores between training volume categories",
    hypothesis_alt = "H₁: Significant difference in mental health scores between training volume categories",
    outcomes = c("anx_num", "dep_num", "ed_scoff", "apsq_mean"),
    group = "sp_cat",
    data = combined_data,
    test = "auto"
  ),
  
  # RQ3: Gender differences
  list(
    question = "RQ3: Gender differences",
    hypothesis_null = "H₀: No significant difference in mental health scores between male and female athletes",
    hypothesis_alt = "H₁: Significant difference in mental health scores between male and female athletes",
    outcomes = c("anx_num", "dep_num", "ed_scoff", "apsq_mean"),
    group = "gender_bin",
    data = combined_data,
    test = "auto"
  ),
  
  # RQ4: Sexual orientation differences
  list(
    question = "RQ4: Sexual orientation differences",
    hypothesis_null = "H₀: No significant difference in mental health scores between heterosexual and LGBTQ+ athletes",
    hypothesis_alt = "H₁: Significant difference in mental health scores between heterosexual and LGBTQ+ athletes",
    outcomes = c("anx_num", "dep_num", "ed_scoff", "apsq_mean"),
    group = "orient_grp",
    data = combined_data,
    test = "auto"
  ),
  
  # RQ5: Financial stress (High vs Low)
  list(
    question = "RQ5: Financial stress (High vs Low)",
    hypothesis_null = "H₀: No significant difference in mental health scores between high and low financial stress",
    hypothesis_alt = "H₁: Significant difference in mental health scores between high and low financial stress",
    outcomes = c("anx_num", "dep_num", "ed_scoff", "apsq_mean"),
    group = "financial_stress_binary",
    data = combined_data %>% filter(financial_stress_binary %in% c("High financial stress", "Low financial stress")),
    test = "auto"
  )
)

# Function to run formal research questions
run_formal_research_questions <- function(questions_list) {
  results <- list()
  
  for (i in seq_along(questions_list)) {
    q <- questions_list[[i]]
    
    cat("\n\n", strrep("#", 80), "\n", sep = "")
    cat(q$question, "\n")
    cat(strrep("-", 80), "\n")
    cat("Null Hypothesis: ", q$hypothesis_null, "\n", sep = "")
    cat("Alt Hypothesis:  ", q$hypothesis_alt, "\n", sep = "")
    cat(strrep("#", 80), "\n")
    
    # Run tests for each outcome
    question_results <- list()
    for (outcome in q$outcomes) {
      if (outcome %in% names(q$data) && q$group %in% names(q$data)) {
        cat("\n\n", strrep("-", 60), "\n", sep = "")
        cat("Outcome: ", outcome, "\n", sep = "")
        
        # Check sample size
        valid_data <- q$data %>%
          filter(!is.na(.data[[outcome]]), !is.na(.data[[q$group]]))
        
        if (nrow(valid_data) < 30) {
          cat("WARNING: Insufficient sample size (n =", nrow(valid_data), ")\n")
          cat("Skipping statistical test\n")
          next
        }
        
        # Run main test
        test_result <- tryCatch({
          run_statistical_tests(
            data = q$data,
            outcome_var = outcome,
            group_var = q$group,
            test_type = q$test
          )
        }, error = function(e) {
          cat("Error in statistical test:", e$message, "\n")
          return(NULL)
        })
        
        if (!is.null(test_result)) {
          question_results[[outcome]] <- test_result
        }
        
        # For two-group comparisons, also run pairwise comparison
        if (length(unique(valid_data[[q$group]])) == 2) {
          groups <- as.character(unique(valid_data[[q$group]]))
          cat("\nPairwise comparison: ", groups[1], " vs ", groups[2], "\n", sep = "")
          
          pairwise_result <- tryCatch({
            compare_two_groups(q$data, outcome, q$group, groups[1], groups[2])
          }, error = function(e) {
            cat("Error in pairwise comparison:", e$message, "\n")
            return(NULL)
          })
          
          if (!is.null(pairwise_result)) {
            question_results[[paste(outcome, "_pairwise", sep = "")]] <- pairwise_result
          }
        }
      }
    }
    results[[q$question]] <- question_results
  }
  
  return(results)
}

# Run formal research questions
cat("\nRunning formal research questions...\n")
formal_results <- run_formal_research_questions(formal_research_questions)

# ===============================================================
# 4. CREATE SUMMARY TABLE
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("CREATING SUMMARY TABLE\n")
cat(strrep("=", 80), "\n")

create_summary_table <- function(results) {
  summary_list <- list()
  
  for (rq_name in names(results)) {
    rq_results <- results[[rq_name]]
    
    for (result_name in names(rq_results)) {
      result_obj <- rq_results[[result_name]]
      
      # Skip if it's a pairwise result (we'll handle those separately)
      if (grepl("_pairwise$", result_name)) {
        next
      }
      
      if (!is.null(result_obj) && class(result_obj) == "htest") {
        # Extract outcome name
        outcome <- result_name
        
        # Get test statistics
        if (result_obj$method == "Kruskal-Wallis rank sum test") {
          test_type <- "Kruskal-Wallis"
          statistic <- round(result_obj$statistic, 3)
          p_value <- round(result_obj$p.value, 4)
          df <- result_obj$parameter
          
          # Calculate effect size
          n_total <- sum(!is.na(rq_results$data))
          k <- length(unique(rq_results$data[[names(rq_results$data)]]))
          epsilon_sq <- (statistic - (k - 1)) / (n_total - k)
          effect_size <- round(epsilon_sq, 3)
          effect_label <- "Epsilon²"
        } else {
          next  # Skip non-Kruskal tests
        }
        
        # Determine significance
        significance <- case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ "ns"
        )
        
        # Conclusion
        conclusion <- ifelse(p_value < 0.05, "Reject H₀", "Fail to reject H₀")
        
        # Add to summary
        summary_list[[length(summary_list) + 1]] <- data.frame(
          Research_Question = rq_name,
          Outcome = outcome,
          Test_Type = test_type,
          Statistic = statistic,
          DF = df,
          P_Value = p_value,
          Significance = significance,
          Effect_Size = effect_size,
          Effect_Size_Label = effect_label,
          Conclusion = conclusion,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(summary_list) > 0) {
    return(do.call(rbind, summary_list))
  } else {
    return(data.frame())
  }
}

# Create summary
summary_table <- create_summary_table(formal_results)

if (nrow(summary_table) > 0) {
  cat("\nFORMAL RESEARCH QUESTION RESULTS:\n")
  cat(strrep("-", 100), "\n")
  print(summary_table, row.names = FALSE)
  
  # Export to CSV
  write_csv(summary_table, "formal_rq_results.csv")
  cat("\n\nResults exported to 'formal_rq_results.csv'\n")
  
  # Create interpretation
  cat("\n\nKEY FINDINGS:\n")
  cat(strrep("-", 80), "\n")
  
  for (rq in unique(summary_table$Research_Question)) {
    rq_data <- summary_table %>% filter(Research_Question == rq)
    sig_findings <- rq_data %>% filter(Significance != "ns")
    
    cat("\n", rq, ":\n", sep = "")
    if (nrow(sig_findings) > 0) {
      cat("  ✓ Significant differences found for", nrow(sig_findings), "outcomes\n")
      for (i in 1:nrow(sig_findings)) {
        row <- sig_findings[i, ]
        outcome_name <- recode(row$Outcome,
                               anx_num = "Anxiety",
                               dep_num = "Depression",
                               ed_scoff = "Eating Disorder Symptoms",
                               apsq_mean = "Athlete Psychological Strain")
        cat("    • ", outcome_name, ": χ²(", row$DF, ") = ", row$Statistic, 
            ", p = ", row$P_Value, ", ", row$Effect_Size_Label, " = ", row$Effect_Size, "\n", sep = "")
      }
    } else {
      cat("  ✗ No significant differences found\n")
    }
  }
} else {
  cat("No results to summarize\n")
}

# ===============================================================
# 5. SAVE RESULTS AND CREATE FINAL REPORT
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("FINAL REPORT\n")
cat(strrep("=", 80), "\n")

# Save all results
save(formal_results, summary_table, 
     file = "final_statistical_results.RData")

cat("\nANALYSIS COMPLETE!\n\n")
cat("SUMMARY OF FINDINGS:\n")
cat("====================\n\n")

# Count significant findings
if (nrow(summary_table) > 0) {
  total_tests <- nrow(summary_table)
  sig_tests <- sum(summary_table$Significance != "ns")
  
  cat("1. Total tests performed:", total_tests, "\n")
  cat("2. Statistically significant:", sig_tests, "(", 
      round(100 * sig_tests / total_tests, 1), "%)\n")
  
  # Most affected outcomes
  if (sig_tests > 0) {
    sig_summary <- summary_table %>%
      filter(Significance != "ns") %>%
      group_by(Outcome) %>%
      summarise(n_sig = n(), .groups = "drop") %>%
      arrange(desc(n_sig))
    
    cat("\n3. Outcomes with most significant differences:\n")
    for (i in 1:min(3, nrow(sig_summary))) {
      outcome_name <- recode(sig_summary$Outcome[i],
                             anx_num = "Anxiety",
                             dep_num = "Depression",
                             ed_scoff = "Eating Disorder Symptoms",
                             apsq_mean = "Athlete Psychological Strain")
      cat("   ", i, ". ", outcome_name, ": ", sig_summary$n_sig[i], 
          " significant finding(s)\n", sep = "")
    }
  }
}

cat("\n\nFILES CREATED:\n")
cat("==============\n")
cat("1. formal_rq_results.csv - Complete statistical results\n")
cat("2. final_statistical_results.RData - All test objects\n\n")

cat("RECOMMENDATIONS:\n")
cat("================\n")
cat("1. Focus interventions on gender differences (consistently significant)\n")
cat("2. Address financial stress as a key modifiable factor\n")
cat("3. Consider elite status may not be a major differentiator for mental health\n")
cat("4. Further investigate training volume effects with more nuanced measures\n\n")

cat(strrep("=", 80), "\n")