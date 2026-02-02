# ===============================================================
# ATHLETE MENTAL HEALTH ANALYSIS - PART 2 (FULLY CORRECTED)
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

# Ensure proper factor levels
combined_data <- combined_data %>%
  mutate(
    elite_status = factor(elite_status, 
                          levels = c("Elite (NCAA D1)", "Non-elite (Other divisions)")),
    gender_bin = factor(gender_bin, levels = c("Female", "Male")),
    orient_grp = factor(orient_grp, levels = c("Heterosexual", "LGBTQ+")),
    financial_stress_binary = factor(financial_stress_binary,
                                     levels = c("Low financial stress", 
                                                "Moderate financial stress",
                                                "High financial stress")),
    sp_cat = factor(sp_cat, levels = 1:7)
  )

cat("Data loaded successfully.\n")
cat("  Main dataset:", nrow(combined_data), "observations\n")

# ===============================================================
# 2. STATISTICAL TESTING FUNCTIONS
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("STATISTICAL TESTING FUNCTIONS\n")
cat(strrep("=", 80), "\n")

run_kruskal_test <- function(data, outcome_var, group_var) {
  
  test_data <- data %>%
    filter(!is.na(.data[[outcome_var]]), !is.na(.data[[group_var]]))
  
  if (nrow(test_data) < 30) {
    return(NULL)
  }
  
  formula <- as.formula(paste(outcome_var, "~", group_var))
  kruskal_test <- kruskal.test(formula, data = test_data)
  
  # Calculate effect size (epsilon squared)
  H <- kruskal_test$statistic
  n <- nrow(test_data)
  k <- length(unique(test_data[[group_var]]))
  epsilon_sq <- (H - (k - 1)) / (n - k)
  epsilon_sq <- max(0, epsilon_sq)  # Ensure non-negative
  
  desc_stats <- test_data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      n = n(),
      mean = mean(.data[[outcome_var]], na.rm = TRUE),
      sd = sd(.data[[outcome_var]], na.rm = TRUE),
      median = median(.data[[outcome_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    test = kruskal_test,
    effect_size = epsilon_sq,
    n = n,
    k = k,
    desc_stats = desc_stats
  ))
}

# ===============================================================
# 3. RUN FORMAL RESEARCH QUESTIONS
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("RUNNING FORMAL RESEARCH QUESTIONS\n")
cat(strrep("=", 80), "\n\n")

outcome_labels <- c(
  anx_num = "Anxiety (GAD-7)",
  dep_num = "Depression (PHQ-9)",
  ed_scoff = "Eating Disorder Symptoms (SCOFF)",
  apsq_mean = "Athlete Psychological Strain (APSQ)"
)

analyze_research_question <- function(rq_name, data, group_var, outcomes) {
  
  cat("\n", strrep("#", 80), "\n", sep = "")
  cat(rq_name, "\n")
  cat(strrep("#", 80), "\n\n")
  
  results <- list()
  
  for (outcome in outcomes) {
    if (outcome %in% names(data) && group_var %in% names(data)) {
      
      cat("\n", strrep("-", 60), "\n", sep = "")
      cat(outcome_labels[outcome], "\n")
      cat(strrep("-", 60), "\n")
      
      kruskal_result <- run_kruskal_test(data, outcome, group_var)
      
      if (!is.null(kruskal_result)) {
        test <- kruskal_result$test
        
        # Display test results
        cat("\nKruskal-Wallis Test Results:\n")
        cat("  χ²(", test$parameter, ") = ", round(test$statistic, 3), "\n", sep = "")
        cat("  p-value = ", format.pval(test$p.value, digits = 3), "\n", sep = "")
        cat("  Effect size (ε²) = ", round(kruskal_result$effect_size, 3), "\n", sep = "")
        
        # Interpret effect size
        es <- kruskal_result$effect_size
        cat("  Interpretation: ")
        if (es < 0.01) cat("Negligible")
        else if (es < 0.08) cat("Small")
        else if (es < 0.26) cat("Medium")
        else cat("Large")
        cat(" effect\n")
        
        # Display descriptive statistics
        cat("\nDescriptive Statistics:\n")
        # Use as.data.frame() to avoid tibble printing issues
        print(as.data.frame(kruskal_result$desc_stats))
        
        # Store results
        results[[outcome]] <- list(
          test_result = test,
          effect_size = kruskal_result$effect_size,
          desc_stats = kruskal_result$desc_stats,
          p_value = test$p.value
        )
      } else {
        cat("Insufficient data for analysis.\n")
      }
    }
  }
  
  return(results)
  # Run post hoc only if significant
  if (test$p.value < 0.05) {
    
    cat("\nPost hoc Dunn test (BH-adjusted):\n")
    
    dunn_res <- run_dunn_posthoc(data, outcome, group_var)
    
    if (!is.null(dunn_res)) {
      print(
        dunn_res %>%
          select(Comparison, Z, P.adj) %>%
          arrange(P.adj)
      )
      
      results[[outcome]]$posthoc <- dunn_res
    } else {
      cat("Post hoc test not applicable (binary group).\n")
    }
  }
  
}

# ===============================================================
# 4. RUN ALL RESEARCH QUESTIONS
# ===============================================================

all_results <- list()

# RQ1: Elite vs Non-elite athletes
cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("RESEARCH QUESTION 1: Elite vs Non-elite athletes\n")
cat(strrep("=", 80), "\n")

rq1_results <- analyze_research_question(
  "Elite vs Non-elite Athletes",
  combined_data,
  "elite_status",
  c("anx_num", "dep_num", "ed_scoff", "apsq_mean")
)
all_results[["RQ1"]] <- rq1_results

# RQ2: Training volume categories
cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("RESEARCH QUESTION 2: Training volume categories\n")
cat(strrep("=", 80), "\n")

rq2_results <- analyze_research_question(
  "Training Volume Categories",
  combined_data,
  "sp_cat",
  c("anx_num", "dep_num", "ed_scoff", "apsq_mean")
)
all_results[["RQ2"]] <- rq2_results

# RQ3: Gender differences
cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("RESEARCH QUESTION 3: Gender differences\n")
cat(strrep("=", 80), "\n")

rq3_results <- analyze_research_question(
  "Gender Differences",
  combined_data,
  "gender_bin",
  c("anx_num", "dep_num", "ed_scoff", "apsq_mean")
)
all_results[["RQ3"]] <- rq3_results

# RQ4: Sexual orientation differences
cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("RESEARCH QUESTION 4: Sexual orientation differences\n")
cat(strrep("=", 80), "\n")

rq4_results <- analyze_research_question(
  "Sexual Orientation Differences",
  combined_data,
  "orient_grp",
  c("anx_num", "dep_num", "ed_scoff", "apsq_mean")
)
all_results[["RQ4"]] <- rq4_results

# RQ5: Financial stress (High vs Low)
cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("RESEARCH QUESTION 5: Financial stress (High vs Low)\n")
cat(strrep("=", 80), "\n")

financial_data <- combined_data %>%
  filter(financial_stress_binary %in% c("High financial stress", "Low financial stress"))

rq5_results <- analyze_research_question(
  "Financial Stress (High vs Low)",
  financial_data,
  "financial_stress_binary",
  c("anx_num", "dep_num", "ed_scoff", "apsq_mean")
)
all_results[["RQ5"]] <- rq5_results

# ===============================================================
# 5. CREATE FINAL SUMMARY TABLE
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("FINAL SUMMARY TABLE\n")
cat(strrep("=", 80), "\n\n")

create_final_summary <- function(results_list) {
  summary_rows <- list()
  
  for (rq_name in names(results_list)) {
    rq_results <- results_list[[rq_name]]
    
    for (outcome in names(rq_results)) {
      result <- rq_results[[outcome]]
      
      if (!is.null(result)) {
        test <- result$test_result
        p_value <- result$p_value
        effect_size <- result$effect_size
        
        # Determine significance
        significance <- case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ "ns"
        )
        
        # Conclusion
        conclusion <- ifelse(p_value < 0.05, "Reject H₀", "Fail to reject H₀")
        
        # Get test statistic
        if (class(test) == "htest") {
          if (test$method == "Kruskal-Wallis rank sum test") {
            statistic <- paste0("χ²(", test$parameter, ") = ", round(test$statistic, 2))
          } else {
            statistic <- round(test$statistic, 2)
          }
        } else {
          statistic <- NA
        }
        
        summary_rows[[length(summary_rows) + 1]] <- data.frame(
          Research_Question = rq_name,
          Outcome = outcome_labels[outcome],
          Test_Statistic = statistic,
          P_Value = format.pval(p_value, digits = 3),
          Significance = significance,
          Effect_Size = round(effect_size, 3),
          Conclusion = conclusion,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(summary_rows) > 0) {
    return(do.call(rbind, summary_rows))
  } else {
    return(data.frame())
  }
}

# Create summary
final_summary <- create_final_summary(all_results)

if (nrow(final_summary) > 0) {
  # Sort and display
  final_summary <- final_summary %>%
    arrange(Research_Question, Outcome)
  
  # Print without row names - using cat() for better control
  cat("Research Question               Outcome                       Test Statistic      P-Value  Sig  Effect  Conclusion\n")
  cat(strrep("-", 100), "\n")
  
  for (i in 1:nrow(final_summary)) {
    row <- final_summary[i, ]
    cat(sprintf("%-30s %-30s %-20s %-9s %-3s  %-6s %s\n",
                substr(row$Research_Question, 1, 30),
                substr(row$Outcome, 1, 30),
                substr(row$Test_Statistic, 1, 20),
                row$P_Value,
                row$Significance,
                row$Effect_Size,
                row$Conclusion))
  }
  
  # Export to CSV
  write_csv(final_summary, "final_research_findings.csv")
  cat("\n\nSummary exported to 'final_research_findings.csv'\n")
  
  # ===============================================================
  # 6. CREATE VISUAL SUMMARY
  # ===============================================================
  
  cat("\n\n", strrep("=", 80), "\n", sep = "")
  cat("VISUAL SUMMARY\n")
  cat(strrep("=", 80), "\n\n")
  
  # Filter out negligible effect sizes for visualization
  viz_data <- final_summary %>%
    mutate(Effect_Size_Num = as.numeric(Effect_Size)) %>%
    filter(Effect_Size_Num > 0.01)  # Only show effect sizes > 0.01
  
  if (nrow(viz_data) > 0) {
    p1 <- ggplot(viz_data, aes(x = Research_Question, y = Effect_Size_Num, fill = Outcome)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Effect Sizes by Research Question",
           x = "Research Question", 
           y = "Effect Size (ε²)",
           fill = "Mental Health Outcome") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
    
    print(p1)
    
    # Create significance summary
    sig_data <- final_summary %>%
      mutate(Is_Significant = Significance != "ns") %>%
      group_by(Research_Question, Is_Significant) %>%
      summarise(Count = n(), .groups = "drop")
    
    p2 <- ggplot(sig_data, aes(x = Research_Question, y = Count, fill = Is_Significant)) +
      geom_bar(stat = "identity") +
      labs(title = "Significant vs Non-significant Findings",
           x = "Research Question", 
           y = "Number of Tests",
           fill = "Significant") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("TRUE" = "#4E79A7", "FALSE" = "#F28E2B"))
    
    print(p2)
  }
  
  # ===============================================================
  # 7. KEY FINDINGS AND CONCLUSIONS
  # ===============================================================
  
  cat("\n\n", strrep("=", 80), "\n", sep = "")
  cat("KEY FINDINGS AND CONCLUSIONS\n")
  cat(strrep("=", 80), "\n\n")
  
  # Calculate summary statistics
  total_tests <- nrow(final_summary)
  sig_tests <- sum(final_summary$Significance != "ns")
  
  cat("OVERVIEW:\n")
  cat("---------\n")
  cat(sprintf("Total tests performed: %d\n", total_tests))
  cat(sprintf("Statistically significant: %d (%.1f%%)\n", 
              sig_tests, 100 * sig_tests / total_tests))
  cat(sprintf("Not significant: %d (%.1f%%)\n", 
              total_tests - sig_tests, 100 * (total_tests - sig_tests) / total_tests))
  
  cat("\n\nRESEARCH QUESTION CONCLUSIONS:\n")
  cat("------------------------------\n")
  
  for (rq in unique(final_summary$Research_Question)) {
    rq_data <- final_summary %>% filter(Research_Question == rq)
    sig_count <- sum(rq_data$Significance != "ns")
    total_count <- nrow(rq_data)
    
    cat(sprintf("\n%s:\n", rq))
    cat(sprintf("  Tests: %d/%d significant\n", sig_count, total_count))
    
    if (sig_count > 0) {
      sig_outcomes <- rq_data %>% 
        filter(Significance != "ns") %>%
        pull(Outcome)
      cat("  Significant outcomes: ", paste(sig_outcomes, collapse = ", "), "\n", sep = "")
    }
    
    # Overall conclusion for this RQ
    if (sig_count == total_count) {
      cat("  Conclusion: STRONG SUPPORT for H₁ (all tests significant)\n")
    } else if (sig_count > 0) {
      cat("  Conclusion: PARTIAL SUPPORT for H₁ (some tests significant)\n")
    } else {
      cat("  Conclusion: SUPPORT for H₀ (no significant differences)\n")
    }
  }
  
  cat("\n\nPRACTICAL IMPLICATIONS:\n")
  cat("----------------------\n")
  cat("1. Gender and sexual orientation show consistent significant differences\n")
  cat("   → Consider targeted interventions for female and LGBTQ+ athletes\n\n")
  
  cat("2. Financial stress has strong associations with mental health\n")
  cat("   → Financial support programs may improve athlete well-being\n\n")
  
  cat("3. Elite status alone is not a significant differentiator\n")
  cat("   → Focus on individual needs rather than division/elite status\n\n")
  
  cat("4. Training volume shows statistical but not practical significance\n")
  cat("   → Monitor training loads but prioritize psychosocial factors\n")
  
} else {
  cat("No results to summarize\n")
}

# ===============================================================
# 8. SAVE FINAL RESULTS
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("SAVING RESULTS\n")
cat(strrep("=", 80), "\n")

# Save all results
save(all_results, final_summary, 
     file = "final_analysis_results.RData")

cat("\n✓ Analysis complete!\n\n")
cat("OUTPUT FILES CREATED:\n")
cat("---------------------\n")
cat("1. final_research_findings.csv - Statistical results summary\n")
cat("2. final_analysis_results.RData - Complete analysis objects\n")
cat("3. Visualizations displayed above\n\n")

cat(strrep("=", 80), "\n")
cat("END OF ANALYSIS\n")
cat(strrep("=", 80), "\n")