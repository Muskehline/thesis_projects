# ===============================================================
# ATHLETE MENTAL HEALTH ANALYSIS - PART 2 (COMPLETE WITH VISUALIZATIONS)
# Statistical Testing and Formal Research Question Analysis
# ===============================================================

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(FSA)
library(patchwork)  # For combining plots

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
    sp_cat = factor(sp_cat, levels = 1:7, labels = cat_labels)
  )

cat("Data loaded successfully.\n")
cat("  Main dataset:", nrow(combined_data), "observations\n")

# ===============================================================
# 2. CREATE VISUALIZATIONS FOR KEY FINDINGS
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("CREATING KEY VISUALIZATIONS\n")
cat(strrep("=", 80), "\n")

# Function to create boxplot for comparison
create_comparison_plot <- function(data, x_var, y_var, title, x_label, y_label) {
  ggplot(data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]])),
         aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.8) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# 1. Gender Differences Visualization
cat("\n1. Creating gender comparison plots...\n")
p_gender_anx <- create_comparison_plot(
  combined_data, "gender_bin", "anx_num",
  "Gender Differences in Anxiety (GAD-7)",
  "Gender", "Anxiety Score (0-21)"
) + scale_fill_manual(values = pal_gender)

p_gender_dep <- create_comparison_plot(
  combined_data, "gender_bin", "dep_num",
  "Gender Differences in Depression (PHQ-9)",
  "Gender", "Depression Score (0-27)"
) + scale_fill_manual(values = pal_gender)

# Combine gender plots
p_gender_combined <- p_gender_anx + p_gender_dep +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Gender Differences in Mental Health Outcomes",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
print(p_gender_combined)

# 2. Financial Stress Visualization
cat("\n2. Creating financial stress plots...\n")
financial_plot_data <- combined_data %>%
  filter(financial_stress_binary %in% c("Low financial stress", "High financial stress"))

p_finstress <- ggplot(financial_plot_data %>%
                        filter(!is.na(financial_stress_binary), !is.na(anx_num)),
                      aes(x = financial_stress_binary, y = anx_num, fill = financial_stress_binary)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA) +
  labs(title = "Anxiety by Financial Stress Level",
       subtitle = "High financial stress associated with significantly higher anxiety",
       x = "Financial Stress Level",
       y = "Anxiety Score (GAD-7)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#59A14F", "#F1CE63"))
print(p_finstress)

# 3. Sexual Orientation Visualization
cat("\n3. Creating sexual orientation plots...\n")
p_orientation <- ggplot(combined_data %>%
                          filter(!is.na(orient_grp), !is.na(anx_num)),
                        aes(x = orient_grp, y = anx_num, fill = orient_grp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray40") +
  labs(title = "Anxiety by Sexual Orientation",
       subtitle = "LGBTQ+ athletes report higher anxiety scores",
       x = "Sexual Orientation",
       y = "Anxiety Score (GAD-7)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = pal_orient)
print(p_orientation)

# 4. Training Volume Visualization
cat("\n4. Creating training volume plots...\n")
p_training <- ggplot(combined_data %>%
                       filter(!is.na(sp_cat), sp_cat != "NA", !is.na(anx_num)),
                     aes(x = sp_cat, y = anx_num)) +
  geom_boxplot(fill = "#4E79A8", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.5, color = "gray40") +
  labs(title = "Anxiety by Weekly Training Hours",
       x = "Weekly Sport Hours (Category)",
       y = "Anxiety Score (GAD-7)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_training)

# 5. Effect Size Visualization
cat("\n5. Creating effect size summary plot...\n")

# Create effect size data frame from your results
effect_data <- data.frame(
  Research_Question = rep(c("Gender", "Sexual Orientation", "Financial Stress", "Training Volume"), each = 2),
  Outcome = rep(c("Anxiety", "Depression"), 4),
  Effect_Size = c(0.073, 0.032,  # Gender
                  0.035, 0.043,   # Sexual Orientation
                  0.121, 0.147,   # Financial Stress
                  0.002, 0.006)   # Training Volume
)

p_effect <- ggplot(effect_data, aes(x = Research_Question, y = Effect_Size, fill = Outcome)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  labs(title = "Effect Sizes by Research Question",
       subtitle = "Financial stress shows largest effects, training volume shows smallest",
       x = "Research Question",
       y = "Effect Size (ε²)",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#E15759", "#76B7B2")) +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_hline(yintercept = 0.08, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_text(x = 1, y = 0.005, label = "Negligible", color = "gray50", size = 3) +
  geom_text(x = 1, y = 0.045, label = "Small", color = "gray50", size = 3) +
  geom_text(x = 1, y = 0.125, label = "Medium", color = "gray50", size = 3)
print(p_effect)

# ===============================================================
# 3. CREATE FINAL REPORT
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("FINAL ANALYSIS REPORT\n")
cat(strrep("=", 80), "\n\n")

cat("ATHLETE MENTAL HEALTH ANALYSIS REPORT\n")
cat("=====================================\n")
cat("Date:", format(Sys.Date(), "%B %d, %Y"), "\n")
cat("Sample Size:", nrow(combined_data), "athletes\n\n")

cat("EXECUTIVE SUMMARY\n")
cat("================\n")
cat("This analysis examined mental health differences across five key dimensions:\n")
cat("1. Elite vs Non-elite status\n")
cat("2. Training volume categories\n")
cat("3. Gender differences\n")
cat("4. Sexual orientation differences\n")
cat("5. Financial stress levels\n\n")

cat("KEY FINDINGS\n")
cat("============\n")
cat("✓ Gender Differences: Highly significant across all outcomes (p < .001)\n")
cat("  - Female athletes report higher anxiety, depression, eating disorder\n")
cat("    symptoms, and psychological strain\n")
cat("  - Effect sizes: Small (ε² = 0.032 to 0.073)\n\n")

cat("✓ Sexual Orientation: Significant differences across all outcomes (p < .001)\n")
cat("  - LGBTQ+ athletes report higher scores on all mental health measures\n")
cat("  - Effect sizes: Small (ε² = 0.016 to 0.043)\n\n")

cat("✓ Financial Stress: Strongest associations found (p < .001)\n")
cat("  - High financial stress linked to significantly worse mental health\n")
cat("  - Effect sizes: Medium for anxiety (ε² = 0.121) and depression (ε² = 0.147)\n\n")

cat("✓ Training Volume: Statistically significant but small effects (p < .001)\n")
cat("  - Differences exist across training categories\n")
cat("  - Effect sizes: Negligible to small (ε² = 0.002 to 0.013)\n\n")

cat("✓ Elite Status: No significant differences found\n")
cat("  - Elite (NCAA D1) and non-elite athletes show similar mental health profiles\n")
cat("  - All p-values > .05, supporting null hypothesis\n\n")

cat("STATISTICAL SUMMARY\n")
cat("===================\n")
cat("Total statistical tests: 20\n")
cat("Significant findings: 16 (80%)\n")
cat("Non-significant: 4 (20%)\n\n")

cat("Effect Size Interpretation:\n")
cat("  ε² < 0.01: Negligible effect\n")
cat("  0.01 ≤ ε² < 0.08: Small effect\n")
cat("  0.08 ≤ ε² < 0.26: Medium effect\n")
cat("  ε² ≥ 0.26: Large effect\n\n")

cat("PRACTICAL IMPLICATIONS\n")
cat("=====================\n")
cat("1. TARGETED INTERVENTIONS NEEDED:\n")
cat("   • Female athletes require gender-sensitive mental health support\n")
cat("   • LGBTQ+ athletes need inclusive, affirming care environments\n")
cat("   • Financial stress screening and support programs are crucial\n\n")

cat("2. RESOURCE ALLOCATION:\n")
cat("   • Prioritize psychosocial factors over athletic division/status\n")
cat("   • Financial support may yield largest mental health benefits\n")
cat("   • Training load monitoring important but not primary focus\n\n")

cat("3. POLICY RECOMMENDATIONS:\n")
cat("   • Implement mandatory mental health screening for all athletes\n")
cat("   • Develop financial literacy and support programs\n")
cat("   • Create inclusive environments for diverse athlete populations\n")
cat("   • Train coaches/staff on mental health risk factors\n\n")

cat("LIMITATIONS\n")
cat("===========\n")
cat("1. Cross-sectional design limits causal inferences\n")
cat("2. Self-report measures subject to response bias\n")
cat("3. Limited data on some subgroups (e.g., specific NJCAA divisions)\n")
cat("4. Training volume measured categorically rather than continuously\n\n")

cat("FUTURE RESEARCH DIRECTIONS\n")
cat("==========================\n")
cat("1. Longitudinal studies to examine mental health trajectories\n")
cat("2. Qualitative research to understand athlete experiences\n")
cat("3. Intervention studies for financial stress reduction\n")
cat("4. Examination of protective factors and resilience\n\n")

# ===============================================================
# 4. EXPORT FINAL RESULTS
# ===============================================================

cat("\n\n", strrep("=", 80), "\n", sep = "")
cat("EXPORTING FINAL RESULTS\n")
cat(strrep("=", 80), "\n")

# Create comprehensive results data frame
final_results <- data.frame(
  Research_Question = c(rep("RQ1: Elite Status", 4),
                        rep("RQ2: Training Volume", 4),
                        rep("RQ3: Gender", 4),
                        rep("RQ4: Sexual Orientation", 4),
                        rep("RQ5: Financial Stress", 4)),
  Outcome = rep(c("Anxiety (GAD-7)", "Depression (PHQ-9)", 
                  "Eating Disorder Symptoms (SCOFF)", 
                  "Athlete Psychological Strain (APSQ)"), 5),
  Test_Statistic = c("χ²(1)=0.05", "χ²(1)=3.64", "χ²(1)=0.28", "χ²(1)=1.31",
                     "χ²(6)=27.29", "χ²(6)=59.42", "χ²(6)=26.61", "χ²(6)=114.69",
                     "χ²(1)=645.30", "χ²(1)=285.77", "χ²(1)=315.51", "χ²(1)=284.28",
                     "χ²(1)=304.78", "χ²(1)=368.91", "χ²(1)=129.69", "χ²(1)=127.64",
                     "χ²(1)=884.61", "χ²(1)=1070.09", "χ²(1)=272.77", "χ²(1)=327.44"),
  P_Value = c("0.830", "0.056", "0.598", "0.252",
              "0.000128", "5.89e-11", "0.000171", "<2e-16",
              "<2e-16", "<2e-16", "<2e-16", "<2e-16",
              "<2e-16", "<2e-16", "<2e-16", "<2e-16",
              "<2e-16", "<2e-16", "<2e-16", "<2e-16"),
  Effect_Size = c("0.000", "0.001", "0.000", "0.000",
                  "0.002", "0.006", "0.003", "0.013",
                  "0.073", "0.032", "0.046", "0.035",
                  "0.035", "0.043", "0.019", "0.016",
                  "0.121", "0.147", "0.045", "0.062"),
  Significance = c("ns", "ns", "ns", "ns",
                   "***", "***", "***", "***",
                   "***", "***", "***", "***",
                   "***", "***", "***", "***",
                   "***", "***", "***", "***"),
  Conclusion = c(rep("Fail to reject H₀", 4),
                 rep("Reject H₀", 16))
)

# Export to CSV
write_csv(final_results, "comprehensive_results_summary.csv")
cat("\n✓ Comprehensive results saved to 'comprehensive_results_summary.csv'\n")

# Save visualizations
ggsave("gender_comparison.png", p_gender_combined, width = 12, height = 6)
ggsave("financial_stress.png", p_finstress, width = 8, height = 6)
ggsave("sexual_orientation.png", p_orientation, width = 8, height = 6)
ggsave("training_volume.png", p_training, width = 10, height = 6)
ggsave("effect_sizes.png", p_effect, width = 10, height = 6)

cat("✓ Visualizations saved as PNG files:\n")
cat("  - gender_comparison.png\n")
cat("  - financial_stress.png\n")
cat("  - sexual_orientation.png\n")
cat("  - training_volume.png\n")
cat("  - effect_sizes.png\n")

# Create and save final report as text file
report_text <- paste(
  "ATHLETE MENTAL HEALTH ANALYSIS REPORT",
  "=====================================",
  paste("Date:", format(Sys.Date(), "%B %d, %Y")),
  paste("Sample Size:", nrow(combined_data), "athletes"),
  "",
  "EXECUTIVE SUMMARY",
  "================",
  "This analysis examined mental health differences across five key dimensions:",
  "1. Elite vs Non-elite status",
  "2. Training volume categories", 
  "3. Gender differences",
  "4. Sexual orientation differences",
  "5. Financial stress levels",
  "",
  "KEY FINDINGS",
  "============",
  "✓ Gender Differences: Highly significant across all outcomes (p < .001)",
  "  - Female athletes report higher anxiety, depression, eating disorder",
  "    symptoms, and psychological strain",
  "  - Effect sizes: Small (ε² = 0.032 to 0.073)",
  "",
  "✓ Sexual Orientation: Significant differences across all outcomes (p < .001)",
  "  - LGBTQ+ athletes report higher scores on all mental health measures",
  "  - Effect sizes: Small (ε² = 0.016 to 0.043)",
  "",
  "✓ Financial Stress: Strongest associations found (p < .001)",
  "  - High financial stress linked to significantly worse mental health",
  "  - Effect sizes: Medium for anxiety (ε² = 0.121) and depression (ε² = 0.147)",
  "",
  "✓ Training Volume: Statistically significant but small effects (p < .001)",
  "  - Differences exist across training categories",
  "  - Effect sizes: Negligible to small (ε² = 0.002 to 0.013)",
  "",
  "✓ Elite Status: No significant differences found",
  "  - Elite (NCAA D1) and non-elite athletes show similar mental health profiles",
  "  - All p-values > .05, supporting null hypothesis",
  "",
  "STATISTICAL SUMMARY",
  "===================",
  "Total statistical tests: 20",
  "Significant findings: 16 (80%)",
  "Non-significant: 4 (20%)",
  "",
  "PRACTICAL IMPLICATIONS",
  "=====================",
  "1. Targeted interventions needed for female and LGBTQ+ athletes",
  "2. Financial support programs may improve athlete well-being",
  "3. Focus on individual needs rather than division/elite status",
  "4. Monitor training loads but prioritize psychosocial factors",
  sep = "\n"
)

writeLines(report_text, "analysis_report.txt")
cat("✓ Final report saved to 'analysis_report.txt'\n")

# ===============================================================
# 5. FINAL MESSAGE
# ===============================================================

cat("\n\n", strrep("*", 80), "\n", sep = "")
cat("ANALYSIS COMPLETE - ALL OUTPUTS GENERATED\n")
cat(strrep("*", 80), "\n\n")

cat("FILES CREATED:\n")
cat("==============\n")
cat("1. comprehensive_results_summary.csv - Complete statistical results\n")
cat("2. analysis_report.txt - Executive summary and key findings\n")
cat("3. gender_comparison.png - Gender differences visualization\n")
cat("4. financial_stress.png - Financial stress effects visualization\n")
cat("5. sexual_orientation.png - Sexual orientation differences visualization\n")
cat("6. training_volume.png - Training volume effects visualization\n")
cat("7. effect_sizes.png - Effect sizes comparison visualization\n\n")

cat("KEY TAKEAWAYS FOR PRESENTATION:\n")
cat("===============================\n")
cat("1. Financial stress shows STRONGEST effects on mental health\n")
cat("2. Gender and sexual orientation show CONSISTENT differences\n")
cat("3. Elite status does NOT predict mental health outcomes\n")
cat("4. Training volume effects are STATISTICAL but not PRACTICAL\n\n")

cat("RECOMMENDED SLIDES FOR PRESENTATION:\n")
cat("====================================\n")
cat("1. Slide 1: Executive Summary & Key Findings\n")
cat("2. Slide 2: Gender Differences Visualization\n")
cat("3. Slide 3: Financial Stress Effects\n")
cat("4. Slide 4: Sexual Orientation Differences\n")
cat("5. Slide 5: Effect Sizes Comparison\n")
cat("6. Slide 6: Practical Implications & Recommendations\n\n")

cat(strrep("=", 80), "\n")
cat("END OF ATHLETE MENTAL HEALTH ANALYSIS\n")
cat(strrep("=", 80), "\n")