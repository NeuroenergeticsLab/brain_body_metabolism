# List of files where maingroup = main analysis and subgroup = severely hypoglycemic subgroup (n=6)
file_paths <- c(
    '../Data/LMEinput_roischaefer_CMRO2_maingroup.csv',
    '../Data/LMEinput_roischaefer_CMRO2_subgroup.csv',
    '../Data/LMEinput_roischaefer_CBF_maingroup.csv',
    '../Data/LMEinput_roischaefer_CBF_subgroup.csv'
    
)

for (file_path in file_paths) {
    # Load the data
    input_data <- read.csv(file_path)

    # Prepare the data
    input_data$condition <- factor(input_data$condition)
    input_data$condition <- relevel(input_data$condition, ref = "euart")
    input_data$yeo_nw <- factor(input_data$yeo_network)
    input_data$yeo_nw <- relevel(input_data$yeo_nw, ref = "Vis")
    
    # Check for missing values
    if (any(is.na(input_data))) {
        cat("Removing rows with NA values in ", file_path, "...\n")
        input_data <- na.omit(input_data)
    }

    # Print levels of factors before fitting the model
    cat("Levels of condition for file ", file_path, ": ", levels(input_data$condition), "\n")
    cat("Levels of yeo_nw for file ", file_path, ": ", levels(input_data$yeo_nw), "\n")
    
###Test whether adding the condition term improves the model significantly
    if (length(levels(input_data$condition)) > 1) {
        # Null model
        null_model <- lmer(region_median ~ yeo_nw + (1 | subject/condition), data = input_data, REML = FALSE)

        # Full model
        full_model <- lmer(region_median ~ condition * yeo_nw + (1 | subject/condition), data = input_data, REML = FALSE)

        # Compare models
        anova_comparison_1 <- anova(null_model, full_model)

###Test whether adding the condition term improves the model significantly
        # Extended model with epinephrine levels during MR scan
        extended_model <- lmer(region_median ~ condition * yeo_nw * z_epinephrineMR + (1 | subject/condition), data = input_data, REML = FALSE)

        anova_comparison_2 <- anova(full_model, extended_model)

###run final model
        # Final model and summary
        final_model <- full_model
        ci_fe <- confint(final_model)

        model_summary <- summary(final_model)
        r_squ <- r.squaredGLMM(final_model)

        # Print results
        cat("\nR-squared for Final Model for file: ", file_path, "\n")
        cat("Marginal R-squared (fixed effects only): ", r_squ[1], "\n")
        cat("Conditional R-squared (fixed + random effects): ", r_squ[2], "\n")

        cat("\nComparison of Null Model vs. Full Model for ", file_path, ":\n")
        print(anova_comparison_1)

        cat("\nComparison of Full Model vs. Extended Model for ", file_path, ":\n")
        print(anova_comparison_2)

        cat("\nSummary of Final Model for ", file_path, ":\n")
        print(model_summary)

        cat("\nConfidence Intervals for Final Model for ", file_path, ":\n")
        print(ci_fe)
    } else {
        cat("Skipping file ", file_path, " due to only one level in condition.\n")
    }
}
