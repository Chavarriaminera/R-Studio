library(ggplot2) 
library(car) 
library(nortest) 
library(dplyr)




# Here I am checking anova assumptions. 
Check_Anova_Assumptions <- function(dataset, Team, Total_Days) {
  
  # Function to check if sample size is sufficient for Shapiro-Wilk test
   # the function checks whether the length is within the acceptable range for this assessment. 
  shapiro_p_value <- function(x) {
    if (length(x) >= 3 & length(x) <= 5000) {
      return(shapiro.test(x)$p.value)
    } else {
      return(NA)  # Return NA for groups with insufficient sample size
    }
  }
  
  # Ensure Team is treated as a factor and Total_Days is numeric
  dataset[[Team]] <- as.factor(dataset[[Team]])
  dataset[[Total_Days]] <- as.numeric(dataset[[Total_Days]])
  
  normality <- dataset %>%
    group_by(!!sym(Team)) %>%
    summarize(p_value = shapiro_p_value(.data[[Total_Days]]))
  
  # Print normality results to debug
  print("Normality Test Results by Team:")
  print(normality)
  
  levene_Test <- leveneTest(dataset[[Total_Days]], dataset[[Team]])
  
  # Print Levene's Test results to debug
  print("Levene's Test Results:")
  print(levene_Test)
  
  # Check for length zero and adjust the condition
  if (nrow(normality) > 0 && all(normality$p_value > 0.05, na.rm = TRUE) && levene_Test$p.value > 0.05) {
    print("Assumptions for ANOVA are met. Performing ANOVA...")
    formula_str <- paste0("`", Total_Days, "` ~ `", Team, "`")
    print(paste("ANOVA Formula:", formula_str))  # Debugging statement
    anova_result <- aov(as.formula(formula_str), data = dataset)
    print("ANOVA Results:")
    print(summary(anova_result))
  } else {
    print("Assumptions for ANOVA are not met. Performing Kruskal-Wallis test...")
    formula_str <- paste0("`", Total_Days, "` ~ `", Team, "`")
    print(paste("Kruskal-Wallis Formula:", formula_str))  # Debugging statement
    kruskal_result <- kruskal.test(as.formula(formula_str), data = dataset)
    print("Kruskal-Wallis Test Results:")
    print(kruskal_result)
    
    # Perform pairwise comparisons with Benjamini-Hochberg adjustment
    pairwise_results <- pairwise.wilcox.test(dataset[[Total_Days]], dataset[[Team]], p.adjust.method = "BH", exact = FALSE)
    print("Pairwise Comparisons (Wilcoxon Test) Results with Benjamini-Hochberg Adjustment:")
    print(pairwise_results$p.value)
  }
  
  # Summary statistics for each team
  summary_stats <- dataset %>%
    group_by(!!sym(Team)) %>%
    summarize(
      count = n(),
      mean = mean(.data[[Total_Days]], na.rm = TRUE),
      median = median(.data[[Total_Days]], na.rm = TRUE),
      sd = sd(.data[[Total_Days]], na.rm = TRUE)
    )
  
  # Print summary statistics
  print("Summary Statistics for Each Team:")
  print(summary_stats)
}

# Example call to the function with the dataset
Check_Anova_Assumptions(dataset, "Team", "Total Days")

