# Load required libraries
library(tidyverse)
library(ggplot2)


# data loading and cleaning

df <- read.csv("~/Documents/NCAIR/DSB/Student Metadata.csv", stringsAsFactors = FALSE)
head(df)
# Initial data size
cat("\nInitial dataset dimensions:", nrow(df), "rows,", ncol(df), "columns\n")

df_clean <- df %>%
  # cleaning the variable name
  janitor::clean_names() %>%
  mutate(
    # Clean gender 
    gender = str_trim(gender),
    gender = ifelse(str_to_lower(gender) %in% c("male", "m", "make"), "Male",
                    ifelse(str_to_lower(gender) %in% c("female", "f"), "Female", NA)),
    
    # Clean CGPA - extract numeric values 
    cgpa_clean = as.numeric(str_extract(cgpa, "^[0-9]+\\.[0-9]+")),
    
    # Clean level of understanding
    level = as.numeric(level_of_understanding_of_current_course),
    
    # Clean best programming language 
    best_language = case_when(
      str_to_lower(str_trim(best_language)) %in% c("python", "python ") ~ "Python",
      str_to_lower(str_trim(best_language)) %in% c("java", "java ") ~ "Java",
      str_to_lower(str_trim(best_language)) == "r" ~ "R",
      TRUE ~ "Other"
    )
  ) %>% 
  # Remove duplicates (keep first occurrence)
  distinct(first_name, last_name, email, .keep_all = TRUE) %>%
  # Filter valid data for Assignment Two
  filter(!is.na(level), !is.na(cgpa_clean), level %in% 1:5) %>%
  mutate(level = factor(level, levels = 1:5, 
                        labels = c("Very Low", "Low", "Medium", "High", "Very High")))
# Report duplicates
cat("Duplicates removed:", nrow(df) - nrow(distinct(df, First.Name, Last.Name, Email,)), "\n\n")

# Filter  (non-NA gender and language)
df_lang <- df_clean %>%
  filter(!is.na(gender), !is.na(best_language))




# PART TWO: CGPA vs COURSE UNDERSTANDING

cat("PART TWO: Relationship between CGPA and Course Understanding\n")
# Boxplot 
p1 <- ggplot(df_clean, aes(x = level, y = cgpa_clean, fill = level)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "darkgreen") +
  stat_summary(fun = mean, geom = "text", aes(label = round(after_stat(y), 2)), 
               vjust = -1, size = 3) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "CGPA Distribution by Course Understanding Level",
       x = "Level of Understanding", y = "CGPA") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

# Bar Chart of Average CGPA
avg_cgpa <- df_clean %>%
  group_by(level) %>%
  summarise(
    avg_cgpa = mean(cgpa_clean, na.rm = TRUE),
    count = n()
  )

p2 <- ggplot(avg_cgpa, aes(x = level, y = avg_cgpa, fill = level)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste("CGPA:", round(avg_cgpa, 2), "\n(n=", count, ")")), 
            vjust = -0.5, size = 3) +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Average CGPA by Understanding Level",
       x = "Level of Understanding", y = "Average CGPA") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 5)

print(p2)

#  Statistical Test (ANOVA)
anova_result <- aov(cgpa_clean ~ level, data = df_clean)
cat("\nANOVA Results:\n")
print(summary(anova_result))

cat("\nTukey HSD Post-hoc Test:\n")
print(TukeyHSD(anova_result))

#  Summary Table
summary_table <- df_clean %>%
  group_by(level) %>%
  summarise(
    `Number of Students` = n(),
    `Average CGPA` = round(mean(cgpa_clean), 2),
    `Median CGPA` = round(median(cgpa_clean), 2),
    `Min CGPA` = round(min(cgpa_clean), 2),
    `Max CGPA` = round(max(cgpa_clean), 2)
  )

cat("\nSummary Statistics by Understanding Level:\n")
print(summary_table)

# CONCLUSION FOR ASSIGNMENT TWO
overall_avg <- mean(df_clean$cgpa_clean, na.rm = TRUE)

cat("\n", paste(rep("-", 60), collapse = ""), "\n")
cat("FINDINGS:\n")
cat("Overall average CGPA:", round(overall_avg, 2), "\n\n")
cat("Average CGPA by understanding level:\n")
for(i in 1:nrow(avg_cgpa)) {
  diff <- avg_cgpa$avg_cgpa[i] - overall_avg
  cat("  ", as.character(avg_cgpa$level[i]), ":", round(avg_cgpa$avg_cgpa[i], 2), 
      ifelse(diff > 0, paste("(+", round(diff, 2), "above average)"), 
             paste("(", round(diff, 2), "below average)")), "\n")
}

p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
cat("\nCONCLUSION:", 
    ifelse(p_value < 0.05, 
           "There IS a significant relationship between course understanding and CGPA (p < 0.05)\n  Students with higher understanding tend to have higher CGPAs",
           "No significant relationship found between course understanding and CGPA\n"))
cat(paste(rep("=", 60), collapse = ""), "\n\n")


# PART THREE: GENDER vs PROGRAMMING LANGUAGE PREFERENCE


cat("PART THREE: Programming Language Preference by Gender\n")

# Calculate percentages
lang_data <- df_lang %>%
  group_by(gender, best_language) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(gender) %>%
  mutate(percent = (count / sum(count)) * 100)

# Display summary
cat("Sample sizes:\n")
df_lang %>% count(gender) %>% print()

cat("\nProgramming language distribution by gender:\n")
lang_data %>%
  select(gender, best_language, count, percent) %>%
  arrange(gender, desc(percent)) %>%
  print()

# stacked bar chart
p3 <- ggplot(lang_data, aes(x = gender, y = percent, fill = best_language)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.9) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Python" = "#306998", 
                               "Java" = "#b07219", 
                               "R" = "#276DC3", 
                               "Other" = "#95a5a6")) +
  labs(title = "Programming Language Preference by Gender",
       subtitle = "Percentage of students who prefer each language",
       x = "Gender", y = "Percentage of Students (%)",
       fill = "Language",
       caption = "Data from student survey") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray50"),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 11),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        # Flip for easier reading
        panel.grid.major.x = element_blank()) + coord_flip()  

print(p3)

# Save the chart
ggsave("language_gender_chart.png", width = 9, height = 5, dpi = 300)
cat("\nChart saved as 'language_gender_chart.png'\n")
