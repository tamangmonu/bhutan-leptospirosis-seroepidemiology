# Leptospirosis seroprevalence and risk factors in subtropical Bhutan, 2024
# Author: Monu Tamang
# Version 2 | Created 5 July 2026 | Last edited 30 September 2026
# Analysis of MAT / ELISA seroprevalence and risk factors for MAT seropositivity.
# Script developed with assistance from Claude (Anthropic).
#
# NOTE: The participant-level dataset is NOT included in this repository.
# Place your own copy at data/LeptoBhutan_data.xlsx (see README.md for the
# expected column names). All outputs are written to the output/ folder.

# ---- Settings ---------------------------------------------------------------
data_file  <- file.path("data", "LeptoBhutan_data.xlsx")
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)
out <- function(file) file.path(output_dir, file)   # helper for export paths

# load necessary packages 

required_pkgs <- c("ggplot2", "dplyr", "readxl", "writexl", "tidyr",
                   "summarytools", "rlang", "purrr", "MASS", "scales", "stringi",
                   "glmnet", "car", "ResourceSelection", "pROC")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

# Load libraries 
library(ggplot2)
library(readxl)
library(writexl)
library(tidyr)
library(summarytools)
library(rlang)
library(purrr)
library(MASS)
library(dplyr) 
library(scales)

# Import data ----
LeptoBhutan <- readxl::read_excel(data_file)
head(LeptoBhutan)
tail(LeptoBhutan)

#Brief Data Visualization ---- 
names(LeptoBhutan)       # Check column names
head(LeptoBhutan)        # View first few rows
summary(LeptoBhutan)     # Summary statistics
str(LeptoBhutan)         # Structure of the data

#Basic bar plot – Seroprevalence by gender

ggplot(LeptoBhutan, aes(x = LeptoBhutan$`Sex`, fill = factor(LeptoBhutan$`MAT_Result`))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Seropositive Status") +
  ggtitle("Seroprevalence by Gender") +
  theme_minimal()

#Seroprevalance by age groups

LeptoBhutan$AgeGroup <- cut(
  LeptoBhutan$`Age`,
  breaks = c(18, 31, 41, 51, Inf),
  labels = c("18–30", "31–40", "41–50", "51+"),
  right = FALSE
)

# Get the count of participants in each age group
age_group_counts <- table(LeptoBhutan$AgeGroup)
print(age_group_counts)

# Convert AgeGroup to factor
AgeGroup <- factor(LeptoBhutan$AgeGroup)

scale_fill_manual(values = c("0" = "lightgray", "1" = "steelblue"))

ggplot(LeptoBhutan, aes(x = AgeGroup, fill = factor(`MAT_Result`, labels = c("Negative", "Positive")))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Seroprevalence by Age Group",
    x = "Age Group",
    y = "Percent",
    fill = "Seropositive"
  ) +
  scale_fill_manual(
    values = c("Negative" = "lightgray", "Positive" = "steelblue")
  ) +
  theme_minimal()

# reflect the percentages on the graph 
# Calculate proportions
label_data <- LeptoBhutan %>%
  group_by(`AgeGroup`, `MAT_Result`) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(`AgeGroup`) %>%
  mutate(group_total = sum(count),
         unique_results = n_distinct(`MAT_Result`),
         prop = count / group_total,
         label = paste0(round(prop * 100), "%"),
         pos = cumsum(prop) - 0.5 * prop) %>%
  # Remove labels where only one result category exists
  filter(unique_results > 1)
# Plot with correct labels
ggplot(LeptoBhutan, aes(x = `AgeGroup`, fill = factor(`MAT_Result`))) +
  geom_bar(position = "fill") +
  geom_text(data = label_data,
            aes(x = `AgeGroup`, y = pos, label = label, group = `MAT_Result`),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(y = "Proportion", x = "Age Group", fill = "Seropositive Status") +
  ggtitle("Seroprevalence by Age Group") +
  theme_minimal()

#Socio-demographic data visulaization
library(summarytools)
#Summarize numeric variable (Age)
# Load required package
library(dplyr)
# Age summary
age_stats <- LeptoBhutan %>%
  summarize(
    Mean = mean(Age, na.rm = TRUE),
    SD = sd(Age, na.rm = TRUE),
    Min = min(Age, na.rm = TRUE),
    Max = max(Age, na.rm = TRUE),
    N = sum(!is.na(Age)),
    SE = SD / sqrt(N),  # Standard Error
    CI_lower = Mean - qt(0.975, df = N - 1) * SE,
    CI_upper = Mean + qt(0.975, df = N - 1) * SE
  )

# View result
print(age_stats)

# Frequency table for Settlement
settlement_table <- table(LeptoBhutan$`Settlement`)
print(settlement_table)

# Frequency table for Level of Education
education_table <- table(LeptoBhutan$`Level_of_Education`)
print(education_table)

# Frequency table for Occupation
occupation_table <- table(LeptoBhutan$`Occupation_Group`)
print(occupation_table)

#Bar Graphs for sociodemographic data 
library(ggplot2)

# Settlement Disctributions 
settlement_df <- as.data.frame(settlement_table)
colnames(settlement_df) <- c("Settlement", "Count")
#Bar Graph for types of settlement 
# Plot with smaller count labels
ggplot(settlement_df, aes(x = reorder(Settlement, -Count), y = Count, fill = Settlement)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, size = 4) +  # reduced from 5 to 4
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution of Settlement Types", x = "Settlement", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

#education level 
# Create data frame from table
education_df <- as.data.frame(education_table)
colnames(education_df) <- c("Education", "Count")

# Set specific order for Education levels
education_df$Education <- factor(education_df$Education, 
                                 levels = c("Cannot read and write", 
                                            "Primary (up to class 6)", 
                                            "Secondary (Class 7 to 12)", 
                                            "Higher Education"))

# Plot
ggplot(education_df, aes(x = Education, y = Count, fill = Education)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, color = "black", size = 4) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Distribution of Education Levels", 
       x = "Level of Education", 
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  expand_limits(y = max(education_df$Count) + 20)

# Occupation
# Create data frame from table
occupation_table <- table(LeptoBhutan$Occupation_Group)
occupation_df <- as.data.frame(occupation_table)
colnames(occupation_df) <- c("Occupation", "Count")
# Reorder factor levels so that "Others" appears last
other_levels <- setdiff(occupation_df$Occupation, "Others")
# Sort them by Count in descending order
ordered_levels <- occupation_df[occupation_df$Occupation %in% other_levels, ]
ordered_levels <- ordered_levels[order(-ordered_levels$Count), "Occupation"]
# Append "Others" at the end
final_levels <- c(as.character(ordered_levels), "Others")
# Set factor levels in desired order
occupation_df$Occupation <- factor(occupation_df$Occupation, levels = final_levels)
# Plot
ggplot(occupation_df, aes(x = Occupation, y = Count, fill = Occupation)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3, color = "black", size = 4) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Distribution of Occupations", x = "Occupation", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  expand_limits(y = max(occupation_df$Count) + 20)

#Sociodemographic characteristics ----
# Step 1: One generic function for categorical summaries
summarize_var <- function(data, var_name, domain) {
  var_sym <- sym(var_name)
  
  data %>%
    filter(!is.na(!!var_sym)) %>%
    count(!!var_sym) %>%
    mutate(
      percent = round(100 * n / sum(n), 1),
      summary = paste0(n, " (", percent, "%)"),
      Variable = var_name,
      Domain = domain
    ) %>%
    rename(Category = !!var_sym) %>%
    dplyr::select(Domain, Variable, Category, n, percent, summary)
}

# Step 2: Create Age_group once, reuse everywhere (avoids duplicating AgeGroup logic)
LeptoBhutan <- LeptoBhutan %>%
  mutate(Age_group = case_when(
    Age >= 18 & Age <= 30 ~ "18–30",
    Age >= 31 & Age <= 40 ~ "31–40",
    Age >= 41 & Age <= 50 ~ "41–50",
    Age > 50              ~ "51+",
    TRUE                  ~ NA_character_
  ))

# Step 3: Create Animal_Contact_Category once
animal_vars <- c("Contact_Rat", "Contact_Rabbit", "Contact_Bat", 
                 "Contact_Goat", "Contact_Cattle", "Contact_Dog", 
                 "Contact_Cat", "Contact_Pig")

LeptoBhutan <- LeptoBhutan %>%
  mutate(
    Animal_Contact_Count = rowSums(across(all_of(animal_vars)) == "Yes", na.rm = TRUE),
    Animal_Contact_Category = case_when(
      Animal_Contact_Count == 0 ~ "No contact",
      Animal_Contact_Count == 1 ~ "Contact with 1 animal",
      Animal_Contact_Count > 1  ~ "Contact with >1 animals"
    )
  )

# Step 4: Define variable groups with domain labels
sociodem_vars <- c("Sex", "Age_group", "Level_of_Education", "Occupation_Group", "Settlement")

symptom_vars <- c("Symp_Fever", "Symp_Headache", "Symp_Myalgia", "Symp_Arthalgia", 
                  "Symp_Vomiting", "Symp_Diarrhea", "Symp_stomache", "Symp_jaundice",
                  "Skin_injuries", "Fever_12months", "Hospitalized_12months")

risk_vars <- c("Animal_Contact_Category", "Garbage_facility", "Open_defecation",
               "Batinfested_cave", "Marshy_land", "Contact_stagnantwater", "Flood_exposure",
               "Paddy_Plantation", "Fishery_farming", "Drink_pondwater", "Drink_lakewater",
               "Drink_unboiled", "Visit_cave", "Walk_barefoot", "Visit_forest", 
               "Contact_deadanimals", "River_bath", "Pond_bath", "Lake_bath")

# Step 5: Apply summarize_var across all groups, tagging domain
all_summary <- bind_rows(
  lapply(sociodem_vars, function(v) summarize_var(LeptoBhutan, v, "Sociodemographic")),
  lapply(symptom_vars,  function(v) summarize_var(LeptoBhutan, v, "Symptoms/History")),
  lapply(risk_vars,     function(v) summarize_var(LeptoBhutan, v, "Risk Factor"))
)

# Step 6: Print and export — everything in one sheet
print(all_summary)
write_xlsx(all_summary, out("Sociodemographic_summary.xlsx"))

#Sero-positive in ELISA & MAT ----
# Step 1: Derive test results directly from LeptoBhutan
test_results <- bind_rows(
  LeptoBhutan %>% count(Result = ELISA_IgM)  %>% mutate(Test = "ELISA IgM"),
  LeptoBhutan %>% count(Result = ELISA_IgG)  %>% mutate(Test = "ELISA IgG"),
  LeptoBhutan %>% count(Result = MAT_Result) %>% mutate(Test = "MAT")
) %>%
  rename(Count = n) %>%
  mutate(Result = factor(Result, levels = c("Positive", "Negative", "Equivocal"))) %>%
  dplyr::select(Test, Result, Count)

# Step 2: Totals, proportions, 95% CI (Wilson via prop.test)
test_results <- test_results %>%
  group_by(Test) %>%
  mutate(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(
    Proportion = Count / Total,
    CI = purrr::map2(Count, Total, ~prop.test(.x, .y)$conf.int),
    CI_Lower = purrr::map_dbl(CI, 1),
    CI_Upper = purrr::map_dbl(CI, 2)
  )

# Step 3: Format into a clean summary table
serology_summary <- test_results %>%
  transmute(
    Test,
    Category = Result,
    n = Count,
    Total,
    percent = round(Proportion * 100, 1),
    CI_Lower = round(CI_Lower * 100, 1),
    CI_Upper = round(CI_Upper * 100, 1),
    summary = paste0(n, " (", percent, "%)")
  )

# Step 4: Export to its OWN dedicated Excel file
write_xlsx(serology_summary, out("Serology_summary.xlsx"))

print(serology_summary)

# Step 5: Plot 1 — all result categories with CI, faceted by test
ggplot(test_results, aes(x = Result, y = Proportion, fill = Result)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0.2, position = position_dodge(0.7)) +
  facet_wrap(~ Test) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proportion of Test Results with 95% CI",
       x = "Result Category", y = "Proportion (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 6: Plot 2 — positive-only seroprevalence across tests
positive_data <- test_results %>%
  filter(Result == "Positive") %>%
  mutate(Percent_Label = paste0(round(Proportion * 100, 1), "%"))

ggplot(positive_data, aes(x = Test, y = Proportion, fill = Test)) +
  geom_bar(stat = "identity", width = 0.6, color = NA) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_text(aes(y = 0, label = Percent_Label), vjust = -0.5, size = 4.5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(
    title = "Seroprevalence of Leptospira Spp. by ELISA (IgM & IgG) and MAT, N = 492",
    x = "Test",
    y = "Proportion Positive (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Serovar distribution ---- 
# Step 1: Clean the Serovar column (blank -> NA)
LeptoBhutan$Serovar <- ifelse(LeptoBhutan$Serovar == "" | is.na(LeptoBhutan$Serovar), 
                              NA, LeptoBhutan$Serovar)

# Step 2: Count and calculate proportions for identified serovars
serovar_summary <- LeptoBhutan %>%
  filter(!is.na(Serovar)) %>%
  count(Serovar) %>%
  arrange(desc(n)) %>%
  mutate(
    Total = sum(n),
    percent = round(100 * n / Total, 1),
    summary = paste0(n, " (", percent, "%)")
  ) %>%
  rename(Count = n) %>%
  dplyr::select(Serovar, Count, Total, percent, summary)

# Step 3: Check total matches expected MAT-positive count
print(paste("Total serovar-typed samples:", sum(serovar_summary$Count)))

# Step 4: View and export
print(serovar_summary)
write_xlsx(serovar_summary, out("Serovar_summary.xlsx"))



#Univariate Logistic Regression (Wald) ----
# Step 0: Recode MAT_Result to binary and factor
LeptoBhutan$MAT_bin <- ifelse(LeptoBhutan$MAT_Result == "Positive", 1, 0)
LeptoBhutan$MAT_Result <- factor(LeptoBhutan$MAT_Result, levels = c("Negative", "Positive"))

# Step 1: Create sociodemographic variables
LeptoBhutan$AgeGroup <- cut(
  LeptoBhutan$Age,
  breaks = c(17, 30, 40, 50, Inf),
  labels = c("18-30", "31-40", "41-50", "51+"),
  right = TRUE, include.lowest = TRUE
)
LeptoBhutan$AgeGroup <- factor(LeptoBhutan$AgeGroup)
LeptoBhutan$Sex <- factor(LeptoBhutan$Sex, levels = c("Female", "Male"))   # reference: Female
LeptoBhutan$Occupation_Group <- factor(LeptoBhutan$Occupation_Group)

# Reference categories (as reported in the manuscript, Table 4):
#   Settlement         -> Rural
#   Level of education -> Secondary (Class 7 to 12)
# Without this, R would use the first level alphabetically
# ("Crowded urban area" and "Cannot read and write") as the reference.
set_reference <- function(x, ref) {
  x <- factor(x)
  if (!ref %in% levels(x)) {
    stop("Reference level '", ref, "' not found. Levels present: ",
         paste(levels(x), collapse = " | "))
  }
  relevel(x, ref = ref)
}
ref_settlement <- "Rural"
ref_education  <- "Secondary (Class 7 to 12)"

LeptoBhutan$Settlement         <- set_reference(LeptoBhutan$Settlement, ref_settlement)
LeptoBhutan$Level_of_Education <- set_reference(LeptoBhutan$Level_of_Education, ref_education)

# Step 2: Recode animal contact into 3 categories
animal_vars <- c("Contact_Rat", "Contact_Rabbit", "Contact_Bat",
                 "Contact_Goat", "Contact_Cattle", "Contact_Dog",
                 "Contact_Cat", "Contact_Pig")

LeptoBhutan <- LeptoBhutan %>%
  mutate(
    Animal_Contact_Count = rowSums(across(all_of(animal_vars)) == "Yes", na.rm = TRUE),
    Animal_Contact_Group = case_when(
      Animal_Contact_Count == 0 ~ "No contact",
      Animal_Contact_Count == 1 ~ "Contact with 1 animal",
      Animal_Contact_Count > 1  ~ "Contact with >1 animals"
    )
  )

LeptoBhutan$Animal_Contact_Group <- factor(
  LeptoBhutan$Animal_Contact_Group,
  levels = c("No contact", "Contact with 1 animal", "Contact with >1 animals")
)

# Step 3: Define variables for univariate regression
sociodem_vars <- c("AgeGroup", "Sex", "Settlement", "Level_of_Education", "Occupation_Group")
binary_vars <- c("Symp_Fever", "Symp_Headache", "Symp_Myalgia", "Symp_Arthalgia",
                 "Symp_Vomiting", "Symp_Diarrhea", "Symp_stomache", "Symp_jaundice",
                 "Animal_Contact_Group", "Garbage_facility", "Open_defecation",
                 "Batinfested_cave", "Marshy_land", "Contact_stagnantwater", "Flood_exposure",
                 "Paddy_Plantation", "Fishery_farming", "Drink_pondwater", "Drink_lakewater",
                 "Drink_unboiled", "Visit_cave", "Walk_barefoot", "Visit_forest", "Contact_deadanimals",
                 "River_bath", "Pond_bath", "Lake_bath", "Skin_injuries", "Fever_12months",
                 "Hospitalized_12months")
all_vars <- c(sociodem_vars, binary_vars)

# Step 4: Univariate logistic regression function (true Wald CI + Wald p-value)
univariate_logistic <- function(var, data) {
  if (!var %in% names(data)) return(NULL)
  
  data_sub <- data[complete.cases(data[[var]], data$MAT_Result), ]
  if (nrow(data_sub) == 0) return(NULL)
  if (length(unique(data_sub[[var]])) < 2) return(NULL)
  
  table_var <- table(data_sub[[var]], data_sub$MAT_Result)
  if (!"Positive" %in% colnames(table_var)) return(NULL)
  prop_var <- prop.table(table_var, 1) * 100
  
  model_var <- glm(as.formula(paste("MAT_bin ~", var)), data = data_sub, family = binomial)
  
  or <- exp(coef(model_var))
  ci <- exp(confint.default(model_var))   # corrected: true Wald CI
  wald_p <- summary(model_var)$coefficients[, "Pr(>|z|)"]
  
  levels_var <- rownames(table_var)
  
  data.frame(
    Variable = var,
    Category = levels_var,
    Total = rowSums(table_var),
    Positive = table_var[, "Positive"],
    Percentage_Positive = round(prop_var[, "Positive"], 1),
    OR = c("Ref", round(or[-1], 2)),
    CI_Lower = c(NA, round(ci[-1, 1], 2)),
    CI_Upper = c(NA, round(ci[-1, 2], 2)),
    P_value = c(NA, signif(wald_p[-1], 3)),
    row.names = NULL
  )
}

# Step 5: Loop through all variables
summary_list <- lapply(all_vars, function(v) univariate_logistic(v, LeptoBhutan))
final_univariate <- do.call(rbind, summary_list)

# Step 6: Export to its own dedicated Excel file
write_xlsx(final_univariate, out("Univariate_Logistic_Regression_Wald.xlsx"))

# Step 7: View the summary
print(final_univariate)

#Multivariable analysis (included LASSO) ---- 

library(glmnet)

# Step 0: Create binary outcome
LeptoBhutan$MAT_bin <- ifelse(LeptoBhutan$MAT_Result == "Positive", 1, 0)

# Step 0b: Ensure all categorical variables used are factors
LeptoBhutan <- LeptoBhutan %>%
  mutate(
    Sex = factor(Sex, levels = c("Female", "Male")),
    Settlement = set_reference(Settlement, ref_settlement),                 # reference: Rural
    Level_of_Education = set_reference(Level_of_Education, ref_education),  # reference: Secondary
    Paddy_Plantation = factor(Paddy_Plantation),
    Drink_unboiled = factor(Drink_unboiled),
    Contact_deadanimals = factor(Contact_deadanimals),
    Contact_stagnantwater = factor(Contact_stagnantwater),
    Skin_injuries = factor(Skin_injuries),
    Fever_12months = factor(Fever_12months),
    Hospitalized_12months = factor(Hospitalized_12months)
  )

# Confirm reference categories (first level listed = reference)
cat("Settlement levels:", levels(LeptoBhutan$Settlement), "\n")
cat("Education levels:", levels(LeptoBhutan$Level_of_Education), "\n")

# Candidate pool: univariate p < 0.20 ONLY
candidate_vars_p02 <- c(
  "Sex", "Settlement", "Level_of_Education", "Contact_stagnantwater",
  "Paddy_Plantation", "Drink_unboiled", "Contact_deadanimals",
  "Skin_injuries", "Fever_12months", "Hospitalized_12months"
)

# MODEL 1: Stepwise selection, BOTH directions (p<0.20 pool)

model_data_m1 <- LeptoBhutan %>%
  dplyr::select(MAT_bin, all_of(candidate_vars_p02)) %>%
  na.omit()

cat("Model 1 sample size (complete cases):", nrow(model_data_m1), "\n")

formula_full_m1 <- as.formula(paste("MAT_bin ~", paste(candidate_vars_p02, collapse = " + ")))
model_full_m1 <- glm(formula_full_m1, data = model_data_m1, family = binomial)
model_both_m1 <- stepAIC(model_full_m1, direction = "both", trace = TRUE)

final_coef_m1 <- summary(model_both_m1)$coefficients
OR <- exp(final_coef_m1[, "Estimate"]); CI <- exp(confint.default(model_both_m1)); P_value <- final_coef_m1[, "Pr(>|z|)"]
final_table_m1 <- data.frame(Variable = rownames(final_coef_m1), OR = round(OR,2),
                             CI_Lower = round(CI[,1],2), CI_Upper = round(CI[,2],2), P_value = signif(P_value,3), row.names = NULL) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(OR_95CI = paste0(OR, " (", CI_Lower, "–", CI_Upper, ")")) %>%
  dplyr::select(Variable, OR_95CI, P_value)

print(final_table_m1)
write_xlsx(final_table_m1, out("Leptospirosis_Adjusted_Logistic_Regression_Model1.xlsx"))

# MODEL 2: Full model, no selection (p<0.20 pool)

model_data_m2 <- model_data_m1
model_full_m2 <- glm(formula_full_m1, data = model_data_m2, family = binomial)

final_coef_m2 <- summary(model_full_m2)$coefficients
OR <- exp(final_coef_m2[, "Estimate"]); CI <- exp(confint.default(model_full_m2)); P_value <- final_coef_m2[, "Pr(>|z|)"]
final_table_m2 <- data.frame(Variable = rownames(final_coef_m2), OR = round(OR,2),
                             CI_Lower = round(CI[,1],2), CI_Upper = round(CI[,2],2), P_value = signif(P_value,3), row.names = NULL) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(OR_95CI = paste0(OR, " (", CI_Lower, "–", CI_Upper, ")")) %>%
  dplyr::select(Variable, OR_95CI, P_value)

print(final_table_m2)
write_xlsx(final_table_m2, out("Leptospirosis_Adjusted_Logistic_Regression_Model2.xlsx"))

# MODEL 3: Backward + Sociodemographic (Sex, Settlement, Education) forced-in

socio_vars_m3 <- c("Sex", "Settlement", "Level_of_Education")
other_vars_m3 <- setdiff(candidate_vars_p02, socio_vars_m3)
all_vars_m3 <- c(socio_vars_m3, other_vars_m3)

model_data_m3 <- LeptoBhutan %>%
  dplyr::select(MAT_bin, all_of(all_vars_m3)) %>%
  na.omit()

formula_all_m3 <- as.formula(paste("MAT_bin ~", paste(all_vars_m3, collapse = " + ")))
model_full_m3 <- glm(formula_all_m3, data = model_data_m3, family = binomial)

model_combined_m3 <- stepAIC(
  model_full_m3,
  scope = list(lower = as.formula(paste("MAT_bin ~", paste(socio_vars_m3, collapse = " + "))),
               upper = formula_all_m3),
  direction = "backward", trace = TRUE
)

final_coef_m3 <- summary(model_combined_m3)$coefficients
OR <- exp(final_coef_m3[, "Estimate"]); CI <- exp(confint.default(model_combined_m3)); P_value <- final_coef_m3[, "Pr(>|z|)"]
final_table_m3 <- data.frame(Variable = rownames(final_coef_m3), OR = round(OR,2),
                             CI_Lower = round(CI[,1],2), CI_Upper = round(CI[,2],2), P_value = signif(P_value,3), row.names = NULL) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(OR_95CI = paste0(OR, " (", CI_Lower, "–", CI_Upper, ")")) %>%
  dplyr::select(Variable, OR_95CI, P_value)

print(final_table_m3)
write_xlsx(final_table_m3, out("Leptospirosis_Adjusted_Logistic_Regression_Model3.xlsx"))

# MODEL 4: Backward elimination (p<0.20 pool)

model_data_m4 <- model_data_m1
model_full_m4 <- glm(formula_full_m1, data = model_data_m4, family = binomial)
model_backward_m4 <- stepAIC(model_full_m4, direction = "backward", trace = TRUE)

final_coef_m4 <- summary(model_backward_m4)$coefficients
OR <- exp(final_coef_m4[, "Estimate"]); CI <- exp(confint.default(model_backward_m4)); P_value <- final_coef_m4[, "Pr(>|z|)"]
final_table_m4 <- data.frame(Variable = rownames(final_coef_m4), OR = round(OR,2),
                             CI_Lower = round(CI[,1],2), CI_Upper = round(CI[,2],2), P_value = signif(P_value,3), row.names = NULL) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(OR_95CI = paste0(OR, " (", CI_Lower, "–", CI_Upper, ")")) %>%
  dplyr::select(Variable, OR_95CI, P_value)

print(final_table_m4)
write_xlsx(final_table_m4, out("Leptospirosis_Adjusted_Logistic_Regression_Model4.xlsx"))


# MODEL 5: LASSO-selected variables, refit as unpenalized GLM

model_data_m5 <- model_data_m1  # same p<0.20 pool, same complete cases

# Build design matrix (dummy-coded factors, no intercept column needed for glmnet)
x_matrix <- model.matrix(formula_full_m1, data = model_data_m5)[, -1]  # drop intercept col
y_vector <- model_data_m5$MAT_bin

# Cross-validated LASSO to choose penalty (lambda)
set.seed(123)  # for reproducibility of the CV folds
cv_lasso <- cv.glmnet(x_matrix, y_vector, family = "binomial", alpha = 1, nfolds = 10)

# lambda.1se = more conservative (fewer variables); lambda.min = best CV fit
best_lambda <- cv_lasso$lambda.1se
cat("Chosen LASSO lambda (1se):", best_lambda, "\n")

# Extract non-zero coefficients at chosen lambda
lasso_coefs <- coef(cv_lasso, s = best_lambda)
selected_terms <- rownames(lasso_coefs)[which(lasso_coefs[, 1] != 0)]
selected_terms <- setdiff(selected_terms, "(Intercept)")
cat("LASSO-selected terms:\n")
print(selected_terms)

# Map selected dummy-coded terms back to original variable names for refitting
selected_vars_m5 <- candidate_vars_p02[sapply(candidate_vars_p02, function(v) any(grepl(paste0("^", v), selected_terms)))]
cat("LASSO-selected variables (original names):\n")
print(selected_vars_m5)

# Refit selected variables as a standard unpenalized GLM for interpretable OR/CI/p-values
formula_m5 <- as.formula(paste("MAT_bin ~", paste(selected_vars_m5, collapse = " + ")))
model_lasso_m5 <- glm(formula_m5, data = model_data_m5, family = binomial)

final_coef_m5 <- summary(model_lasso_m5)$coefficients
OR <- exp(final_coef_m5[, "Estimate"]); CI <- exp(confint.default(model_lasso_m5)); P_value <- final_coef_m5[, "Pr(>|z|)"]
final_table_m5 <- data.frame(Variable = rownames(final_coef_m5), OR = round(OR,2),
                             CI_Lower = round(CI[,1],2), CI_Upper = round(CI[,2],2), P_value = signif(P_value,3), row.names = NULL) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(OR_95CI = paste0(OR, " (", CI_Lower, "–", CI_Upper, ")")) %>%
  dplyr::select(Variable, OR_95CI, P_value)

print(final_table_m5)
write_xlsx(final_table_m5, out("Leptospirosis_Adjusted_Logistic_Regression_Model5_LASSO.xlsx"))


# AIC COMPARISON ACROSS ALL 5 MODELS

model_names <- c(
  "Model 1: Stepwise both-direction (p<0.20)",
  "Model 2: Full (p<0.20, no selection)",
  "Model 3: Backward + Sociodemographic forced-in",
  "Model 4: Backward (p<0.20)",
  "Model 5: LASSO-selected (refit unpenalized)"
)
model_list <- list(model_both_m1, model_full_m2, model_combined_m3, model_backward_m4, model_lasso_m5)
N_values <- sapply(list(model_data_m1, model_data_m2, model_data_m3, model_data_m4, model_data_m5), nrow)
AIC_values <- sapply(model_list, AIC)

AIC_table <- data.frame(Model = model_names, N = N_values, AIC = round(AIC_values, 2))
print(AIC_table)
write_xlsx(AIC_table, out("Model_AIC_Comparison.xlsx"))


#Diagnostic for Model 3 ---- 
# Model Diagnostics: Final Model (Model 3)

library(car)
library(ResourceSelection)
library(pROC)
library(writexl)

final_model <- model_combined_m3   # Model 3: Backward + Sociodemographic forced-in

# 1. Multicollinearity: Variance Inflation Factors (VIF)

vif_values <- car::vif(final_model)
print(vif_values)

# For factors with >2 levels, car::vif() returns GVIF and GVIF^(1/(2*Df))
# Use the adjusted column for interpretation
vif_df <- as.data.frame(vif_values)
vif_df$Variable <- rownames(vif_df)
write_xlsx(vif_df, out("Model3_Diagnostics_VIF.xlsx"))


# 2. Goodness-of-fit: Hosmer-Lemeshow test

hl_test <- ResourceSelection::hoslem.test(final_model$y, fitted(final_model), g = 10)
print(hl_test)

hl_summary <- data.frame(
  Chi_square = round(hl_test$statistic, 3),
  df = hl_test$parameter,
  P_value = round(hl_test$p.value, 4)
)
print(hl_summary)


# 3. Discriminatory ability: AUC / ROC curve

roc_obj <- pROC::roc(final_model$y, fitted(final_model))
auc_value <- pROC::auc(roc_obj)
auc_ci <- pROC::ci.auc(roc_obj)

cat("AUC:", round(auc_value, 3), "\n")
cat("95% CI:", round(auc_ci[1], 3), "-", round(auc_ci[3], 3), "\n")

plot(roc_obj, main = paste0("ROC Curve for Final Model (Model 3), AUC = ", round(auc_value, 3)))

auc_summary <- data.frame(
  AUC = round(as.numeric(auc_value), 3),
  CI_Lower = round(auc_ci[1], 3),
  CI_Upper = round(auc_ci[3], 3)
)
print(auc_summary)


# 4. Influential observations: standardized residuals, leverage, Cook's distance

influence_stats <- data.frame(
  Row = 1:length(rstandard(final_model)),
  Std_Residual = rstandard(final_model),
  Leverage = hatvalues(final_model),
  Cooks_D = cooks.distance(final_model)
)

influence_stats$Flag_Residual <- abs(influence_stats$Std_Residual) > 2
influence_stats$Flag_CooksD <- influence_stats$Cooks_D > (4 / nrow(influence_stats))

n_flagged_resid <- sum(influence_stats$Flag_Residual)
n_flagged_cooksd <- sum(influence_stats$Flag_CooksD)

cat("Observations with |standardized residual| > 2:", n_flagged_resid, "\n")
cat("Observations with high Cook's distance (> 4/n):", n_flagged_cooksd, "\n")

# Visual diagnostic plots
plot(final_model, which = c(4, 5))

# Export full influence diagnostics table
write_xlsx(influence_stats, out("Model3_Diagnostics_Influence.xlsx"))


# 5. Combined diagnostics summary (one clean file)

diagnostics_summary <- data.frame(
  Test = c("Hosmer-Lemeshow Chi-square", "Hosmer-Lemeshow df", "Hosmer-Lemeshow P-value",
           "AUC", "AUC 95% CI Lower", "AUC 95% CI Upper",
           "N flagged (|std resid| > 2)", "N flagged (high Cook's D)"),
  Value = c(round(hl_test$statistic, 3), hl_test$parameter, round(hl_test$p.value, 4),
            round(as.numeric(auc_value), 3), round(auc_ci[1], 3), round(auc_ci[3], 3),
            n_flagged_resid, n_flagged_cooksd)
)

print(diagnostics_summary)
write_xlsx(diagnostics_summary, out("Model3_Diagnostics_Summary.xlsx"))


#Sensitivity Analysis: Model 3 excluding influential observations ----

library(dplyr)
library(writexl)

# Step 1: Identify rows to exclude (using the same influence_stats object
# and thresholds from the diagnostics code)
influential_rows <- which(influence_stats$Flag_CooksD)
cat("Number of influential observations to exclude:", length(influential_rows), "\n")

# Step 2: Create a sensitivity dataset excluding these rows
model_data_m3_sens <- model_data_m3[-influential_rows, ]
cat("Original N:", nrow(model_data_m3), " | Sensitivity N:", nrow(model_data_m3_sens), "\n")

# Step 3: Refit Model 3's exact formula on the reduced dataset
formula_m3 <- formula(model_combined_m3)
model_m3_sensitivity <- glm(formula_m3, data = model_data_m3_sens, family = binomial)

# Step 4: Extract OR/CI/p-values for the sensitivity model
final_coef_sens <- summary(model_m3_sensitivity)$coefficients
OR_sens <- exp(final_coef_sens[, "Estimate"])
CI_sens <- exp(confint.default(model_m3_sensitivity))
P_sens <- final_coef_sens[, "Pr(>|z|)"]

sensitivity_table <- data.frame(
  Variable = rownames(final_coef_sens),
  OR_sensitivity = round(OR_sens, 2),
  CI_Lower_sensitivity = round(CI_sens[, 1], 2),
  CI_Upper_sensitivity = round(CI_sens[, 2], 2),
  P_value_sensitivity = signif(P_sens, 3),
  row.names = NULL
) %>%
  filter(Variable != "(Intercept)")

# Step 5: Extract the ORIGINAL Model 3 table for direct comparison
final_coef_orig <- summary(model_combined_m3)$coefficients
OR_orig <- exp(final_coef_orig[, "Estimate"])
CI_orig <- exp(confint.default(model_combined_m3))
P_orig <- final_coef_orig[, "Pr(>|z|)"]

original_table <- data.frame(
  Variable = rownames(final_coef_orig),
  OR_original = round(OR_orig, 2),
  CI_Lower_original = round(CI_orig[, 1], 2),
  CI_Upper_original = round(CI_orig[, 2], 2),
  P_value_original = signif(P_orig, 3),
  row.names = NULL
) %>%
  filter(Variable != "(Intercept)")

# Step 6: Merge original vs sensitivity side by side
comparison_table <- original_table %>%
  left_join(sensitivity_table, by = "Variable") %>%
  mutate(
    OR_pct_change = round(100 * (OR_sensitivity - OR_original) / OR_original, 1),
    Direction_change_flag = sign(log(OR_original)) != sign(log(OR_sensitivity))
  )

print(comparison_table)
write_xlsx(comparison_table, out("Model3_Sensitivity_Analysis_Comparison.xlsx"))

# Step 7: Quick summary check - did any variable change significance status?
comparison_table <- comparison_table %>%
  mutate(
    Significant_original = P_value_original < 0.05,
    Significant_sensitivity = P_value_sensitivity < 0.05,
    Significance_flipped = Significant_original != Significant_sensitivity
  )

cat("\nVariables where statistical significance changed after excluding influential points:\n")
print(comparison_table %>% filter(Significance_flipped) %>% dplyr::select(Variable, P_value_original, P_value_sensitivity))

cat("\nMax absolute % change in OR across all variables:", 
    max(abs(comparison_table$OR_pct_change), na.rm = TRUE), "%\n")


#Checking for sparse cells / quasi-separation (with Excel export) ---- 

library(dplyr)
library(writexl)

# Step 1 & 2: Contingency tables
paddy_full <- as.data.frame.matrix(table(LeptoBhutan$Paddy_Plantation, LeptoBhutan$MAT_Result))
paddy_full$Category <- rownames(paddy_full)

hosp_full <- as.data.frame.matrix(table(LeptoBhutan$Hospitalized_12months, LeptoBhutan$MAT_Result))
hosp_full$Category <- rownames(hosp_full)

# Step 3: Flagged influential observations breakdown
flagged_data <- model_data_m3[influential_rows, ]

paddy_flagged <- as.data.frame.matrix(table(flagged_data$Paddy_Plantation, flagged_data$MAT_bin))
paddy_flagged$Category <- rownames(paddy_flagged)

hosp_flagged <- as.data.frame.matrix(table(flagged_data$Hospitalized_12months, flagged_data$MAT_bin))
hosp_flagged$Category <- rownames(hosp_flagged)

# Step 4 & 5: Summary percentages
paddy_yes_positive_total <- sum(model_data_m3$Paddy_Plantation == "Yes" & model_data_m3$MAT_bin == 1)
paddy_yes_positive_flagged <- sum(flagged_data$Paddy_Plantation == "Yes" & flagged_data$MAT_bin == 1)

hosp_yes_positive_total <- sum(model_data_m3$Hospitalized_12months == "Yes" & model_data_m3$MAT_bin == 1)
hosp_yes_positive_flagged <- sum(flagged_data$Hospitalized_12months == "Yes" & flagged_data$MAT_bin == 1)

summary_table <- data.frame(
  Metric = c("Paddy_Plantation=Yes & MAT-positive (total)",
             "Paddy_Plantation=Yes & MAT-positive (flagged influential)",
             "Paddy_Plantation % of positives flagged",
             "Hospitalized=Yes & MAT-positive (total)",
             "Hospitalized=Yes & MAT-positive (flagged influential)",
             "Hospitalized % of positives flagged"),
  Value = c(paddy_yes_positive_total,
            paddy_yes_positive_flagged,
            round(100*paddy_yes_positive_flagged/paddy_yes_positive_total, 1),
            hosp_yes_positive_total,
            hosp_yes_positive_flagged,
            round(100*hosp_yes_positive_flagged/max(hosp_yes_positive_total,1), 1))
)

# Print everything to console too
print(paddy_full); print(hosp_full)
print(paddy_flagged); print(hosp_flagged)
print(summary_table)

# Export all tables into one multi-sheet Excel file
write_xlsx(
  list(
    "Paddy_FullData" = paddy_full,
    "Hospitalized_FullData" = hosp_full,
    "Paddy_FlaggedObs" = paddy_flagged,
    "Hospitalized_FlaggedObs" = hosp_flagged,
    "Summary" = summary_table
  ),
  out("Sparse_Cell_Diagnostic_Check.xlsx")
)

# Checking cell counts before vs after exclusion (Paddy_Plantation + all Model 3 vars)

library(dplyr)
library(writexl)

# Step 1: Paddy_Plantation before vs after exclusion
cat("=== Paddy_Plantation x Outcome: BEFORE exclusion (original Model 3 data) ===\n")
before_paddy <- table(model_data_m3$Paddy_Plantation, model_data_m3$MAT_bin)
print(before_paddy)

cat("\n=== Paddy_Plantation x Outcome: AFTER exclusion (sensitivity dataset) ===\n")
after_paddy <- table(model_data_m3_sens$Paddy_Plantation, model_data_m3_sens$MAT_bin)
print(after_paddy)

# Step 2: Do the same scan for EVERY variable in Model 3, to catch any other
# cell that became dangerously sparse after exclusion (not just Paddy_Plantation)
model3_vars <- c("Sex", "Settlement", "Level_of_Education", "Contact_stagnantwater",
                 "Paddy_Plantation", "Drink_unboiled", "Fever_12months", "Hospitalized_12months")

cat("\n=== Full scan: cell counts BEFORE vs AFTER exclusion, all Model 3 variables ===\n")

cell_check <- lapply(model3_vars, function(v) {
  before_tab <- table(model_data_m3[[v]], model_data_m3$MAT_bin)
  after_tab  <- table(model_data_m3_sens[[v]], model_data_m3_sens$MAT_bin)
  
  # Combine into a readable data frame: rows = categories, columns = before/after counts
  categories <- rownames(before_tab)
  data.frame(
    Variable = v,
    Category = categories,
    Before_Neg = before_tab[, "0"],
    Before_Pos = before_tab[, "1"],
    After_Neg = ifelse(categories %in% rownames(after_tab), after_tab[categories, "0"], 0),
    After_Pos = ifelse(categories %in% rownames(after_tab), after_tab[categories, "1"], 0),
    row.names = NULL
  )
})

cell_check_df <- bind_rows(cell_check)

# Flag any cell where the positive count dropped to 0 or very low (<3) after exclusion
cell_check_df <- cell_check_df %>%
  mutate(
    Pos_dropped_to_zero = After_Pos == 0 & Before_Pos > 0,
    Pos_now_very_sparse = After_Pos > 0 & After_Pos < 3
  )

print(cell_check_df)

cat("\n=== Cells where positive count dropped to ZERO after exclusion: ===\n")
print(cell_check_df %>% filter(Pos_dropped_to_zero))

cat("\n=== Cells now very sparse (1-2 positives remaining): ===\n")
print(cell_check_df %>% filter(Pos_now_very_sparse))

# Export full comparison table
write_xlsx(cell_check_df, out("Cell_Counts_Before_After_Exclusion.xlsx"))
