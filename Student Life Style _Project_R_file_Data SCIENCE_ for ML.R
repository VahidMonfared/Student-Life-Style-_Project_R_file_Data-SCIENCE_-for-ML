## Vahid Monfared ##
# data source: https://www.kaggle.com/datasets/steve1215rogg/student-lifestyle-dataset?resource=download #
# title: student lifestyle dataset, Daily Lifestyle and Academic Performance of Students #


# 1- Considering stress_level, with stress_level
## Step 0 ##
## Exploratory Data Analysis (EDA) ##
##  Cleaning up the data and randomly sample 1000 observations from it if your data set is large ##
# Set working directory (adjust as necessary)
setwd("C:/Users/vahid/OneDrive/Documents/CS_555/Final_Project")
# Load the necessary library
library(readxl)
# Load the dataset
data <- read.csv("CS_555_Final_Project_dataset.xlsx.csv")
# Display the original shape of the dataset
cat("Original shape of the dataset:", nrow(data), "rows and", ncol(data), "columns\n")
# Clean the dataset (example: remove rows with missing values)
cleaned_data <- na.omit(data)
# Randomly sample 1000 observations
set.seed(123) # For reproducibility
sampled_data <- cleaned_data[sample(1:nrow(cleaned_data), 1000), ]
# Display the shape of the sampled dataset
cat("Shape of the sampled dataset:", nrow(sampled_data), "rows and", ncol(sampled_data), "columns\n")
# Display the first few rows of the sampled dataset as a dataframe
head(sampled_data)
data <- cleaned_data[sample(1:nrow(cleaned_data), 1000), ]
dim(data)
colnames(data)
data <- data[, !names(data) %in% "Student_ID"]
colnames(data)
dim(data)


# Load necessary libraries
library(ggplot2)
library(reshape2)
# Convert Stress_Level to numeric ordinal values if not already done
data$Stress_Level <- as.numeric(factor(data$Stress_Level, levels = c("Low", "Moderate", "High"))) - 1
# List of variables including Stress_Level
variables <- c("Study_Hours_Per_Day", "Extracurricular_Hours_Per_Day", 
               "Sleep_Hours_Per_Day", "Social_Hours_Per_Day", 
               "Physical_Activity_Hours_Per_Day", "GPA", "Stress_Level")
# Calculate the correlation matrix
correlation_matrix <- cor(data[variables], use = "complete.obs")
# Extract correlations with Stress_Level
stress_correlations <- correlation_matrix["Stress_Level", -ncol(correlation_matrix)] # Exclude self-correlation
# Convert to data frame for plotting
correlation_df <- data.frame(
  Variable = names(stress_correlations),
  Correlation = as.numeric(stress_correlations)
)
# Plot the bar chart
ggplot(correlation_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "gray") +
  coord_flip() +  # Flip for horizontal bars
  theme_minimal() +
  ggtitle("Correlation of Variables with Stress_Level") +
  xlab("Variables") +
  ylab("Correlation Coefficient")



# Ensure Stress_Level is converted to numeric ordinal values
data$Stress_Level <- as.numeric(factor(data$Stress_Level, levels = c("Low", "Moderate", "High"))) - 1
# List of variables to include in the correlation calculation
variables <- c("Study_Hours_Per_Day", "Extracurricular_Hours_Per_Day", 
               "Sleep_Hours_Per_Day", "Social_Hours_Per_Day", 
               "Physical_Activity_Hours_Per_Day", "GPA", "Stress_Level")
# Subset the data for the selected variables
correlation_data <- data[variables]
# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")
# Print the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)




# Load necessary library
library(ggplot2)
# Assuming 'data' contains the Stress_Level variable
# Convert Stress_Level to a factor (if not already done)
data$Stress_Level <- factor(data$Stress_Level, levels = c("Low", "Moderate", "High"))
# Create a bar plot for the frequency of Stress_Level
ggplot(data, aes(x = Stress_Level, fill = Stress_Level)) +
  geom_bar() +
  theme_minimal() +
  ggtitle("Frequency of Stress Levels") +
  xlab("Stress Level") +
  ylab("Frequency") +
  theme(legend.position = "none")



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
# Convert Stress_Level to a factor variable
data$Stress_Level <- factor(data$Stress_Level, levels = c("Low", "Moderate", "High"))
# List of continuous variables
variables <- c("Study_Hours_Per_Day", "Extracurricular_Hours_Per_Day", 
               "Sleep_Hours_Per_Day", "Social_Hours_Per_Day", 
               "Physical_Activity_Hours_Per_Day", "GPA")
# Perform ANOVA and Tukey HSD for each variable
anova_results <- list()
tukey_results <- list()
for (var in variables) {
  # ANOVA
  formula <- as.formula(paste(var, "~ Stress_Level"))
  anova_model <- aov(formula, data = data)
  anova_results[[var]] <- summary(anova_model)
  print(paste("ANOVA Results for", var))
  print(anova_results[[var]])
  
  # Tukey HSD Test
  tukey_results[[var]] <- TukeyHSD(anova_model)
  print(paste("Tukey HSD Test Results for", var))
  print(tukey_results[[var]])
  
  # Boxplot for Visualization
  plot <- ggplot(data, aes(x = Stress_Level, y = .data[[var]], fill = Stress_Level)) +
    geom_boxplot() +
    theme_minimal() +
    ggtitle(paste("Boxplot of", var, "by Stress Level")) +
    xlab("Stress Level") +
    ylab(var) +
    theme(legend.position = "none")
  
  # Render the plot and save it
  print(plot)  # Display the plot in the R console
  ggsave(filename = paste0(var, "_boxplot.png"), plot = plot, width = 7, height = 7)
}



# Load necessary libraries
library(ggplot2)
library(reshape2)
library(ggcorrplot)
# Convert Stress_Level to ordinal numeric values
data$Stress_Level <- as.numeric(factor(data$Stress_Level, levels = c("Low", "Moderate", "High"))) - 1
# Ensure all variables are numeric
# List of variables for correlation (including Stress_Level now as numeric)
variables <- c("Study_Hours_Per_Day", "Extracurricular_Hours_Per_Day", 
               "Sleep_Hours_Per_Day", "Social_Hours_Per_Day", 
               "Physical_Activity_Hours_Per_Day", "GPA", "Stress_Level")
# Subset data for selected variables
correlation_data <- data[variables]
# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")
# Plot the heatmap of correlations
ggcorrplot(correlation_matrix, method = "circle", type = "lower", 
           title = "Heatmap of Correlation for All Variables",
           lab = TRUE, lab_size = 3, 
           colors = c("red", "white", "blue"))


# Load necessary libraries
library(ggplot2)
library(reshape2)
library(ggcorrplot)
# Convert Stress_Level to ordinal numeric values
data$Stress_Level <- as.numeric(factor(data$Stress_Level, levels = c("Low", "Moderate", "High"))) - 1
# Ensure all variables are numeric
# List of variables for correlation (including Stress_Level now as numeric)
variables <- c("Study_Hours_Per_Day", "Extracurricular_Hours_Per_Day", 
               "Sleep_Hours_Per_Day", "Social_Hours_Per_Day", 
               "Physical_Activity_Hours_Per_Day", "GPA", "Stress_Level")
# Subset data for selected variables
correlation_data <- data[variables]
# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")
# Print the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)
# Plot the heatmap of correlations
ggcorrplot(correlation_matrix, method = "circle", type = "lower", 
           title = "Heatmap of Correlation for All Variables",
           lab = TRUE, lab_size = 3, 
           colors = c("red", "white", "blue"))



# Without Stress_Level
##visualization##
# Load necessary libraries
library(ggplot2)
library(dplyr)
# General summary of the dataset
print("General Summary:")
print(summary(data))
# Additional summary statistics for numeric variables
summary_stats <- data %>%
  select(where(is.numeric)) %>% # Select only numeric columns
  summarise_all(list(
    Mean = ~mean(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    Std_Dev = ~sd(., na.rm = TRUE),
    Variance = ~var(., na.rm = TRUE)
  ))
print("Summary Statistics for Numeric Variables:")
print(summary_stats)
# Mode function
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}

# Compute mode for each variable
mode_stats <- sapply(data, function(column) {
  if (is.numeric(column) || is.factor(column) || is.character(column)) {
    return(get_mode(column))
  } else {
    return(NA)
  }
})
print("Mode for each variable:")
print(mode_stats)
# Variable types
print("Structure of the dataset:")
str(data)
# Barplot for categorical variable (Stress_Level)
if ("Stress_Level" %in% colnames(data)) {
  bar_plot <- ggplot(data, aes(x = Stress_Level)) +
    geom_bar(fill = "skyblue") +
    labs(title = "Frequency of Stress Levels", x = "Stress Level", y = "Count") +
    theme_minimal()
  print(bar_plot)
}
# Boxplots for continuous variables
continuous_vars <- names(data)[sapply(data, is.numeric)]
for (var in continuous_vars) {
  box_plot <- ggplot(data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "lightblue", outlier.colour = "red") +
    labs(title = paste("Boxplot of", var), y = var) +
    theme_minimal()
  
  print(box_plot)
}

# Pairwise correlations for numerical variables
cor_matrix <- cor(data[sapply(data, is.numeric)], use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)
# Visualization: Heatmap of the correlation matrix
if (ncol(cor_matrix) > 1) {
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    install.packages("corrplot")  # Ensure corrplot is installed
  }
  library(corrplot)
  corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
}


# Pairwise correlations for numerical variables
cor_matrix <- cor(data[sapply(data, is.numeric)], use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)
# Visualization: Heatmap of the correlation matrix with numbers
if (ncol(cor_matrix) > 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")  # Ensure ggplot2 is installed
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    install.packages("reshape2")  # Ensure reshape2 is installed
  }
  library(ggplot2)
  library(reshape2)
  # Convert the correlation matrix to a format suitable for ggplot2
  cor_data <- melt(cor_matrix)
  # Create the heatmap with numbers
  ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +  # Heatmap tiles
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
    geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +  # Add correlation values
    labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}


##General heat map: Stress_Level encoded as Low=0, Moderate=1, High=2##
# Convert "Stress_Level" to numeric (Low=0, Moderate=1, High=2)
data$Stress_Level <- factor(data$Stress_Level, levels = c("Low", "Moderate", "High"), labels = c(0, 1, 2))
data$Stress_Level <- as.numeric(as.character(data$Stress_Level))
# Calculate correlations including the ordinal categorical variable
cor_matrix <- cor(data, use = "complete.obs")
# Print the correlation matrix
print("Correlation Matrix:")
print(cor_matrix)
# Visualization: Heatmap of the correlation matrix with numbers
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(reshape2)
library(ggplot2)
# Melt the correlation matrix for ggplot2
cor_data <- melt(cor_matrix)
# Create the heatmap
ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Heatmap tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +  # Add correlation values
  labs(title = "Correlation Heatmap (Including Ordinal Categorical Variable)", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


##Box Plot##
# Load necessary libraries
library(ggplot2)
library(dplyr)
# Identify scalar (numeric) variables (excluding "Stress_Level")
scalar_vars <- setdiff(names(data)[sapply(data, is.numeric)], "Stress_Level")
# Function to calculate and print outliers beyond 3 standard deviations
detect_outliers <- function(var_name, data) {
  values <- data[[var_name]]
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  outliers <- values[abs(values - mean_val) > 3 * sd_val]
  
  if (length(outliers) > 0) {
    print(paste("Outliers for", var_name, ":", toString(outliers)))
  } else {
    print(paste("No outliers for", var_name))
  }
  
  return(outliers)
}

# Loop through each scalar variable to create individual plots
for (var in scalar_vars) {
  # Detect outliers and print them
  detect_outliers(var, data)
  
  # Create a boxplot for the current variable
  p <- ggplot(data, aes(x = "", y = .data[[var]])) +  # Use "" as a placeholder for x
    geom_boxplot(outlier.colour = "red", outlier.size = 2) +
    labs(title = paste("Boxplot for", var), x = "Variable", y = var) +
    theme_minimal()
# Display the plot
  print(p)
  
  # Pause to allow screenshot before moving to the next plot
  readline(prompt = "Press [Enter] to continue to the next plot...")
}


# Calculate the mode for Stress_Level
stress_mode <- as.character(names(sort(table(data$Stress_Level), decreasing = TRUE)[1]))
# Print the result
cat("The mode for Stress_Level is:", stress_mode, "\n")
##original data##
setwd("C:/Users/vahid/OneDrive/Documents/CS_555/Final_Project")
# Load the necessary library
library(readxl)
# Load the dataset
data <- read.csv("CS_555_Final_Project_dataset.xlsx.csv")
head(data)

##removing student_ID and stress_level for second round of my project##
# Set working directory (adjust as necessary)
setwd("C:/Users/vahid/OneDrive/Documents/CS_555/Final_Project")
# Load the necessary library
library(readxl)
# Load the dataset
data <- read.csv("CS_555_Final_Project_dataset.xlsx.csv")
# Remove Stress_Level and Student_ID columns from the dataset
data <- data %>% select(-Stress_Level, -Student_ID)
# Randomly sample 1000 rows from the dataset
set.seed(123)  # Set a seed for reproducibility
data <- data %>% sample_n(1000)
# Display the updated dataset structure
str(data)
# Display a summary of the updated dataset
summary(data)
head(data)
colnames(data)
dim(data)


##1. Describe your research scenario and question(s).##
# Load necessary libraries
library(ggplot2)
# Assuming your data is stored in a data frame named "data"
# 1. Perform Pearson's correlation analysis
correlation_matrix <- cor(data, method = "pearson")
# Display the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)
# Isolate GPA correlations for clarity
gpa_correlations <- correlation_matrix[,"GPA"]
print("Correlations with GPA:")
print(gpa_correlations)
# 2. Visualize correlations with GPA using a bar plot
gpa_cor_df <- data.frame(Variable = names(gpa_correlations),
                         Correlation = gpa_correlations)
ggplot(data = gpa_cor_df, aes(x = reorder(Variable, -Correlation), y = Correlation)) +
  geom_bar(stat = "identity") +
  labs(title = "Correlation of Lifestyle Variables with GPA",
       x = "Lifestyle Variables",
       y = "Correlation Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels
# 3. Testing significance of correlations
correlation_tests <- lapply(names(data)[-which(names(data) == "GPA")], function(var) {
  cor.test(data[[var]], data$GPA, method = "pearson")
})
# Extract p-values
p_values <- sapply(correlation_tests, function(test) test$p.value)
# Combine variables and p-values into a data frame
correlation_significance <- data.frame(
  Variable = names(data)[-which(names(data) == "GPA")],
  P_Value = p_values
)
# Print significance of correlations
print("Significance of Correlations (P-Values):")
print(correlation_significance)
# 4. Identify significant correlations
significant_vars <- correlation_significance[correlation_significance$P_Value < 0.05, ]
# Print significant variables
print("Significant Variables with GPA:")
print(significant_vars)


# Fit a multiple linear regression model
model <- lm(GPA ~ Study_Hours_Per_Day + Sleep_Hours_Per_Day + Physical_Activity_Hours_Per_Day, data = data)
# Display the summary of the regression model
summary_model <- summary(model)
print(summary_model)
# Extract R-squared and Adjusted R-squared
r_squared <- summary_model$r.squared
adj_r_squared <- summary_model$adj.r.squared
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")
# Check for significant predictors
significant_predictors <- summary_model$coefficients[, "Pr(>|t|)"] < 0.05
cat("Significant Predictors:\n")
print(rownames(summary_model$coefficients)[significant_predictors])


# Create a categorical variable for study hours based on median split
data$Study_Hours_Level <- ifelse(data$Study_Hours_Per_Day > median(data$Study_Hours_Per_Day), 
                                 "High", "Low")
# Check the distribution of students in each group
table(data$Study_Hours_Level)
# Perform one-way ANOVA
anova_result <- aov(GPA ~ Study_Hours_Level, data = data)
# Display the ANOVA summary
summary(anova_result)
# Calculate group means for GPA
group_means <- aggregate(GPA ~ Study_Hours_Level, data = data, mean)
print("Group Means:")
print(group_means)
# Post-hoc test if ANOVA is significant (Tukey's HSD)
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print("Tukey Post-Hoc Test Results:")
  print(tukey_result)
} else {
  print("ANOVA is not significant. No need for post-hoc testing.")
}



##removing student_ID and stress_level for second round of my project##
# Set working directory (adjust as necessary)
setwd("C:/Users/vahid/OneDrive/Documents/CS_555/Final_Project")
# Load the necessary library
library(readxl)
# Load the dataset
data <- read.csv("CS_555_Final_Project_dataset.xlsx.csv")
# Remove Stress_Level and Student_ID columns from the dataset
data <- data %>% select(-Stress_Level, -Student_ID)
# Randomly sample 1000 rows from the dataset
set.seed(123)  # Set a seed for reproducibility
data <- data %>% sample_n(1000)
# Display the updated dataset structure
str(data)
# Display a summary of the updated dataset
summary(data)
head(data)
colnames(data)
dim(data)


# Load necessary libraries
library(ggplot2)
# Sort the dataset by Study_Hours_Per_Day in ascending order
data <- data[order(data$Study_Hours_Per_Day), ]
# Create three equal groups for Study_Hours_Per_Day: Low, Moderate, High
n <- nrow(data)
data$Study_Hours_Group <- cut(1:n, 
                              breaks = c(0, floor(n / 3), floor(2 * n / 3), n),
                              labels = c("Low", "Moderate", "High"),
                              include.lowest = TRUE)
# Check the distribution of groups
table(data$Study_Hours_Group)
# Calculate the average Study_Hours_Per_Day in each group
average_hours <- aggregate(Study_Hours_Per_Day ~ Study_Hours_Group, data = data, mean)
# Print the average hours for each group
print("Average Study Hours in Each Group:")
print(average_hours)
# Perform one-way ANOVA
anova_result <- aov(GPA ~ Study_Hours_Group, data = data)
anova_summary <- summary(anova_result)
print("ANOVA Summary:")
print(anova_summary)
# Perform pairwise comparisons using Tukey's HSD
tukey_result <- TukeyHSD(anova_result)
print("Pairwise Comparisons (Tukey's HSD):")
print(tukey_result)
# Plot boxplots for GPA by Study_Hours_Group
ggplot(data, aes(x = Study_Hours_Group, y = GPA, fill = Study_Hours_Group)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Comparison of GPA Among Study Hour Groups",
       x = "Study Hour Groups",
       y = "GPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  facet_wrap(~ Study_Hours_Group, nrow = 1)


##removing student_ID and stress_level for second round of my project##
# Set working directory (adjust as necessary)
setwd("C:/Users/vahid/OneDrive/Documents/CS_555/Final_Project")
# Load the necessary library
library(readxl)
# Load the dataset
data <- read.csv("CS_555_Final_Project_dataset.xlsx.csv")
# Remove Stress_Level and Student_ID columns from the dataset
data <- data %>% select(-Stress_Level, -Student_ID)
# Randomly sample 1000 rows from the dataset
set.seed(123)  # Set a seed for reproducibility
data <- data %>% sample_n(1000)
# Display the updated dataset structure
str(data)
# Display a summary of the updated dataset
summary(data)
head(data)
colnames(data)
dim(data)


# Sort the dataset by Social_Hours_Per_Day in ascending order
data <- data[order(data$Social_Hours_Per_Day), ]
# Create three equal groups for Social_Hours_Per_Day: Low, Moderate, High
n <- nrow(data)
data$Social_Hours_Group <- cut(1:n, 
                               breaks = c(0, floor(n / 3), floor(2 * n / 3), n),
                               labels = c("Low", "Moderate", "High"),
                               include.lowest = TRUE)
# Check the distribution of groups
table(data$Social_Hours_Group)
# Calculate the average Social_Hours_Per_Day in each group
average_hours_social <- aggregate(Social_Hours_Per_Day ~ Social_Hours_Group, data = data, mean)
print("Average Social Hours in Each Group:")
print(average_hours_social)
# Perform one-way ANOVA
anova_social_result <- aov(GPA ~ Social_Hours_Group, data = data)
anova_social_summary <- summary(anova_social_result)
print("ANOVA Summary for Social Hours Groups:")
print(anova_social_summary)
# Perform pairwise comparisons using Tukey's HSD
tukey_social_result <- TukeyHSD(anova_social_result)
print("Pairwise Comparisons (Tukey's HSD) for Social Hours Groups:")
print(tukey_social_result)
# Plot boxplots for GPA by Social_Hours_Group
ggplot(data, aes(x = Social_Hours_Group, y = GPA, fill = Social_Hours_Group)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Comparison of GPA Among Social Hour Groups",
       x = "Social Hour Groups",
       y = "GPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  facet_wrap(~ Social_Hours_Group, nrow = 1)


##removing student_ID and stress_level for second round of my project##
# Set working directory (adjust as necessary)
setwd("C:/Users/vahid/OneDrive/Documents/CS_555/Final_Project")
# Load the necessary library
library(readxl)
# Load the dataset
data <- read.csv("CS_555_Final_Project_dataset.xlsx.csv")
# Remove Stress_Level and Student_ID columns from the dataset
data <- data %>% select(-Stress_Level, -Student_ID)
# Randomly sample 1000 rows from the dataset
set.seed(123)  # Set a seed for reproducibility
data <- data %>% sample_n(1000)
# Display the updated dataset structure
str(data)
# Display a summary of the updated dataset
summary(data)
head(data)
colnames(data)
dim(data)


# Load necessary libraries
library(ggplot2)
# Perform ANCOVA with interaction between Study_Hours_Per_Day and Physical_Activity_Hours_Per_Day
ancova_result <- aov(GPA ~ Study_Hours_Per_Day * Physical_Activity_Hours_Per_Day, data = data)
# Display ANCOVA summary
summary_ancova <- summary(ancova_result)
print("ANCOVA Summary:")
print(summary_ancova)
# Check interaction effect visualization
ggplot(data, aes(x = Study_Hours_Per_Day, y = GPA, color = Physical_Activity_Hours_Per_Day)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", aes(group = Physical_Activity_Hours_Per_Day), se = FALSE) +
  labs(title = "Interaction Effect of Study Hours and Physical Activity on GPA",
       x = "Study Hours Per Day",
       y = "GPA") +
  theme_minimal() +
  theme(legend.position = "right")


##removing student_ID and stress_level for second round of my project##
# Set working directory (adjust as necessary)
setwd("C:/Users/vahid/OneDrive/Documents/CS_555/Final_Project")
# Load the necessary library
library(readxl)
# Load the dataset
data <- read.csv("CS_555_Final_Project_dataset.xlsx.csv")
# Remove Stress_Level and Student_ID columns from the dataset
data <- data %>% select(-Stress_Level, -Student_ID)
# Randomly sample 1000 rows from the dataset
set.seed(123)  # Set a seed for reproducibility
data <- data %>% sample_n(1000)
# Display the updated dataset structure
str(data)
# Display a summary of the updated dataset
summary(data)
head(data)
colnames(data)
dim(data)


# Load necessary libraries
library(ggplot2)
library(dplyr)
# Step 1: Data Cleaning
# Check for missing values
print("Checking for missing values:")
missing_data <- colSums(is.na(data))
print(missing_data)
# Remove rows with missing values
data <- na.omit(data)
# Outlier detection using z-scores (values > 3 standard deviations)
z_scores <- as.data.frame(scale(data))  # Standardizing variables
outliers <- apply(z_scores, 2, function(x) abs(x) > 3)
data <- data[!rowSums(outliers), ]
# Step 2: Standardization
data_scaled <- as.data.frame(scale(data))
# Step 3: Descriptive Statistics
print("Descriptive Statistics:")
summary_stats <- data %>% summarise(across(everything(), list(
  mean = ~ mean(.),
  median = ~ median(.),
  sd = ~ sd(.)
)))
print(summary_stats)
# Visualizations
# Histograms for each variable
par(mfrow = c(3, 2))
for (var in names(data)) {
  hist(data[[var]], main = paste("Histogram of", var), xlab = var, col = "skyblue", border = "white")
}
# Boxplots for each variable
par(mfrow = c(3, 2))
for (var in names(data)) {
  boxplot(data[[var]], main = paste("Boxplot of", var), ylab = var, col = "lightgreen")
}
# Step 4: ANCOVA Analysis
# Perform ANCOVA with interaction between Study_Hours_Per_Day and Physical_Activity_Hours_Per_Day
ancova_result <- aov(GPA ~ Study_Hours_Per_Day * Physical_Activity_Hours_Per_Day, data = data)
# Display ANCOVA summary
summary_ancova <- summary(ancova_result)
print("ANCOVA Summary:")
print(summary_ancova)
# Step 5: Visualization of Interaction Effect
ggplot(data, aes(x = Study_Hours_Per_Day, y = GPA, color = Physical_Activity_Hours_Per_Day)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", aes(group = Physical_Activity_Hours_Per_Day), se = FALSE) +
  labs(title = "Interaction Effect of Study Hours and Physical Activity on GPA",
       x = "Study Hours Per Day",
       y = "GPA") +
  theme_minimal() +
  theme(legend.position = "right")


##removing student_ID and stress_level for second round of my project##
# Set working directory (adjust as necessary)
setwd("C:/Users/vahid/OneDrive/Documents/CS_555/Final_Project")
# Load the necessary library
library(readxl)
# Load the dataset
data <- read.csv("CS_555_Final_Project_dataset.xlsx.csv")
# Remove Stress_Level and Student_ID columns from the dataset
data <- data %>% select(-Stress_Level, -Student_ID)
# Randomly sample 1000 rows from the dataset
set.seed(123)  # Set a seed for reproducibility
data <- data %>% sample_n(1000)
# Display the updated dataset structure
str(data)
# Display a summary of the updated dataset
summary(data)
head(data)
colnames(data)
dim(data)


# Step 1: Compute Pearson Correlation Matrix
correlation_matrix <- cor(data, method = "pearson")
# Step 2: Display Correlation Matrix
print("Correlation Matrix:")
print(correlation_matrix)
# Step 3: Test for Significance of Correlations
correlation_tests <- lapply(names(data)[-which(names(data) == "GPA")], function(var) {
  cor.test(data[[var]], data$GPA, method = "pearson")
})
# Extract p-values
p_values <- sapply(correlation_tests, function(test) test$p.value)
# Combine variables and p-values
correlation_significance <- data.frame(
  Variable = names(data)[-which(names(data) == "GPA")],
  P_Value = p_values
)
print("Significance of Correlations (P-Values):")
print(correlation_significance)
# Step 4: Visualize Correlation Matrix
library(ggplot2)
library(reshape2)
# Convert matrix to long format for heatmap
cor_data_long <- melt(correlation_matrix)
ggplot(cor_data_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Heatmap of Pearson Correlation Coefficients",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Fit the Multiple Linear Regression Model
model <- lm(GPA ~ Study_Hours_Per_Day + Extracurricular_Hours_Per_Day + 
              Sleep_Hours_Per_Day + Social_Hours_Per_Day + Physical_Activity_Hours_Per_Day, 
            data = data)
# Display the summary of the regression model
summary_model <- summary(model)
print("Summary of Multiple Linear Regression Model:")
print(summary_model)
# Extract R-squared and Adjusted R-squared
r_squared <- summary_model$r.squared
adj_r_squared <- summary_model$adj.r.squared
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")
# Identify significant predictors
significant_predictors <- summary_model$coefficients[,"Pr(>|t|)"] < 0.05
cat("Significant Predictors:\n")
print(rownames(summary_model$coefficients)[significant_predictors])
# Visualize Residuals
par(mfrow = c(2, 2))
plot(model)
# Optional: Standardized Coefficients (to compare effect sizes)
library(QuantPsyc)
std_coeff <- lm.beta(model)
cat("Standardized Coefficients:\n")
print(std_coeff)


# Step 1: Group Division
# Divide Study_Hours_Per_Day into quartiles
data$Study_Hours_Group <- cut(data$Study_Hours_Per_Day, 
                              breaks = quantile(data$Study_Hours_Per_Day, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                              labels = c("Low", "Lower-Middle", "Upper-Middle", "High"),
                              include.lowest = TRUE)
# Check the distribution of groups
print("Group Distribution:")
print(table(data$Study_Hours_Group))
# Step 2: Perform One-Way ANOVA
anova_result <- aov(GPA ~ Study_Hours_Group, data = data)
# Display ANOVA Summary
anova_summary <- summary(anova_result)
print("ANOVA Summary:")
print(anova_summary)
# Step 3: Perform Post-Hoc Test (Tukey's HSD)
tukey_result <- TukeyHSD(anova_result)
print("Post-Hoc Test Results (Tukey's HSD):")
print(tukey_result)
# Step 4: Visualize Group Differences
library(ggplot2)
ggplot(data, aes(x = Study_Hours_Group, y = GPA, fill = Study_Hours_Group)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Comparison of GPA Across Study Hour Groups",
       x = "Study Hour Groups",
       y = "GPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


# Fit the ANCOVA model with interaction term
ancova_model <- aov(GPA ~ Study_Hours_Per_Day * Physical_Activity_Hours_Per_Day +
                      Extracurricular_Hours_Per_Day +
                      Sleep_Hours_Per_Day +
                      Social_Hours_Per_Day, data = data)
# Display the summary of the ANCOVA model
summary_ancova <- summary(ancova_model)
print("ANCOVA Summary:")
print(summary_ancova)
# Visualize the interaction effect
library(ggplot2)
ggplot(data, aes(x = Study_Hours_Per_Day, y = GPA, color = Physical_Activity_Hours_Per_Day)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", aes(group = Physical_Activity_Hours_Per_Day), se = FALSE) +
  labs(title = "Interaction Effect of Study Hours and Physical Activity on GPA",
       x = "Study Hours Per Day",
       y = "GPA") +
  theme_minimal() +
  theme(legend.position = "right")


library(ggplot2)
ggplot(data, aes(x = Study_Hours_Group, y = GPA, fill = Study_Hours_Group)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Boxplot: GPA Distribution Across Study Hours Groups",
       x = "Study Hours Groups",
       y = "GPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


library(reshape2)
# Compute Pearson correlation matrix
correlation_matrix <- cor(data, method = "pearson")
# Convert correlation matrix to long format
cor_data_long <- melt(correlation_matrix)
# Plot heatmap
ggplot(cor_data_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Heatmap of Pearson Correlation Coefficients",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Example: Scatterplot for GPA vs. Study_Hours_Per_Day
ggplot(data, aes(x = Study_Hours_Per_Day, y = GPA)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Scatterplot: GPA vs. Study Hours Per Day",
       x = "Study Hours Per Day",
       y = "GPA") +
  theme_minimal()

